//! The cache ledger: which sessions use which entries, and what each entry
//! is worth.
//!
//! The shared [`cache`](crate::cache) publishes entries atomically and lets
//! builds use them in place without a lock. That is enough while nothing is
//! ever removed. Removing entries (the next step: docs/design/cache-ledger.md)
//! needs two facts no directory listing gives: whether a running build still
//! uses an entry, and when an entry was last wanted. The ledger records both,
//! transactionally, in a TurboKV database at `<root>/ledger`:
//!
//! | key | value |
//! |---|---|
//! | `entry/<kind>/<key>` | an [`Entry`]: state, owning session, size, last use |
//! | `pin/<session>/<kind>/<key>` | this session wants this entry |
//! | `session/<id>` | a running (or crashed) rene |
//! | `total` | the summed size of the live entries |
//! | `schema` | the ledger's schema version |
//!
//! # Sessions and liveness
//!
//! Every rene run is a [`Session`]. It holds an exclusive OS lock on
//! `<root>/sessions/<id>.lock` for its whole life, and that lock is its
//! only liveness signal: the operating system releases it when the process
//! exits, crashed or not. Any other session can tell a dead session from a
//! live one with one non-blocking lock attempt. Nothing is ever inferred
//! from pids or timestamps.
//!
//! # Transactions
//!
//! TurboKV holds an exclusive directory lock while a database is open, so
//! the ledger is opened for one operation at a time: open, read, write one
//! atomic batch, close. Nobody keeps it open across a build; a session that
//! finds it held waits a few milliseconds and retries. The exclusive open
//! makes every operation serialisable without further machinery, and lets
//! an operation consult the file system (is the entry directory there?)
//! knowing nobody changes it underneath.
//!
//! Every operation starts by **reaping** ([`View::reap`]): for each session
//! row whose lock can be taken, the session is dead; its pins go, and each
//! row it owned is repaired from two facts, whether the entry's build lock is
//! free and whether its directory is still there. The session row goes only
//! once no entry row names it, so a later reaper can still prove it dead.
//!
//! # Protocol
//!
//! A session that wants an entry calls [`Session::lookup`], which pins the
//! key and stamps the access time whether or not the entry exists yet. A
//! live entry is used in place. One under prune whose directory is still
//! there is reclaimed. Otherwise the caller takes the entry's build lock
//! ([`Cache::lock`]) and calls [`Session::recheck`], since whoever held the
//! lock may have published meanwhile; if not, the row becomes `building`,
//! owned by this session, until [`Session::register`] marks it live with its
//! size. Pins are dropped by [`Session::unpin`] or when the session ends.
//!
//! The protocol, including the pruning side, is model-checked:
//! docs/design/cache-prune/CachePrune.tla.
//!
//! # Degradation
//!
//! A ledger that cannot be opened, or that a newer rene wrote in a schema
//! this one does not know, disables itself with one warning. Entries are
//! then trusted from their on-disk markers alone, exactly as before the
//! ledger existed, and such a session never removes anything.

use std::cell::Cell;
use std::collections::{BTreeMap, BTreeSet};
use std::fs::{File, OpenOptions, TryLockError};
use std::path::{Path, PathBuf};
use std::time::{Duration, SystemTime, UNIX_EPOCH};

use serde::{Deserialize, Serialize};
use tokio::runtime::Runtime;
use turbokv::{Db, DbError, DbOptions, WriteBatch};

use crate::cache::Cache;

/// The ledger database's directory under the cache root.
pub const LEDGER_DIR: &str = "ledger";
/// The session lock files' directory under the cache root.
pub const SESSIONS_DIR: &str = "sessions";

const SCHEMA_KEY: &str = "schema";
/// Bumped only for changes an older rene could misread. Additions to the
/// JSON values are not such changes: unknown fields are ignored.
const SCHEMA: u64 = 1;
const ENTRY: &str = "entry/";
const PIN: &str = "pin/";
const SESSION: &str = "session/";
const TOTAL: &str = "total";

/// How often a session retries a ledger another session has open.
const OPEN_POLL: Duration = Duration::from_millis(10);

/// An entry row's state.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum State {
    /// Being built by `owner`, which holds the entry's build lock.
    Building,
    /// Complete on disk and usable in place.
    Live,
    /// Selected for removal by `owner`; its directory may or may not still
    /// be there.
    Pruning,
}

/// An entry row.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Entry {
    pub state: State,
    /// The session owning a transient state; `None` for a live entry.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub owner: Option<String>,
    /// Bytes on disk, measured when the entry was registered.
    pub size: u64,
    /// Unix seconds of the last lookup.
    pub last_used: u64,
}

/// A session row. Informational: liveness comes from the lock file.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SessionRow {
    pub pid: u32,
    pub started: u64,
}

/// What [`Session::lookup`] found.
#[derive(Debug, PartialEq, Eq)]
pub enum Lookup {
    /// The entry is live (or was reclaimed): verify it on disk and use it.
    Live,
    /// No usable entry: take the build lock, then [`Session::recheck`].
    Build,
    /// The entry is being removed and its directory is already gone: wait,
    /// then look it up again.
    Wait,
}

/// What [`Session::recheck`] decided, under the build lock.
#[derive(Debug, PartialEq, Eq)]
pub enum Recheck {
    /// Someone published a usable entry meanwhile: use it, release the lock.
    Live,
    /// The row is this session's `building` row: build, then
    /// [`Session::register`].
    Build,
    /// As [`Lookup::Wait`]: release the lock, wait, start over.
    Wait,
}

/// A read-only copy of the ledger, for inspection and tests.
#[derive(Debug, Default, Clone)]
pub struct Snapshot {
    /// `<kind>/<key>` to its row.
    pub entries: BTreeMap<String, Entry>,
    /// `(session, <kind>/<key>)` pairs.
    pub pins: BTreeSet<(String, String)>,
    pub sessions: BTreeMap<String, SessionRow>,
    pub total: u64,
}

/// This rene's session in the ledger. Dropping it ends the session: its
/// pins go, and so does its row if nothing names it.
pub struct Session {
    root: PathBuf,
    id: String,
    /// The session lock, held for the session's life. `None` when the file
    /// could not be created, which also disables the ledger.
    lock: Option<File>,
    runtime: Runtime,
    /// Cleared on the first failure; every later operation is a no-op that
    /// trusts the disk, as builds did before the ledger.
    enabled: Cell<bool>,
    /// Set by [`Session::abandon`] (tests): leave everything behind, as a
    /// crash would.
    abandoned: Cell<bool>,
}

impl Session {
    /// Start a session in `cache`: take the session lock and record the
    /// session row. Never fails: a ledger that cannot be used leaves the
    /// session disabled, with a warning.
    pub async fn start(cache: &Cache) -> Session {
        Self::start_as(cache, &format!("{}-{:x}", std::process::id(), now_nanos())).await
    }

    async fn start_as(cache: &Cache, id: &str) -> Session {
        let runtime = tokio::runtime::Builder::new_current_thread()
            .enable_all()
            .build()
            .expect("tokio runtime");
        let mut session = Session {
            root: cache.root().to_owned(),
            id: id.to_owned(),
            lock: None,
            runtime,
            enabled: Cell::new(true),
            abandoned: Cell::new(false),
        };
        match take_session_lock(&session.root, id) {
            Ok(lock) => session.lock = Some(lock),
            Err(e) => {
                session.disable(&e);
                return session;
            }
        }
        let row = SessionRow {
            pid: std::process::id(),
            started: now_secs(),
        };
        let id = session.id.clone();
        let result = session
            .transact(move |view| {
                view.set_session(&id, &row);
                Ok(())
            })
            .await;
        if let Err(e) = result {
            session.disable(&e);
        }
        session
    }

    pub fn id(&self) -> &str {
        &self.id
    }

    /// Whether the ledger is in use. A disabled session trusts the disk.
    pub fn enabled(&self) -> bool {
        self.enabled.get()
    }

    /// Ask for the entry `<kind>/<key>`, whose directory is `dir`: pin it,
    /// stamp its access time, and say whether it can be used, must be
    /// built, or is being removed right now.
    ///
    /// `Lookup::Live` is a promise that nobody will remove the entry while
    /// this session holds the pin, not that its contents are intact: the
    /// caller still checks the entry's own completeness marker, and goes
    /// down the build path if that fails.
    pub async fn lookup(&self, kind: &str, key: &str, dir: &Path) -> Lookup {
        let kk = kk(kind, key);
        let dir = dir.to_owned();
        let id = self.id.clone();
        self.transact(move |view| {
            view.add_pin(&id, &kk);
            Ok(match view.entries.get(&kk).cloned() {
                Some(mut entry) => match entry.state {
                    State::Live => {
                        entry.last_used = view.now;
                        view.set_entry(&kk, entry);
                        Lookup::Live
                    }
                    State::Pruning if dir.exists() => {
                        view.revive(&kk, entry);
                        Lookup::Live
                    }
                    State::Pruning => Lookup::Wait,
                    State::Building => {
                        entry.last_used = view.now;
                        view.set_entry(&kk, entry);
                        Lookup::Build
                    }
                },
                None => Lookup::Build,
            })
        })
        .await
        .unwrap_or(Lookup::Live)
    }

    /// Under the entry's build lock: decide whether to use what is there or
    /// build it. `usable` is the caller's own verdict on the directory `dir`
    /// (its completeness marker and files). `Recheck::Build` claims the row
    /// as `building` by this session; the caller clears the directory and
    /// builds, then calls [`Session::register`].
    pub async fn recheck(&self, kind: &str, key: &str, dir: &Path, usable: bool) -> Recheck {
        let kk = kk(kind, key);
        let dir = dir.to_owned();
        let id = self.id.clone();
        self.transact(move |view| {
            let entry = view.entries.get(&kk).cloned();
            Ok(match entry {
                Some(entry) if entry.state == State::Live && usable => Recheck::Live,
                Some(entry) if entry.state == State::Pruning && usable && dir.exists() => {
                    view.revive(&kk, entry);
                    Recheck::Live
                }
                Some(entry) if entry.state == State::Pruning && !dir.exists() => Recheck::Wait,
                Some(entry) => {
                    // Live but broken on disk, or an abandoned build, or a
                    // prune whose directory is unusable anyway: take over.
                    if entry.state == State::Live {
                        view.total = view.total.saturating_sub(entry.size);
                        view.set_total();
                    }
                    view.claim_building(&id, &kk, entry.last_used);
                    Recheck::Build
                }
                None => {
                    view.claim_building(&id, &kk, view.now);
                    Recheck::Build
                }
            })
        })
        .await
        .unwrap_or(if usable {
            Recheck::Live
        } else {
            Recheck::Build
        })
    }

    /// The entry is published: mark it live with its `size` on disk. Returns
    /// the ledger's new total, which the caller may compare with the size
    /// limit. The pin taken at lookup stays.
    pub async fn register(&self, kind: &str, key: &str, size: u64) -> u64 {
        let kk = kk(kind, key);
        let id = self.id.clone();
        self.transact(move |view| {
            if let Some(old) = view.entries.get(&kk)
                && old.state == State::Live
            {
                view.total = view.total.saturating_sub(old.size);
            }
            view.set_entry(
                &kk,
                Entry {
                    state: State::Live,
                    owner: None,
                    size,
                    last_used: view.now,
                },
            );
            view.total += size;
            view.set_total();
            view.add_pin(&id, &kk);
            Ok(view.total)
        })
        .await
        .unwrap_or(0)
    }

    /// Drop this session's pin on an entry it no longer needs (one it copied
    /// out of the cache, say). Entries used in place stay pinned until the
    /// session ends.
    pub async fn unpin(&self, kind: &str, key: &str) {
        let kk = kk(kind, key);
        let id = self.id.clone();
        let _ = self
            .transact(move |view| {
                view.remove_pin(&id, &kk);
                Ok(())
            })
            .await;
    }

    /// A copy of the ledger, after reaping.
    pub async fn snapshot(&self) -> Result<Snapshot, String> {
        self.transact(|view| Ok(view.snapshot())).await
    }

    /// Leave without cleaning up, as a crashed process would: the session
    /// lock is released, the rows stay for the next reaper.
    #[cfg(test)]
    fn abandon(self) {
        self.abandoned.set(true);
    }

    fn disable(&self, why: &str) {
        if self.enabled.replace(false) {
            tracing::warn!("{why}; continuing without the cache ledger");
        }
    }

    /// Run `op` against the ledger in one transaction: open (waiting for
    /// another session's operation to finish), reap, run, commit, close.
    /// A disabled session returns `Err` without touching anything; a
    /// failure disables the session.
    async fn transact<T>(
        &self,
        op: impl FnOnce(&mut View) -> Result<T, String>,
    ) -> Result<T, String> {
        if !self.enabled.get() {
            return Err("ledger disabled".to_owned());
        }
        let db = loop {
            match self.open() {
                Ok(db) => break db,
                Err(OpenError::Held) => compio::runtime::time::sleep(OPEN_POLL).await,
                Err(OpenError::Other(e)) => {
                    self.disable(&e);
                    return Err(e);
                }
            }
        };
        let result = self.run(db, op);
        if let Err(e) = &result {
            self.disable(e);
        }
        result
    }

    /// [`Session::transact`] for synchronous contexts (`Drop`).
    fn transact_blocking<T>(
        &self,
        op: impl FnOnce(&mut View) -> Result<T, String>,
    ) -> Result<T, String> {
        if !self.enabled.get() {
            return Err("ledger disabled".to_owned());
        }
        let db = loop {
            match self.open() {
                Ok(db) => break db,
                Err(OpenError::Held) => std::thread::sleep(OPEN_POLL),
                Err(OpenError::Other(e)) => {
                    self.disable(&e);
                    return Err(e);
                }
            }
        };
        let result = self.run(db, op);
        if let Err(e) = &result {
            self.disable(e);
        }
        result
    }

    fn open(&self) -> Result<Db, OpenError> {
        let path = self.root.join(LEDGER_DIR);
        std::fs::create_dir_all(&self.root).map_err(|e| {
            OpenError::Other(format!("cannot create `{}`: {e}", self.root.display()))
        })?;
        self.runtime
            .block_on(Db::open_with_options(&path, DbOptions::paranoid()))
            .map_err(|e| match e {
                DbError::DirectoryLocked { .. } => OpenError::Held,
                other => OpenError::Other(format!(
                    "cannot open the cache ledger `{}`: {other}",
                    path.display()
                )),
            })
    }

    fn run<T>(&self, db: Db, op: impl FnOnce(&mut View) -> Result<T, String>) -> Result<T, String> {
        let result = (|| {
            let mut view = View::load(&self.runtime, &db, &self.root)?;
            view.reap(&self.id);
            let value = op(&mut view)?;
            self.runtime
                .block_on(db.write_batch(&view.batch))
                .map_err(|e| format!("cannot write the cache ledger: {e}"))?;
            Ok(value)
        })();
        if let Err(e) = self.runtime.block_on(db.close()) {
            tracing::debug!("cannot close the cache ledger: {e}");
        }
        result
    }
}

impl Drop for Session {
    fn drop(&mut self) {
        if self.abandoned.get() || self.lock.is_none() {
            return;
        }
        let id = self.id.clone();
        let _ = self.transact_blocking(|view| {
            view.remove_pins_of(&id);
            if !view.names(&id) {
                view.remove_session(&id);
            }
            Ok(())
        });
        // Release the lock before removing its file, so a reaper that just
        // locked the orphan is deleting a file we no longer hold.
        drop(self.lock.take());
        let _ = std::fs::remove_file(session_lock_path(&self.root, &id));
    }
}

enum OpenError {
    /// Another session has the ledger open.
    Held,
    Other(String),
}

/// The ledger, loaded whole (it holds tens of rows) for one transaction.
/// Mutations go through the methods, which also record the batch.
struct View {
    root: PathBuf,
    now: u64,
    entries: BTreeMap<String, Entry>,
    pins: BTreeSet<(String, String)>,
    sessions: BTreeMap<String, SessionRow>,
    total: u64,
    batch: WriteBatch,
}

impl View {
    fn load(runtime: &Runtime, db: &Db, root: &Path) -> Result<View, String> {
        let get = |key: &str| runtime.block_on(db.get(key)).map_err(|e| e.to_string());
        let scan = |prefix: &str| {
            runtime
                .block_on(db.scan_prefix(prefix))
                .map_err(|e| e.to_string())
        };
        let mut batch = WriteBatch::new();
        match get(SCHEMA_KEY)? {
            None => batch.put(SCHEMA_KEY, SCHEMA.to_string()),
            Some(raw) => {
                let found: u64 = String::from_utf8_lossy(&raw)
                    .trim()
                    .parse()
                    .unwrap_or(u64::MAX);
                if found > SCHEMA {
                    return Err(format!(
                        "the cache ledger `{}` was written by a newer rene (schema {found}, this \
                         rene knows {SCHEMA})",
                        root.join(LEDGER_DIR).display()
                    ));
                }
            }
        }
        let mut entries = BTreeMap::new();
        for (key, value) in scan(ENTRY)? {
            let kk = String::from_utf8_lossy(&key[ENTRY.len()..]).into_owned();
            match serde_json::from_slice::<Entry>(&value) {
                Ok(entry) => {
                    entries.insert(kk, entry);
                }
                Err(e) => {
                    // A row this rene cannot read is dropped rather than
                    // guessed at; its directory, if any, is re-registered
                    // by the next session that needs it.
                    tracing::debug!(row = %kk, "dropping an unreadable ledger row: {e}");
                    batch.delete(key);
                }
            }
        }
        let mut pins = BTreeSet::new();
        for (key, _) in scan(PIN)? {
            let rest = String::from_utf8_lossy(&key[PIN.len()..]).into_owned();
            match rest.split_once('/') {
                Some((session, kk)) => {
                    pins.insert((session.to_owned(), kk.to_owned()));
                }
                None => batch.delete(key),
            }
        }
        let mut sessions = BTreeMap::new();
        for (key, value) in scan(SESSION)? {
            let id = String::from_utf8_lossy(&key[SESSION.len()..]).into_owned();
            let row = serde_json::from_slice(&value).unwrap_or(SessionRow { pid: 0, started: 0 });
            sessions.insert(id, row);
        }
        let total = get(TOTAL)?
            .and_then(|raw| String::from_utf8_lossy(&raw).trim().parse().ok())
            .unwrap_or(0);
        Ok(View {
            root: root.to_owned(),
            now: now_secs(),
            entries,
            pins,
            sessions,
            total,
            batch,
        })
    }

    /// Repair after every dead session but `me`. See the module docs.
    fn reap(&mut self, me: &str) {
        let ids: Vec<String> = self.sessions.keys().cloned().collect();
        for id in ids {
            if id == me {
                continue;
            }
            let Some(_held) = lock_if_dead(&session_lock_path(&self.root, &id)) else {
                continue;
            };
            self.remove_pins_of(&id);
            let owned: Vec<(String, Entry)> = self
                .entries
                .iter()
                .filter(|(_, e)| e.owner.as_deref() == Some(&id))
                .map(|(kk, e)| (kk.clone(), e.clone()))
                .collect();
            for (kk, entry) in owned {
                let dir = self.entry_dir(&kk);
                match entry.state {
                    State::Building => {
                        // Only if no live session took the build lock over.
                        if let Some(_build) = lock_if_dead(&build_lock_path(&self.root, &kk)) {
                            if dir.exists()
                                && let Err(e) = std::fs::remove_dir_all(&dir)
                            {
                                tracing::debug!(
                                    "cannot remove the abandoned entry `{}`: {e}",
                                    dir.display()
                                );
                                continue;
                            }
                            self.remove_entry(&kk);
                        }
                    }
                    State::Pruning => {
                        if dir.exists() {
                            self.revive(&kk, entry);
                        } else {
                            self.remove_entry(&kk);
                        }
                    }
                    State::Live => {
                        self.set_entry(
                            &kk,
                            Entry {
                                owner: None,
                                ..entry
                            },
                        );
                    }
                }
            }
            if !self.names(&id) {
                self.remove_session(&id);
                let _ = std::fs::remove_file(session_lock_path(&self.root, &id));
            }
        }
        // Lock files of sessions that ended normally, or whose rows an
        // earlier reaper removed.
        if let Ok(files) = std::fs::read_dir(self.root.join(SESSIONS_DIR)) {
            for file in files.flatten() {
                let path = file.path();
                let Some(id) = path
                    .file_name()
                    .and_then(|n| n.to_str())
                    .and_then(|n| n.strip_suffix(".lock"))
                else {
                    continue;
                };
                if id != me && !self.sessions.contains_key(id) && lock_if_dead(&path).is_some() {
                    let _ = std::fs::remove_file(&path);
                }
            }
        }
    }

    /// Does any entry row name `session` as its owner?
    fn names(&self, session: &str) -> bool {
        self.entries
            .values()
            .any(|e| e.owner.as_deref() == Some(session))
    }

    fn entry_dir(&self, kk: &str) -> PathBuf {
        let (kind, key) = kk.split_once('/').unwrap_or((kk, ""));
        self.root.join(kind).join(key)
    }

    /// A `pruning` entry whose directory is still there is live again.
    fn revive(&mut self, kk: &str, entry: Entry) {
        self.total += entry.size;
        self.set_total();
        self.set_entry(
            kk,
            Entry {
                state: State::Live,
                owner: None,
                last_used: self.now,
                ..entry
            },
        );
    }

    fn claim_building(&mut self, me: &str, kk: &str, last_used: u64) {
        self.set_entry(
            kk,
            Entry {
                state: State::Building,
                owner: Some(me.to_owned()),
                size: 0,
                last_used,
            },
        );
    }

    fn set_entry(&mut self, kk: &str, entry: Entry) {
        self.batch.put(
            format!("{ENTRY}{kk}"),
            serde_json::to_vec(&entry).expect("entry row serializes"),
        );
        self.entries.insert(kk.to_owned(), entry);
    }

    fn remove_entry(&mut self, kk: &str) {
        self.batch.delete(format!("{ENTRY}{kk}"));
        self.entries.remove(kk);
    }

    fn add_pin(&mut self, session: &str, kk: &str) {
        if self.pins.insert((session.to_owned(), kk.to_owned())) {
            self.batch.put(format!("{PIN}{session}/{kk}"), "");
        }
    }

    fn remove_pin(&mut self, session: &str, kk: &str) {
        if self.pins.remove(&(session.to_owned(), kk.to_owned())) {
            self.batch.delete(format!("{PIN}{session}/{kk}"));
        }
    }

    fn remove_pins_of(&mut self, session: &str) {
        let mine: Vec<String> = self
            .pins
            .iter()
            .filter(|(s, _)| s == session)
            .map(|(_, kk)| kk.clone())
            .collect();
        for kk in mine {
            self.remove_pin(session, &kk);
        }
    }

    fn set_session(&mut self, id: &str, row: &SessionRow) {
        self.batch.put(
            format!("{SESSION}{id}"),
            serde_json::to_vec(row).expect("session row serializes"),
        );
        self.sessions.insert(id.to_owned(), row.clone());
    }

    fn remove_session(&mut self, id: &str) {
        self.batch.delete(format!("{SESSION}{id}"));
        self.sessions.remove(id);
    }

    fn set_total(&mut self) {
        self.batch.put(TOTAL, self.total.to_string());
    }

    fn snapshot(&self) -> Snapshot {
        Snapshot {
            entries: self.entries.clone(),
            pins: self.pins.clone(),
            sessions: self.sessions.clone(),
            total: self.total,
        }
    }
}

fn kk(kind: &str, key: &str) -> String {
    format!("{kind}/{key}")
}

fn session_lock_path(root: &Path, id: &str) -> PathBuf {
    root.join(SESSIONS_DIR).join(format!("{id}.lock"))
}

fn build_lock_path(root: &Path, kk: &str) -> PathBuf {
    let (kind, key) = kk.split_once('/').unwrap_or((kk, ""));
    root.join(kind).join(format!("{key}.lock"))
}

/// Create and lock this session's lock file.
fn take_session_lock(root: &Path, id: &str) -> Result<File, String> {
    let dir = root.join(SESSIONS_DIR);
    std::fs::create_dir_all(&dir).map_err(|e| format!("cannot create `{}`: {e}", dir.display()))?;
    let path = session_lock_path(root, id);
    let file = OpenOptions::new()
        .create(true)
        .truncate(false)
        .write(true)
        .open(&path)
        .map_err(|e| format!("cannot create the session lock `{}`: {e}", path.display()))?;
    match file.try_lock() {
        Ok(()) => Ok(file),
        Err(TryLockError::WouldBlock) => Err(format!(
            "the session lock `{}` is already held",
            path.display()
        )),
        Err(TryLockError::Error(e)) => Err(format!(
            "cannot lock the session lock `{}`: {e}",
            path.display()
        )),
    }
}

/// Take the lock at `path` if nobody holds it. `Some` means its holder is
/// gone, or there never was one (a missing file counts as free and yields no
/// guard); a guard is held until dropped. `None` means held, or unknowable,
/// which is treated as held: a reaper must never act on a session it cannot
/// prove dead.
fn lock_if_dead(path: &Path) -> Option<Option<File>> {
    let file = match OpenOptions::new().write(true).open(path) {
        Ok(file) => file,
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => return Some(None),
        Err(_) => return None,
    };
    match file.try_lock() {
        Ok(()) => Some(Some(file)),
        Err(_) => None,
    }
}

fn now_secs() -> u64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map_or(0, |d| d.as_secs())
}

fn now_nanos() -> u128 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map_or(0, |d| d.as_nanos())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn block_on<T>(fut: impl Future<Output = T>) -> T {
        compio::runtime::Runtime::new().unwrap().block_on(fut)
    }

    fn cache(tmp: &Path) -> Cache {
        Cache::at(tmp.join("cache")).unwrap()
    }

    /// Build `<kind>/<key>` as `session` would: look it up, take the build
    /// lock, recheck, create the directory, register it.
    async fn build(cache: &Cache, session: &Session, kind: &str, key: &str, size: u64) {
        let dir = cache.entry(kind, key);
        assert_eq!(session.lookup(kind, key, &dir).await, Lookup::Build);
        let _lock = cache.lock(kind, key, "testing", |_| {}).await.unwrap();
        assert_eq!(
            session.recheck(kind, key, &dir, false).await,
            Recheck::Build
        );
        std::fs::create_dir_all(&dir).unwrap();
        std::fs::write(dir.join("marker"), "x".repeat(size as usize)).unwrap();
        session.register(kind, key, size).await;
    }

    #[test]
    fn an_entry_is_built_once_then_used_by_everyone() {
        let tmp = tempfile::tempdir().unwrap();
        let cache = cache(tmp.path());
        block_on(async {
            let a = Session::start(&cache).await;
            assert!(a.enabled());
            build(&cache, &a, "k", "e", 5).await;
            let snap = a.snapshot().await.unwrap();
            assert_eq!(snap.entries["k/e"].state, State::Live);
            assert_eq!(snap.entries["k/e"].size, 5);
            assert_eq!(snap.total, 5);
            assert!(snap.pins.contains(&(a.id().to_owned(), "k/e".to_owned())));

            // A second session finds it live and pins it too.
            let b = Session::start(&cache).await;
            assert_eq!(
                b.lookup("k", "e", &cache.entry("k", "e")).await,
                Lookup::Live
            );
            let snap = b.snapshot().await.unwrap();
            assert_eq!(snap.pins.len(), 2);
            assert_eq!(snap.sessions.len(), 2);

            // Ending a session drops its pins and its row; the other stays.
            let a_id = a.id().to_owned();
            drop(a);
            let snap = b.snapshot().await.unwrap();
            assert_eq!(snap.pins.len(), 1);
            assert!(!snap.sessions.contains_key(&a_id));
            assert!(
                !tmp.path()
                    .join("cache")
                    .join(SESSIONS_DIR)
                    .join(format!("{a_id}.lock"))
                    .exists()
            );

            // Unpinning keeps the entry, only the pin goes.
            b.unpin("k", "e").await;
            let snap = b.snapshot().await.unwrap();
            assert!(snap.pins.is_empty());
            assert_eq!(snap.entries["k/e"].state, State::Live);
        });
    }

    #[test]
    fn an_entry_under_prune_is_reclaimed_while_its_directory_lasts() {
        let tmp = tempfile::tempdir().unwrap();
        let cache = cache(tmp.path());
        block_on(async {
            let a = Session::start(&cache).await;
            build(&cache, &a, "k", "e", 7).await;
            a.unpin("k", "e").await;
            // A pruner (not a registered session, so the reaper leaves its
            // row alone) marks the entry.
            let mark = |view: &mut View| {
                let mut entry = view.entries["k/e"].clone();
                entry.state = State::Pruning;
                entry.owner = Some("pruner".to_owned());
                view.total -= entry.size;
                view.set_total();
                view.set_entry("k/e", entry);
                Ok(())
            };
            a.transact(mark).await.unwrap();
            assert_eq!(a.snapshot().await.unwrap().total, 0);

            // Directory still there: reclaimed, live, counted again.
            let dir = cache.entry("k", "e");
            assert_eq!(a.lookup("k", "e", &dir).await, Lookup::Live);
            let snap = a.snapshot().await.unwrap();
            assert_eq!(snap.entries["k/e"].state, State::Live);
            assert_eq!(snap.entries["k/e"].owner, None);
            assert_eq!(snap.total, 7);

            // Directory gone: wait for the pruner to finish.
            a.transact(mark).await.unwrap();
            std::fs::remove_dir_all(&dir).unwrap();
            assert_eq!(a.lookup("k", "e", &dir).await, Lookup::Wait);
            let _lock = cache.lock("k", "e", "testing", |_| {}).await.unwrap();
            assert_eq!(a.recheck("k", "e", &dir, false).await, Recheck::Wait);
        });
    }

    #[test]
    fn a_dead_session_is_repaired_by_the_next_operation() {
        let tmp = tempfile::tempdir().unwrap();
        let cache = cache(tmp.path());
        block_on(async {
            // `dead` leaves behind: a pin, a live entry it used, an entry it
            // was building (directory half written), one it was pruning
            // whose directory survives, and one it was pruning whose
            // directory is gone.
            let dead = Session::start_as(&cache, "dead").await;
            build(&cache, &dead, "k", "used", 1).await;
            build(&cache, &dead, "k", "kept", 2).await;
            build(&cache, &dead, "k", "gone", 3).await;
            let building = cache.entry("k", "building");
            assert_eq!(dead.lookup("k", "building", &building).await, Lookup::Build);
            {
                let _lock = cache
                    .lock("k", "building", "testing", |_| {})
                    .await
                    .unwrap();
                assert_eq!(
                    dead.recheck("k", "building", &building, false).await,
                    Recheck::Build
                );
            }
            std::fs::create_dir_all(&building).unwrap();
            dead.transact(|view| {
                for (kk, size) in [("k/kept", 2), ("k/gone", 3)] {
                    let mut entry = view.entries[kk].clone();
                    entry.state = State::Pruning;
                    entry.owner = Some("dead".to_owned());
                    view.total -= size;
                    view.set_total();
                    view.set_entry(kk, entry);
                }
                Ok(())
            })
            .await
            .unwrap();
            std::fs::remove_dir_all(cache.entry("k", "gone")).unwrap();
            dead.abandon();

            let a = Session::start(&cache).await;
            let snap = a.snapshot().await.unwrap();
            assert!(!snap.sessions.contains_key("dead"), "{snap:#?}");
            assert!(snap.pins.iter().all(|(s, _)| s != "dead"));
            assert_eq!(snap.entries["k/used"].state, State::Live);
            assert_eq!(snap.entries["k/kept"].state, State::Live);
            assert_eq!(snap.entries["k/kept"].owner, None);
            assert!(!snap.entries.contains_key("k/gone"));
            assert!(!snap.entries.contains_key("k/building"));
            assert!(!building.exists());
            assert_eq!(snap.total, 1 + 2);
            assert!(
                !tmp.path()
                    .join("cache")
                    .join(SESSIONS_DIR)
                    .join("dead.lock")
                    .exists()
            );
        });
    }

    #[test]
    fn a_dead_builder_whose_lock_was_taken_over_is_left_to_the_new_holder() {
        let tmp = tempfile::tempdir().unwrap();
        let cache = cache(tmp.path());
        block_on(async {
            let dead = Session::start_as(&cache, "dead").await;
            let dir = cache.entry("k", "e");
            assert_eq!(dead.lookup("k", "e", &dir).await, Lookup::Build);
            {
                let _lock = cache.lock("k", "e", "testing", |_| {}).await.unwrap();
                assert_eq!(dead.recheck("k", "e", &dir, false).await, Recheck::Build);
            }
            std::fs::create_dir_all(&dir).unwrap();
            dead.abandon();

            // The build lock is taken before any reaper runs (starting a
            // session is itself an operation): the row and the dead session
            // must survive until the holder takes the row over.
            let lock = cache.lock("k", "e", "testing", |_| {}).await.unwrap();
            let a = Session::start(&cache).await;
            let snap = a.snapshot().await.unwrap();
            assert_eq!(snap.entries["k/e"].owner.as_deref(), Some("dead"));
            assert!(snap.sessions.contains_key("dead"));
            assert_eq!(a.recheck("k", "e", &dir, false).await, Recheck::Build);
            let snap = a.snapshot().await.unwrap();
            assert_eq!(snap.entries["k/e"].owner.as_deref(), Some(a.id()));
            assert!(!snap.sessions.contains_key("dead"));
            drop(lock);
        });
    }

    #[test]
    fn a_newer_ledger_disables_the_session_which_then_trusts_the_disk() {
        let tmp = tempfile::tempdir().unwrap();
        let cache = cache(tmp.path());
        let runtime = tokio::runtime::Builder::new_current_thread()
            .enable_all()
            .build()
            .unwrap();
        runtime.block_on(async {
            let db = Db::open_with_options(cache.root().join(LEDGER_DIR), DbOptions::paranoid())
                .await
                .unwrap();
            db.insert(SCHEMA_KEY, "99").await.unwrap();
            db.close().await.unwrap();
        });
        block_on(async {
            let s = Session::start(&cache).await;
            assert!(!s.enabled());
            let dir = cache.entry("k", "e");
            assert_eq!(s.lookup("k", "e", &dir).await, Lookup::Live);
            assert_eq!(s.recheck("k", "e", &dir, false).await, Recheck::Build);
            assert_eq!(s.recheck("k", "e", &dir, true).await, Recheck::Live);
            assert_eq!(s.register("k", "e", 1).await, 0);
        });
    }
}
