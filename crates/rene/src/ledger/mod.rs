//! The cache ledger: which sessions use which cache entries
//! (docs/design/cache-ledger.md).
//!
//! The shared [`cache`](crate::cache) publishes entries whole and lets builds
//! use complete ones without a lock, which is enough while nothing is ever
//! removed. Removing entries needs two facts no directory listing gives:
//! whether a running build still uses an entry, and when it was last wanted.
//! The ledger records them, transactionally, in a TurboKV database at
//! `<root>/ledger` ([`LEDGER_DIR`]):
//!
//! | key | value |
//! |---|---|
//! | `schema` | the ledger's schema version |
//! | `session/<id>` | a [`SessionRow`]: a running (or ended) rene |
//! | `pin/<session>/<kind>/<key>` | this session wants this entry |
//! | `entry/<kind>/<key>` | an [`Entry`]: state, size, last use |
//!
//! # Sessions and liveness
//!
//! Every rene run is a [`Session`]. It holds the lock
//! `<root>/sessions/<id>.lock` ([`SESSIONS_DIR`]) for its whole life, and
//! that lock is its only liveness signal: the operating system releases it
//! when the process exits, crashed or not. A session whose lock is free, or
//! whose lock file is gone, is dead; nothing is inferred from pids or
//! timestamps. The lock is a [`FileLock`], whose contract (a lock file is
//! deleted only by its holder, and a locker checks that the path still
//! names the file it locked) is what makes a missing file proof of death.
//!
//! A session ends by releasing its lock and deleting the file, with no
//! ledger operation, so an exiting rene never waits for the ledger.
//!
//! # Entries
//!
//! A session that wants an entry looks it up ([`Session::lookup`]), which
//! pins the key and stamps its last use whether or not the entry exists
//! yet: a pin means "I want this key", so nothing will remove the entry
//! from under the session. A live entry is used if the consumer's own
//! check finds it complete; one under prune whose directory is still there
//! is reclaimed. Otherwise the session takes the entry's lock and rechecks
//! ([`Session::recheck`]) with the consumer's verdict, since whoever held
//! the lock may have completed the entry: a complete entry is used, and
//! adopted if no row tracked it. If not, the row becomes `building`, owned
//! by this session, until [`Session::register`] marks it live with its
//! size. A session drops a pin with [`Session::unpin`] once it has copied
//! the entry out; entries read in place stay pinned for the session's life.
//!
//! # Transactions
//!
//! TurboKV holds an exclusive directory lock while a database is open, so
//! the ledger is opened for one operation at a time ([`store`]): open, load
//! every row ([`view`]), operate, commit one atomic batch, close.
//! Nobody keeps it open across a build; a session that finds it open
//! elsewhere polls. An operation never awaits and does only bounded I/O, so
//! nobody holds the ledger for long.
//!
//! # Degradation
//!
//! A ledger that cannot be opened or written, or that a newer rene wrote in
//! a schema this one does not know, disables the session with one warning.
//! A disabled session has no opinion on anything (its queries return
//! `None`), and rene goes on exactly as it does without a ledger.

mod store;
mod view;

use std::cell::Cell;
use std::path::PathBuf;
use std::time::{Duration, SystemTime, UNIX_EPOCH};

use crate::cache::{Cache, EntryId, FileLock};
use store::Store;
use view::View;
pub use view::{Entry, SessionRow, Snapshot, State};

/// The ledger database's directory under the cache root.
pub const LEDGER_DIR: &str = "ledger";
/// The directory of the session lock files under the cache root.
pub const SESSIONS_DIR: &str = "sessions";

/// How often a session retries a ledger another session has open.
const OPEN_POLL: Duration = Duration::from_millis(10);

/// What [`Session::lookup`] found.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Lookup {
    /// The entry is live (or was just reclaimed from a prune): check it and
    /// use it.
    Live,
    /// No usable entry: take the entry's lock, then [`Session::recheck`].
    Build,
    /// The entry is being removed and its directory is gone: wait, then
    /// look it up again.
    Wait,
}

/// What [`Session::recheck`] decided, under the entry's lock.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Recheck {
    /// The entry is complete and its row live: use it.
    Live,
    /// The row is this session's, `building`: build the entry, then
    /// [`Session::register`] it.
    Build,
    /// As [`Lookup::Wait`]: release the lock, wait, look it up again.
    Wait,
}

/// This rene's session in the cache ledger. Dropping it ends the session.
pub struct Session {
    cache: Cache,
    id: String,
    /// The session lock, held for the session's life. `None` once released,
    /// or when it could not be taken, which also disables the session.
    lock: Option<FileLock>,
    /// `None` when the ledger's runtime could not start: disabled.
    store: Option<Store>,
    /// Cleared, with a warning, on the first failure.
    enabled: Cell<bool>,
}

impl Session {
    /// Start a session in `cache`: take the session lock, then record the
    /// session's row. Never fails: a ledger that cannot be used leaves the
    /// session disabled, with a warning.
    pub async fn start(cache: &Cache) -> Session {
        Self::start_as(cache, &format!("{}-{:x}", std::process::id(), now_nanos())).await
    }

    async fn start_as(cache: &Cache, id: &str) -> Session {
        let mut session = Session {
            cache: cache.clone(),
            id: id.to_owned(),
            lock: None,
            store: None,
            enabled: Cell::new(true),
        };
        match Store::new(cache.root()) {
            Ok(store) => session.store = Some(store),
            Err(e) => {
                session.disable(&e);
                return session;
            }
        }
        match session.take_lock().await {
            Ok(lock) => session.lock = Some(lock),
            Err(e) => {
                session.disable(&e);
                return session;
            }
        }
        // The lock is held before the row exists, so no reaper ever finds a
        // row whose session has not locked its file yet.
        let row = SessionRow {
            pid: std::process::id(),
            started: now_secs(),
        };
        let id = session.id.clone();
        session.transact(|view| view.set_session(&id, row)).await;
        session
    }

    /// Create and lock `sessions/<id>.lock`. A reaper may hold or delete a
    /// fresh lock file before it is locked, for an instant, so this waits
    /// and starts over rather than fail.
    async fn take_lock(&self) -> Result<FileLock, String> {
        let path = self.lock_path();
        let dir = path.parent().expect("a session lock lies in its directory");
        std::fs::create_dir_all(dir)
            .map_err(|e| format!("cannot create `{}`: {e}", dir.display()))?;
        FileLock::lock(&path, |_| {})
            .await
            .map_err(|e| format!("cannot take the session lock `{}`: {e}", path.display()))
    }

    fn lock_path(&self) -> PathBuf {
        session_lock(&self.cache, &self.id)
    }

    pub fn id(&self) -> &str {
        &self.id
    }

    /// Whether the ledger is in use. A disabled session has no opinion on
    /// anything, and rene goes on as without a ledger.
    pub fn enabled(&self) -> bool {
        self.enabled.get()
    }

    /// Ask for the entry `id`: pin it, stamp its last use, and say whether
    /// it can be used, must be built, or is being removed right now. `None`
    /// when the session is disabled: go on as without a ledger.
    ///
    /// [`Lookup::Live`] promises that nobody removes the entry while this
    /// session pins it, not that it is complete: the caller still checks.
    pub async fn lookup(&self, id: &EntryId) -> Option<Lookup> {
        let dir = self.cache.entry(id);
        self.transact(|view| {
            view.add_pin(&self.id, id);
            let Some(mut entry) = view.entries.get(id).cloned() else {
                return Lookup::Build;
            };
            let found = match entry.state {
                State::Live => Lookup::Live,
                // Wanted after all: the pruner finds the row live again.
                State::Pruning { .. } if dir.exists() => {
                    entry.state = State::Live;
                    Lookup::Live
                }
                State::Pruning { .. } => return Lookup::Wait,
                State::Building { .. } => Lookup::Build,
            };
            entry.last_used = view.now;
            view.set_entry(id, entry);
            found
        })
        .await
    }

    /// Under the entry's lock: decide whether to use the entry `id` or build
    /// it. `size` is the consumer's verdict: the entry's measured size if
    /// its own check finds it complete, `None` if not. [`Recheck::Build`]
    /// makes the row this session's, `building`.
    pub async fn recheck(&self, id: &EntryId, size: Option<u64>) -> Option<Recheck> {
        let dir = self.cache.entry(id);
        self.transact(|view| {
            let entry = view.entries.get(id).cloned();
            let last_used = entry.as_ref().map_or(view.now, |entry| entry.last_used);
            let state = entry.map(|entry| entry.state);
            match (state, size) {
                (Some(State::Live), Some(_)) => return Recheck::Live,
                // Complete, but under prune (reclaimed), built by a session
                // that died before registering it, or completed without
                // the ledger: adopt it rather than build it again.
                (_, Some(size)) => {
                    let live = Entry {
                        state: State::Live,
                        size,
                        last_used: view.now,
                    };
                    view.set_entry(id, live);
                    return Recheck::Live;
                }
                (Some(State::Pruning { .. }), None) if !dir.exists() => return Recheck::Wait,
                // Missing, broken, or abandoned: this session builds it.
                _ => {}
            }
            let building = Entry {
                state: State::Building {
                    owner: self.id.clone(),
                },
                size: 0,
                last_used,
            };
            view.set_entry(id, building);
            Recheck::Build
        })
        .await
    }

    /// The entry `id` is complete, `size` bytes on disk: mark it live.
    /// Returns the ledger's total. The pin taken at lookup stays.
    pub async fn register(&self, id: &EntryId, size: u64) -> Option<u64> {
        self.transact(|view| {
            let live = Entry {
                state: State::Live,
                size,
                last_used: view.now,
            };
            view.set_entry(id, live);
            view.total()
        })
        .await
    }

    /// Drop this session's pin on the entry `id`, which it no longer reads
    /// (it copied the entry out).
    pub async fn unpin(&self, id: &EntryId) {
        self.transact(|view| view.remove_pin(&self.id, id)).await;
    }

    /// A copy of the ledger. `None` when disabled.
    pub async fn snapshot(&self) -> Option<Snapshot> {
        self.transact(|view| view.snapshot()).await
    }

    /// Run `op` against the ledger in one transaction, waiting for another
    /// session's to finish (see the module docs). `None` when the session
    /// is disabled; a failure disables it.
    async fn transact<T>(&self, op: impl FnOnce(&mut View) -> T) -> Option<T> {
        let store = self.store.as_ref().filter(|_| self.enabled.get())?;
        let db = loop {
            match store.try_open() {
                Ok(Some(db)) => break db,
                Ok(None) => compio::runtime::time::sleep(OPEN_POLL).await,
                Err(e) => {
                    self.disable(&e);
                    return None;
                }
            }
        };
        store.run(db, op).map_err(|e| self.disable(&e)).ok()
    }

    fn disable(&self, why: &str) {
        if self.enabled.replace(false) {
            tracing::warn!("{why}; continuing without the cache ledger");
        }
    }
}

impl Drop for Session {
    /// End the session: release its lock and delete the file.
    fn drop(&mut self) {
        if let Some(lock) = self.lock.take()
            && let Err(e) = lock.remove()
        {
            tracing::debug!("cannot delete the session lock: {e}");
        }
    }
}

/// The lock file of session `id`.
fn session_lock(cache: &Cache, id: &str) -> PathBuf {
    cache.root().join(SESSIONS_DIR).join(format!("{id}.lock"))
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

    fn cache(tmp: &std::path::Path) -> Cache {
        Cache::at(tmp.join("cache")).unwrap()
    }

    fn id(key: &str) -> EntryId {
        EntryId::new(crate::cache::kind::DEPS, key)
    }

    /// Take the lock of the entry `id`, as `Cache::acquire` does.
    fn lock(cache: &Cache, id: &EntryId) -> FileLock {
        let path = cache.lock_path(id);
        std::fs::create_dir_all(path.parent().unwrap()).unwrap();
        FileLock::try_lock(&path).unwrap().unwrap()
    }

    /// Build the entry `id`, `size` bytes, as `session` would: look it up,
    /// take its lock, recheck, create it, register it.
    async fn build(cache: &Cache, session: &Session, id: &EntryId, size: u64) {
        assert_eq!(session.lookup(id).await, Some(Lookup::Build));
        let _lock = lock(cache, id);
        assert_eq!(session.recheck(id, None).await, Some(Recheck::Build));
        let dir = cache.entry(id);
        std::fs::create_dir_all(&dir).unwrap();
        std::fs::write(dir.join("marker"), "x".repeat(size as usize)).unwrap();
        session.register(id, size).await.unwrap();
    }

    /// Mark the entry `id` for removal by a pruner named `owner`.
    async fn mark(session: &Session, id: &EntryId, owner: &str) {
        session
            .transact(|view| {
                let mut entry = view.entries[id].clone();
                entry.state = State::Pruning {
                    owner: owner.to_owned(),
                };
                view.set_entry(id, entry);
            })
            .await
            .unwrap();
    }

    #[test]
    fn a_session_is_recorded_and_ends_with_its_lock() {
        let tmp = tempfile::tempdir().unwrap();
        let cache = cache(tmp.path());
        block_on(async {
            let a = Session::start(&cache).await;
            assert!(a.enabled());
            let snapshot = a.snapshot().await.unwrap();
            assert_eq!(snapshot.sessions[a.id()].pid, std::process::id());
            let a_lock = a.lock_path();
            assert!(FileLock::try_lock(&a_lock).unwrap().is_none());

            let b = Session::start(&cache).await;
            let snapshot = b.snapshot().await.unwrap();
            assert_eq!(snapshot.sessions.len(), 2, "{snapshot:#?}");

            // Ending a session releases its lock and deletes the file, and
            // touches nothing else.
            drop(a);
            assert!(!a_lock.exists());
            assert_eq!(b.snapshot().await.unwrap().sessions.len(), 2);
        });
    }

    #[test]
    fn an_entry_is_built_once_then_used_by_everyone() {
        let tmp = tempfile::tempdir().unwrap();
        let cache = cache(tmp.path());
        let e = id("e");
        block_on(async {
            let a = Session::start(&cache).await;
            build(&cache, &a, &e, 5).await;
            let snapshot = a.snapshot().await.unwrap();
            assert_eq!(snapshot.entries[&e].state, State::Live);
            assert_eq!(snapshot.entries[&e].size, 5);
            assert_eq!(snapshot.total, 5);
            assert!(snapshot.pins.contains(&(a.id().to_owned(), e.clone())));

            // Another session finds it live and pins it too; unpinning
            // drops only the pin.
            let b = Session::start(&cache).await;
            assert_eq!(b.lookup(&e).await, Some(Lookup::Live));
            assert_eq!(b.snapshot().await.unwrap().pins.len(), 2);
            b.unpin(&e).await;
            let snapshot = b.snapshot().await.unwrap();
            assert_eq!(snapshot.pins.len(), 1);
            assert_eq!(snapshot.entries[&e].state, State::Live);
        });
    }

    #[test]
    fn an_entry_under_prune_is_reclaimed_while_its_directory_lasts() {
        let tmp = tempfile::tempdir().unwrap();
        let cache = cache(tmp.path());
        let e = id("e");
        block_on(async {
            let a = Session::start(&cache).await;
            build(&cache, &a, &e, 7).await;
            a.unpin(&e).await;
            mark(&a, &e, "pruner").await;
            assert_eq!(a.snapshot().await.unwrap().total, 0);

            // The directory is still there: reclaimed, live, counted again.
            assert_eq!(a.lookup(&e).await, Some(Lookup::Live));
            let snapshot = a.snapshot().await.unwrap();
            assert_eq!(snapshot.entries[&e].state, State::Live);
            assert_eq!(snapshot.total, 7);

            // The directory is gone: wait for the pruner to finish.
            mark(&a, &e, "pruner").await;
            std::fs::remove_dir_all(cache.entry(&e)).unwrap();
            assert_eq!(a.lookup(&e).await, Some(Lookup::Wait));
            let _lock = lock(&cache, &e);
            assert_eq!(a.recheck(&e, None).await, Some(Recheck::Wait));
        });
    }

    /// Under the lock, a complete entry is used whatever its row says: one
    /// without a row (completed without the ledger) or with a `building`
    /// row (its builder died before registering) is adopted, at its
    /// measured size, not rebuilt. An incomplete live entry is rebuilt.
    #[test]
    fn a_complete_entry_is_adopted_and_a_broken_one_rebuilt() {
        let tmp = tempfile::tempdir().unwrap();
        let cache = cache(tmp.path());
        let [untracked, abandoned, broken] = ["untracked", "abandoned", "broken"].map(id);
        block_on(async {
            let a = Session::start(&cache).await;
            let b = Session::start(&cache).await;
            assert_eq!(b.lookup(&abandoned).await, Some(Lookup::Build));
            assert_eq!(b.recheck(&abandoned, None).await, Some(Recheck::Build));
            build(&cache, &a, &broken, 3).await;

            for entry in [&untracked, &abandoned] {
                assert_eq!(a.lookup(entry).await, Some(Lookup::Build));
                assert_eq!(a.recheck(entry, Some(11)).await, Some(Recheck::Live));
            }
            assert_eq!(a.lookup(&broken).await, Some(Lookup::Live));
            assert_eq!(a.recheck(&broken, None).await, Some(Recheck::Build));

            let snapshot = a.snapshot().await.unwrap();
            for entry in [&untracked, &abandoned] {
                assert_eq!(snapshot.entries[entry].state, State::Live);
                assert_eq!(snapshot.entries[entry].size, 11);
            }
            let owner = a.id().to_owned();
            assert_eq!(snapshot.entries[&broken].state, State::Building { owner });
            assert_eq!(snapshot.total, 22);
        });
    }

    #[test]
    fn a_newer_ledger_disables_the_session() {
        let tmp = tempfile::tempdir().unwrap();
        let cache = cache(tmp.path());
        let runtime = tokio::runtime::Builder::new_current_thread()
            .enable_all()
            .build()
            .unwrap();
        runtime.block_on(async {
            let path = cache.root().join(LEDGER_DIR);
            let db = turbokv::Db::open_with_options(path, turbokv::DbOptions::paranoid())
                .await
                .unwrap();
            db.insert(view::SCHEMA_KEY, "99").await.unwrap();
            db.close().await.unwrap();
        });
        block_on(async {
            let s = Session::start(&cache).await;
            assert!(!s.enabled());
            assert!(s.snapshot().await.is_none());
            // No opinion on anything: the caller goes on as without a
            // ledger.
            let e = id("e");
            assert_eq!(s.lookup(&e).await, None);
            assert_eq!(s.recheck(&e, Some(1)).await, None);
            assert_eq!(s.register(&e, 1).await, None);
        });
    }

    #[test]
    fn an_unusable_ledger_disables_the_session() {
        let tmp = tempfile::tempdir().unwrap();
        let cache = cache(tmp.path());
        std::fs::write(cache.root().join(LEDGER_DIR), "not a database").unwrap();
        block_on(async {
            let s = Session::start(&cache).await;
            assert!(!s.enabled());
            assert!(s.snapshot().await.is_none());
            // The session still holds its lock: reapers see it alive.
            assert!(s.lock_path().is_file());
        });
    }
}
