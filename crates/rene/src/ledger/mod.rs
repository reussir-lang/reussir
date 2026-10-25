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

use crate::cache::{Cache, FileLock};
use store::Store;
use view::View;
pub use view::{SessionRow, Snapshot};

/// The ledger database's directory under the cache root.
pub const LEDGER_DIR: &str = "ledger";
/// The directory of the session lock files under the cache root.
pub const SESSIONS_DIR: &str = "sessions";

/// How often a session retries a ledger another session has open.
const OPEN_POLL: Duration = Duration::from_millis(10);

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
