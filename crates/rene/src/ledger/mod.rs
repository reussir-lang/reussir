//! The cache ledger: the TurboKV database at `<root>/ledger` that holds the
//! facts of the per-user cache (docs/design/cache-ledger.md, "Facts and
//! liveness" and "Transactions").
//!
//! TurboKV locks its directory while a database is open, so the ledger is
//! opened for one transaction at a time ([`Ledger::transact`]): open,
//! waiting while another rene has it; check the schema; run the operation;
//! commit what it wrote as one batch; close; delete the trash. The
//! operation does bounded I/O only, and renames directories through the
//! transaction ([`Txn::rename`], [`Txn::discard`]), so that a transaction
//! that does not commit renames them back, newest first. A commit error
//! leaves the outcome unknown (TurboKV may log the batch and replay it at
//! the next open); the renames are undone either way, and either outcome
//! leaves only disagreements the entries layer repairs.
//!
//! TurboKV runs on Tokio, so the ledger keeps a private current-thread
//! runtime that runs only inside its calls, like [`crate::db`]; the waits
//! sleep on compio.

use std::fmt::Display;
use std::io;
use std::path::{Path, PathBuf};
use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};

use tokio::runtime::Runtime;
use turbokv::{Db, DbError, DbOptions, WriteBatch};

use crate::lock::{Backoff, NOTICE_AFTER};

mod entry;
mod session;

pub use entry::{EntryId, Live, Published, Row};
pub use session::{Pins, SESSIONS_DIR, Session, Tag, alive, parse_sibling};

/// The ledger's directory under the cache root.
pub const LEDGER_DIR: &str = "ledger";

/// The variable that moves the cache root, which every error of a cache
/// that cannot be used suggests ([`unusable`]).
pub const CACHE_DIR_ENV: &str = "RENE_CACHE_DIR";

const SCHEMA_KEY: &str = "schema";
/// The schema this rene writes and reads. A ledger with a newer one is
/// refused rather than misread.
const SCHEMA: u64 = 1;

/// How often a transaction retries a ledger another rene has open.
const OPEN_POLL: Duration = Duration::from_millis(10);
/// How long a transaction whose rename Windows refused is run again.
const RETRY_REFUSED_FOR: Duration = Duration::from_secs(10);

/// The ledger of the cache at one root. Opens nothing until a transaction.
#[derive(Debug)]
pub struct Ledger {
    path: PathBuf,
    runtime: Runtime,
    retry_for: Duration,
    /// Make the next commit fail, as a full disk would.
    #[cfg(test)]
    fail_commit: std::cell::Cell<bool>,
}

/// One transaction's view of the ledger. Reads see the ledger as it was
/// when the transaction opened it; writes take effect at the commit.
pub struct Txn<'a> {
    runtime: &'a Runtime,
    db: &'a Db,
    path: &'a Path,
    now: u64,
    batch: WriteBatch,
    /// The renames done, oldest first: `(from, to)`.
    moves: Vec<(PathBuf, PathBuf)>,
    /// The directories renamed out, to delete once committed.
    trash: Vec<PathBuf>,
    /// A rename failed in a way that passes ([`transient`]).
    refused: bool,
    /// Refuse every rename, as Windows does while a file is open.
    #[cfg(test)]
    refuse: bool,
}

impl Ledger {
    /// The ledger of the cache rooted at `root`, which must exist.
    pub fn new(root: &Path) -> Result<Ledger, String> {
        let path = root.join(LEDGER_DIR);
        let runtime = tokio::runtime::Builder::new_current_thread()
            .enable_all()
            .build()
            .map_err(|e| format!("cannot start the runtime of `{}`: {e}", path.display()))?;
        Ok(Ledger {
            path,
            runtime,
            retry_for: RETRY_REFUSED_FOR,
            #[cfg(test)]
            fail_commit: std::cell::Cell::new(false),
        })
    }

    /// Run `op` as one transaction (see the module docs). If `op` fails
    /// after a rename Windows refused while a file was open, the
    /// transaction is undone, the ledger closed, and the whole transaction
    /// run again after a backoff, for up to ten seconds.
    pub async fn transact<T>(
        &self,
        mut op: impl FnMut(&mut Txn<'_>) -> Result<T, String>,
    ) -> Result<T, String> {
        let deadline = Instant::now() + self.retry_for;
        let mut backoff = Backoff::new();
        loop {
            let db = self.open().await?;
            let mut txn = Txn {
                runtime: &self.runtime,
                db: &db,
                path: &self.path,
                now: SystemTime::now()
                    .duration_since(UNIX_EPOCH)
                    .map_or(0, |d| d.as_secs()),
                batch: WriteBatch::new(),
                moves: Vec::new(),
                trash: Vec::new(),
                refused: false,
                #[cfg(test)]
                refuse: false,
            };
            let result = self.attempt(&mut txn, &mut op);
            let Txn { moves, trash, .. } = txn;
            if result.is_err() {
                rename_back(&moves);
            }
            if let Err(e) = self.runtime.block_on(db.close()) {
                tracing::debug!("cannot close `{}`: {e}", self.path.display());
            }
            match result {
                Ok(value) => {
                    for dir in trash {
                        if let Err(e) = std::fs::remove_dir_all(&dir) {
                            tracing::debug!("cannot delete `{}`: {e}", dir.display());
                        }
                    }
                    return Ok(value);
                }
                Err((true, _)) if Instant::now() < deadline => {
                    compio::runtime::time::sleep(backoff.pause()).await;
                }
                Err((_, message)) => return Err(message),
            }
        }
    }

    /// Open the ledger, polling while another rene has it open and saying
    /// so once if that lasts a second.
    async fn open(&self) -> Result<Db, String> {
        let start = Instant::now();
        let mut told = false;
        loop {
            let opened = Db::open_with_options(&self.path, DbOptions::paranoid());
            match self.runtime.block_on(opened) {
                Ok(db) => return Ok(db),
                Err(DbError::DirectoryLocked { .. }) => {
                    if !told && start.elapsed() >= NOTICE_AFTER {
                        told = true;
                        let path = self.path.display();
                        tracing::warn!("waiting for another rene to finish with `{path}`");
                    }
                    compio::runtime::time::sleep(OPEN_POLL).await;
                }
                Err(e) => return Err(unusable(&self.path, format_args!("cannot open it: {e}"))),
            }
        }
    }

    /// Check the schema, run `op`, and commit. A failure says whether to
    /// run the transaction again: only after `op` met a refused rename.
    fn attempt<T>(
        &self,
        txn: &mut Txn<'_>,
        op: &mut impl FnMut(&mut Txn<'_>) -> Result<T, String>,
    ) -> Result<T, (bool, String)> {
        let value = check_schema(txn)
            .and_then(|()| op(txn))
            .map_err(|e| (txn.refused, e))?;
        #[cfg(test)]
        if self.fail_commit.take() {
            return Err((false, unusable(&self.path, "the test failed the commit")));
        }
        if !txn.batch.is_empty() {
            let written = self.runtime.block_on(txn.db.write_batch(&txn.batch));
            let failed = |e: DbError| unusable(&self.path, format_args!("cannot write it: {e}"));
            written.map_err(|e| (false, failed(e)))?;
        }
        Ok(value)
    }
}

impl Txn<'_> {
    /// Unix seconds when the transaction began.
    pub fn now(&self) -> u64 {
        self.now
    }

    pub fn get(&self, key: &str) -> Result<Option<Vec<u8>>, String> {
        self.runtime
            .block_on(self.db.get(key))
            .map_err(|e| unusable(self.path, format_args!("cannot read it: {e}")))
    }

    /// Every row whose key starts with `prefix`, in key order.
    pub fn scan(&self, prefix: &str) -> Result<Vec<(String, Vec<u8>)>, String> {
        let rows = self
            .runtime
            .block_on(self.db.scan_prefix(prefix))
            .map_err(|e| unusable(self.path, format_args!("cannot read it: {e}")))?;
        rows.into_iter()
            .map(|(key, value)| match String::from_utf8(key) {
                Ok(key) => Ok((key, value)),
                Err(_) => Err(unusable(self.path, "it holds a key that is not UTF-8")),
            })
            .collect()
    }

    pub fn put(&mut self, key: &str, value: impl AsRef<[u8]>) {
        self.batch.put(key, value);
    }

    pub fn delete(&mut self, key: &str) {
        self.batch.delete(key);
    }

    /// Rename the directory `from` to `to`, to be undone if the transaction
    /// does not commit.
    pub fn rename(&mut self, from: &Path, to: &Path) -> io::Result<()> {
        #[cfg(test)]
        if self.refuse {
            self.refused = true;
            return Err(io::Error::other("the test refused the rename"));
        }
        if let Err(e) = std::fs::rename(from, to) {
            self.refused |= transient(&e);
            return Err(e);
        }
        self.moves.push((from.to_owned(), to.to_owned()));
        Ok(())
    }

    /// Rename the directory `dir` to `trash`, which is deleted once the
    /// transaction has committed and closed the ledger.
    pub fn discard(&mut self, dir: &Path, trash: PathBuf) -> io::Result<()> {
        self.rename(dir, &trash)?;
        self.trash.push(trash);
        Ok(())
    }
}

/// Write this rene's schema to a fresh ledger, and refuse a newer one.
fn check_schema(txn: &mut Txn<'_>) -> Result<(), String> {
    let Some(raw) = txn.get(SCHEMA_KEY)? else {
        txn.put(SCHEMA_KEY, SCHEMA.to_string());
        return Ok(());
    };
    let found = String::from_utf8_lossy(&raw);
    let found = found.trim();
    if found.parse::<u64>().is_ok_and(|found| found <= SCHEMA) {
        return Ok(());
    }
    let why =
        format!("it was written by a newer rene (schema `{found}`; this rene reads {SCHEMA})");
    Err(unusable(txn.path, why))
}

/// Undo the renames `moves` of a transaction that did not commit, newest
/// first, so that each puts back what the one before it expects.
fn rename_back(moves: &[(PathBuf, PathBuf)]) {
    for (from, to) in moves.iter().rev() {
        if let Err(e) = std::fs::rename(to, from) {
            let (to, from) = (to.display(), from.display());
            tracing::warn!("cannot rename `{to}` back to `{from}`: {e}");
        }
    }
}

/// Whether a failed rename may succeed if tried again: on Windows, access
/// denied, a sharing violation, or a lock violation, as while an antivirus
/// scans a file under the directory.
fn transient(err: &io::Error) -> bool {
    cfg!(windows) && matches!(err.raw_os_error(), Some(5 | 32 | 33))
}

/// The error of a cache that cannot be used safely: it names `path`, why,
/// and the way out (docs/design/cache-ledger.md, "Failure").
pub(crate) fn unusable(path: &Path, why: impl Display) -> String {
    format!(
        "cannot use the cache at `{}`: {why}; set {CACHE_DIR_ENV} to a directory on a local \
         file system that rene can write",
        path.display()
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    pub(crate) fn block_on<T>(fut: impl Future<Output = T>) -> T {
        compio::runtime::Runtime::new().unwrap().block_on(fut)
    }

    fn read(ledger: &Ledger, key: &str) -> Option<Vec<u8>> {
        block_on(ledger.transact(|txn| txn.get(key))).unwrap()
    }

    fn write(root: &Path, key: &str, value: &str) {
        let ledger = Ledger::new(root).unwrap();
        let put = |txn: &mut Txn<'_>| {
            txn.put(key, value);
            Ok(())
        };
        block_on(ledger.transact(put)).unwrap();
    }

    fn assert_unusable(err: &str, path: &Path) {
        assert!(err.contains(&format!("`{}`", path.display())), "{err}");
        assert!(err.contains(CACHE_DIR_ENV), "{err}");
    }

    #[test]
    fn a_commit_is_visible_to_the_next_transaction() {
        let tmp = tempfile::tempdir().unwrap();
        let ledger = Ledger::new(tmp.path()).unwrap();
        block_on(ledger.transact(|txn| {
            txn.put("a/1", "one");
            txn.put("a/2", "two");
            txn.put("b", "three");
            // Writes take effect at the commit.
            assert_eq!(txn.get("a/1")?, None);
            Ok(())
        }))
        .unwrap();
        let rows = block_on(ledger.transact(|txn| {
            txn.delete("a/2");
            txn.scan("a/")
        }))
        .unwrap();
        let rows: Vec<(&str, &[u8])> = rows.iter().map(|(k, v)| (&**k, &**v)).collect();
        assert_eq!(rows, [("a/1", b"one".as_slice()), ("a/2", b"two")]);
        assert_eq!(read(&ledger, "a/2"), None);
        let again = Ledger::new(tmp.path()).unwrap();
        assert_eq!(read(&again, "b").unwrap(), b"three");
    }

    #[test]
    fn a_fresh_ledger_gets_the_schema() {
        let tmp = tempfile::tempdir().unwrap();
        let ledger = Ledger::new(tmp.path()).unwrap();
        assert!(!tmp.path().join(LEDGER_DIR).exists());
        assert_eq!(read(&ledger, SCHEMA_KEY), None);
        assert_eq!(read(&ledger, SCHEMA_KEY).unwrap(), b"1");
    }

    #[test]
    fn a_newer_schema_is_refused() {
        let tmp = tempfile::tempdir().unwrap();
        write(tmp.path(), SCHEMA_KEY, "2");
        let ledger = Ledger::new(tmp.path()).unwrap();
        let err = block_on(ledger.transact(|_| Ok(()))).unwrap_err();
        assert_unusable(&err, &tmp.path().join(LEDGER_DIR));
        assert!(err.contains("newer rene"), "{err}");
    }

    #[test]
    fn a_file_at_the_ledger_path_is_refused() {
        let tmp = tempfile::tempdir().unwrap();
        let path = tmp.path().join(LEDGER_DIR);
        std::fs::write(&path, "not a database").unwrap();
        let err = block_on(Ledger::new(tmp.path()).unwrap().transact(|_| Ok(()))).unwrap_err();
        assert_unusable(&err, &path);
    }

    /// `a` to `b`, then `b` to `c`: only newest first puts `a` back.
    #[test]
    fn a_failed_commit_renames_back_newest_first() {
        let tmp = tempfile::tempdir().unwrap();
        let [a, b, c] = ["a", "b", "c"].map(|name| tmp.path().join(name));
        std::fs::create_dir(&a).unwrap();
        let ledger = Ledger::new(tmp.path()).unwrap();
        ledger.fail_commit.set(true);
        let err = block_on(ledger.transact(|txn| {
            txn.put("k", "v");
            txn.rename(&a, &b).unwrap();
            txn.discard(&b, c.clone()).unwrap();
            Ok(())
        }))
        .unwrap_err();
        assert!(err.contains("the test failed the commit"), "{err}");
        assert!(a.is_dir() && !b.exists() && !c.exists());
        assert_eq!(read(&ledger, "k"), None);
    }

    #[test]
    fn a_failed_operation_renames_back() {
        let tmp = tempfile::tempdir().unwrap();
        let [a, b] = ["a", "b"].map(|name| tmp.path().join(name));
        std::fs::create_dir(&a).unwrap();
        let ledger = Ledger::new(tmp.path()).unwrap();
        let err = block_on(ledger.transact(|txn| -> Result<(), String> {
            txn.put("k", "v");
            txn.rename(&a, &b).unwrap();
            Err("the operation failed".to_owned())
        }))
        .unwrap_err();
        assert_eq!(err, "the operation failed");
        assert!(a.is_dir() && !b.exists());
        assert_eq!(read(&ledger, "k"), None);
    }

    #[test]
    fn discarded_directories_are_deleted_after_the_commit() {
        let tmp = tempfile::tempdir().unwrap();
        let [dir, trash] = ["dir", "trash"].map(|name| tmp.path().join(name));
        std::fs::create_dir_all(dir.join("sub")).unwrap();
        std::fs::write(dir.join("sub/file"), "x").unwrap();
        let ledger = Ledger::new(tmp.path()).unwrap();
        block_on(ledger.transact(|txn| {
            txn.discard(&dir, trash.clone()).unwrap();
            assert!(trash.is_dir(), "deleted only after the commit");
            Ok(())
        }))
        .unwrap();
        assert!(!dir.exists() && !trash.exists());
    }

    #[test]
    fn a_second_opener_waits_for_the_first() {
        let tmp = tempfile::tempdir().unwrap();
        let runtime = tokio::runtime::Runtime::new().unwrap();
        let path = tmp.path().join(LEDGER_DIR);
        let held = runtime
            .block_on(Db::open_with_options(path, DbOptions::paranoid()))
            .unwrap();
        let waiter = {
            let root = tmp.path().to_owned();
            std::thread::spawn(move || write(&root, "k", "v"))
        };
        std::thread::sleep(Duration::from_millis(200));
        assert!(!waiter.is_finished());
        runtime.block_on(held.close()).unwrap();
        waiter.join().unwrap();
        assert!(read(&Ledger::new(tmp.path()).unwrap(), "k").is_some());
    }

    /// The rename is refused until another rene has written `other`, which
    /// it can only do while the refused transaction has the ledger closed.
    #[test]
    fn a_refused_rename_runs_the_transaction_again() {
        let tmp = tempfile::tempdir().unwrap();
        let [a, b, from, to] = ["a", "b", "from", "to"].map(|name| tmp.path().join(name));
        std::fs::create_dir(&a).unwrap();
        std::fs::create_dir(&from).unwrap();
        let (refused, first_refusal) = std::sync::mpsc::channel();
        let other = {
            let root = tmp.path().to_owned();
            std::thread::spawn(move || {
                first_refusal.recv().unwrap();
                write(&root, "other", "");
            })
        };
        let ledger = Ledger::new(tmp.path()).unwrap();
        let mut attempts = 0;
        block_on(ledger.transact(|txn| {
            attempts += 1;
            // The previous attempt's rename was put back.
            txn.rename(&a, &b).map_err(|e| e.to_string())?;
            txn.refuse = txn.get("other")?.is_none();
            let result = txn.rename(&from, &to).map_err(|e| e.to_string());
            if result.is_err() && attempts == 1 {
                refused.send(()).unwrap();
            }
            result
        }))
        .unwrap();
        other.join().unwrap();
        assert!(attempts > 1);
        assert!(b.is_dir() && to.is_dir() && !a.exists() && !from.exists());
    }

    #[test]
    fn a_rename_refused_for_good_fails_at_the_deadline() {
        let tmp = tempfile::tempdir().unwrap();
        let [from, to] = ["from", "to"].map(|name| tmp.path().join(name));
        std::fs::create_dir(&from).unwrap();
        let mut ledger = Ledger::new(tmp.path()).unwrap();
        ledger.retry_for = Duration::from_millis(300);
        let start = Instant::now();
        let mut attempts = 0;
        let err = block_on(ledger.transact(|txn| {
            attempts += 1;
            txn.refuse = true;
            txn.rename(&from, &to).map_err(|e| e.to_string())
        }))
        .unwrap_err();
        assert!(start.elapsed() >= ledger.retry_for);
        assert!(attempts > 1);
        assert_eq!(err, "the test refused the rename");
        assert!(from.is_dir());
    }
}
