//! Advisory file locks: the cache's only means of mutual exclusion.
//!
//! A lock is the operating system's lock on an open file (`flock` on Unix,
//! `LockFileEx` on Windows). It is released when its holder closes the file
//! or exits, even by crashing, so a stale lock cannot wedge the cache.
//!
//! A lock file may be deleted, which needs care: a process that opened the
//! file just before another deleted it would lock a file nobody else can
//! reach any more, and believe it holds the lock while a third process locks
//! the new file at the same path. Two rules rule this out:
//!
//! - a lock file is deleted only by a process holding its lock
//!   ([`FileLock::remove`]);
//! - after locking, [`FileLock::try_lock`] and [`FileLock::probe`] check that
//!   the path still names the file they locked, and start over if not.
//!
//! On Unix the check compares the open file's device and inode with the
//! path's. On Windows lock files are opened without `FILE_SHARE_DELETE`, so
//! a file cannot be deleted while anyone has it open, and the path always
//! names the file.

use std::fs::{File, OpenOptions, TryLockError};
use std::io;
use std::path::{Path, PathBuf};
use std::time::Duration;

/// How often a waiting process retries a held lock.
const LOCK_POLL: Duration = Duration::from_millis(100);

/// A held lock. Dropping it closes the file, which releases the lock.
#[derive(Debug)]
#[must_use = "the lock is released when it is dropped"]
pub struct FileLock {
    file: File,
    path: PathBuf,
}

/// What [`FileLock::probe`] found.
#[derive(Debug)]
pub enum Probe {
    /// No lock file: nobody holds it, and nobody created it either.
    Absent,
    /// Another process holds the lock.
    Held,
    /// Nobody held the lock; now the caller does.
    Free(FileLock),
}

/// One attempt at locking an open file.
enum Attempt {
    Locked(FileLock),
    Held,
    /// The path no longer names the file that was locked: try again.
    Stale,
}

impl FileLock {
    /// Take the lock at `path` if nobody holds it, creating the file if
    /// needed. `None` when another process holds it; an error when the file
    /// cannot be opened or the file system cannot lock files.
    pub fn try_lock(path: &Path) -> io::Result<Option<Self>> {
        loop {
            match attempt(open(path, true)?, path)? {
                Attempt::Locked(lock) => return Ok(Some(lock)),
                Attempt::Held => return Ok(None),
                Attempt::Stale => continue,
            }
        }
    }

    /// Take the lock at `path`, waiting for another process to release it.
    /// `waiting` runs once, with the path, if the lock is held; the wait
    /// then polls on the async runtime instead of blocking it.
    pub async fn lock(path: &Path, waiting: impl FnOnce(&Path)) -> io::Result<Self> {
        let mut waiting = Some(waiting);
        loop {
            if let Some(lock) = Self::try_lock(path)? {
                return Ok(lock);
            }
            if let Some(waiting) = waiting.take() {
                waiting(path);
            }
            compio::runtime::time::sleep(LOCK_POLL).await;
        }
    }

    /// Whether anyone holds the lock at `path`, without ever creating the
    /// file. A free lock comes back held by the caller, who may then
    /// [`remove`](Self::remove) it.
    pub fn probe(path: &Path) -> io::Result<Probe> {
        loop {
            let file = match open(path, false) {
                Ok(file) => file,
                Err(e) if e.kind() == io::ErrorKind::NotFound => return Ok(Probe::Absent),
                Err(e) => return Err(e),
            };
            match attempt(file, path)? {
                Attempt::Locked(lock) => return Ok(Probe::Free(lock)),
                Attempt::Held => return Ok(Probe::Held),
                Attempt::Stale => continue,
            }
        }
    }

    pub fn path(&self) -> &Path {
        &self.path
    }

    /// Delete the lock file and release the lock. Safe against a process
    /// waiting on the same path: it locks whatever file the path names
    /// afterwards, never the deleted one.
    pub fn remove(self) -> io::Result<()> {
        let FileLock { file, path } = self;
        // Unix deletes while still holding, so no one locks the file between
        // the release and the deletion. Windows cannot delete a file that is
        // open without `FILE_SHARE_DELETE`, ours included, so it releases
        // first; a process that opens the file in between keeps it, and the
        // deletion fails harmlessly with a sharing violation.
        #[cfg(windows)]
        drop(file);
        let removed = std::fs::remove_file(&path);
        #[cfg(not(windows))]
        drop(file);
        match removed {
            Err(e) if e.kind() == io::ErrorKind::NotFound => Ok(()),
            Err(e) if cfg!(windows) && e.raw_os_error() == Some(SHARING_VIOLATION) => Ok(()),
            result => result,
        }
    }
}

/// Windows' `ERROR_SHARING_VIOLATION`.
const SHARING_VIOLATION: i32 = 32;

/// Open the lock file at `path` for locking, creating it if `create`.
fn open(path: &Path, create: bool) -> io::Result<File> {
    let mut options = OpenOptions::new();
    options
        .read(true)
        .write(true)
        .create(create)
        .truncate(false);
    #[cfg(windows)]
    {
        use std::os::windows::fs::OpenOptionsExt;
        // FILE_SHARE_READ | FILE_SHARE_WRITE, deliberately without
        // FILE_SHARE_DELETE: see the module docs.
        options.share_mode(0x1 | 0x2);
    }
    options.open(path)
}

/// Lock `file`, opened at `path`, if nobody holds it, and check that the
/// path still names it.
fn attempt(file: File, path: &Path) -> io::Result<Attempt> {
    match file.try_lock() {
        Ok(()) => {}
        Err(TryLockError::WouldBlock) => return Ok(Attempt::Held),
        Err(TryLockError::Error(e)) => return Err(e),
    }
    Ok(if names(path, &file)? {
        Attempt::Locked(FileLock {
            file,
            path: path.to_owned(),
        })
    } else {
        Attempt::Stale
    })
}

/// Whether `path` names the open `file`.
#[cfg(unix)]
fn names(path: &Path, file: &File) -> io::Result<bool> {
    use std::os::unix::fs::MetadataExt;
    let open = file.metadata()?;
    match std::fs::metadata(path) {
        Ok(named) => Ok(named.dev() == open.dev() && named.ino() == open.ino()),
        Err(e) if e.kind() == io::ErrorKind::NotFound => Ok(false),
        Err(e) => Err(e),
    }
}

/// Whether `path` names the open `file`: always, on Windows, where a lock
/// file cannot be deleted while it is open.
#[cfg(not(unix))]
fn names(_path: &Path, _file: &File) -> io::Result<bool> {
    Ok(true)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn block_on<T>(fut: impl Future<Output = T>) -> T {
        compio::runtime::Runtime::new().unwrap().block_on(fut)
    }

    #[test]
    fn a_held_lock_makes_the_next_locker_wait_and_say_so_once() {
        let tmp = tempfile::tempdir().unwrap();
        let path = tmp.path().join("k.lock");
        let held = FileLock::try_lock(&path).unwrap().unwrap();
        assert!(FileLock::try_lock(&path).unwrap().is_none());

        let (tx, rx) = std::sync::mpsc::channel();
        let waiter = {
            let path = path.clone();
            std::thread::spawn(move || {
                let lock = block_on(FileLock::lock(&path, |waited| {
                    tx.send(waited.to_owned()).unwrap();
                }))
                .unwrap();
                assert_eq!(lock.path(), path);
            })
        };
        // The waiter says it waits, and only then is released.
        let waited = rx.recv_timeout(Duration::from_secs(30)).unwrap();
        assert_eq!(waited, path);
        std::thread::sleep(3 * LOCK_POLL);
        assert!(!waiter.is_finished());
        drop(held);
        waiter.join().unwrap();
        // Once: the callback was consumed by the first wait.
        assert!(rx.try_recv().is_err());
    }

    /// The race behind a lost lock: a file opened, then deleted before it is
    /// locked. On Unix the path no longer names what was locked, so that
    /// lock does not count; on Windows the deletion itself fails.
    #[test]
    fn a_lock_on_a_deleted_file_does_not_count() {
        let tmp = tempfile::tempdir().unwrap();
        let path = tmp.path().join("k.lock");
        let file = open(&path, true).unwrap();
        match std::fs::remove_file(&path) {
            Ok(()) => assert!(matches!(attempt(file, &path).unwrap(), Attempt::Stale)),
            Err(_) => assert!(matches!(attempt(file, &path).unwrap(), Attempt::Locked(_))),
        }
        // Locking by path takes the file the path names now.
        let lock = FileLock::try_lock(&path).unwrap().unwrap();
        assert!(path.is_file());
        assert!(FileLock::try_lock(&path).unwrap().is_none());
        drop(lock);
    }

    #[test]
    fn a_probe_never_creates_the_file() {
        let tmp = tempfile::tempdir().unwrap();
        let path = tmp.path().join("k.lock");
        assert!(matches!(FileLock::probe(&path).unwrap(), Probe::Absent));
        assert!(!path.exists());

        let held = FileLock::try_lock(&path).unwrap().unwrap();
        assert!(matches!(FileLock::probe(&path).unwrap(), Probe::Held));
        drop(held);
        let Probe::Free(lock) = FileLock::probe(&path).unwrap() else {
            panic!("a released lock is free");
        };
        assert!(FileLock::try_lock(&path).unwrap().is_none());
        lock.remove().unwrap();
        assert!(!path.exists());
        assert!(matches!(FileLock::probe(&path).unwrap(), Probe::Absent));
    }

    /// A process waiting on a lock whose holder deletes the file ends up
    /// holding the file the path names, which everyone else then sees held.
    #[test]
    fn a_waiter_never_inherits_a_deleted_lock_file() {
        let tmp = tempfile::tempdir().unwrap();
        let path = tmp.path().join("k.lock");
        let held = FileLock::try_lock(&path).unwrap().unwrap();
        let (tx, rx) = std::sync::mpsc::channel();
        let waiter = {
            let path = path.clone();
            std::thread::spawn(move || {
                block_on(FileLock::lock(&path, |_| tx.send(()).unwrap())).unwrap()
            })
        };
        rx.recv_timeout(Duration::from_secs(30)).unwrap();
        held.remove().unwrap();
        let lock = waiter.join().unwrap();
        assert!(path.is_file());
        assert!(FileLock::try_lock(&path).unwrap().is_none());
        assert!(matches!(FileLock::probe(&path).unwrap(), Probe::Held));
        drop(lock);
    }
}
