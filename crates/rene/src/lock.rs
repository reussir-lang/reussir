//! Advisory file locks, the only means of mutual exclusion in the per-user
//! cache (docs/design/cache-ledger.md, "File locks and their contract").
//!
//! A lock is the operating system's lock on an open file (`flock` on Unix,
//! `LockFileEx` on Windows). The OS releases it when its holder closes the
//! file or exits, crashed or not, so a stale lock never wedges anything.
//!
//! Lock files may be deleted, which needs care: a process that opened the
//! file just before another deleted it would lock a file nobody else can
//! reach, and believe it holds the lock while a third process locks the new
//! file at the same path. Two rules rule this out:
//!
//! 1. a lock file is deleted only by a process holding its lock
//!    ([`FileLock::remove`]), before it releases it. Windows cannot delete a
//!    file that is open without `FILE_SHARE_DELETE`, as lock files are, so
//!    there the holder releases first, and the deletion fails harmlessly if
//!    anyone has opened the file since;
//! 2. after locking, the locker checks that the path still names the file it
//!    locked (device and inode on Unix; on Windows it always does), and
//!    starts over if not.
//!
//! So once a process has locked a file, that file stays at its path until
//! the process deletes it: a missing lock file proves its holder let go.
//!
//! Locks are trusted only on local file systems: on a network file system
//! `flock` may block forever even when asked not to, or hold on one host
//! only. [`remote_file_system`] names such a file system before anything
//! locks there.

use std::fs::{File, OpenOptions, TryLockError};
use std::io;
use std::path::{Path, PathBuf};
use std::time::{Duration, Instant};

/// The first pause of a wait ([`Backoff`]).
const FIRST_POLL: Duration = Duration::from_millis(5);
/// The longest pause of a wait ([`Backoff`]).
const LAST_POLL: Duration = Duration::from_millis(100);
/// How long a wait stays silent before it tells the caller.
pub(crate) const NOTICE_AFTER: Duration = Duration::from_secs(1);

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
    /// No lock file: nobody holds it.
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
    /// needed. `None` while another process holds it; an error when the
    /// file cannot be opened or the file system cannot lock files.
    pub fn try_lock(path: &Path) -> io::Result<Option<Self>> {
        loop {
            match attempt(open(path, true)?, path)? {
                Attempt::Locked(lock) => return Ok(Some(lock)),
                Attempt::Held => return Ok(None),
                Attempt::Stale => continue,
            }
        }
    }

    /// Take the lock at `path`, waiting for whoever holds it. The wait
    /// polls on the async runtime with a [`Backoff`], and runs `waiting`
    /// once, with the path, if the lock is still held after a second.
    pub async fn lock(path: &Path, waiting: impl FnOnce(&Path)) -> io::Result<Self> {
        let start = Instant::now();
        let mut waiting = Some(waiting);
        let mut backoff = Backoff::new();
        loop {
            if let Some(lock) = Self::try_lock(path)? {
                return Ok(lock);
            }
            if start.elapsed() >= NOTICE_AFTER
                && let Some(waiting) = waiting.take()
            {
                waiting(path);
            }
            compio::runtime::time::sleep(backoff.pause()).await;
        }
    }

    /// Whether anyone holds the lock at `path`, never creating the file. A
    /// free lock comes back held by the caller, who may then
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

    /// Delete the lock file and release the lock, as rule 1 demands. A
    /// process waiting on the same path then locks whatever file the path
    /// names, never the deleted one.
    pub fn remove(self) -> io::Result<()> {
        let FileLock { file, path } = self;
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

/// The pauses of a wait: 5 ms, doubling up to 100 ms. Short waits, behind
/// another process's brief use of a lock, end quickly; long ones cost
/// little.
#[derive(Debug)]
pub(crate) struct Backoff {
    next: Duration,
}

impl Backoff {
    pub(crate) fn new() -> Self {
        Backoff { next: FIRST_POLL }
    }

    /// The next pause.
    pub(crate) fn pause(&mut self) -> Duration {
        let pause = self.next;
        self.next = (pause * 2).min(LAST_POLL);
        pause
    }
}

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
/// path still names it (rule 2).
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

/// Whether `path` names the open `file`: always, where an open lock file
/// cannot be deleted.
#[cfg(not(unix))]
fn names(_path: &Path, _file: &File) -> io::Result<bool> {
    Ok(true)
}

/// The kind of network file system `dir` lies on, if it lies on one: on
/// Linux by the `statfs` type, on macOS when `statfs` does not mark it
/// `MNT_LOCAL`, on Windows when its volume is a `DRIVE_REMOTE`. Elsewhere
/// every file system is taken as local. `dir` must exist.
#[cfg(any(target_os = "linux", target_os = "android"))]
pub fn remote_file_system(dir: &Path) -> io::Result<Option<&'static str>> {
    // The kernel's `f_type` is a word whose low 32 bits are the magic.
    Ok(remote_kind(statfs(dir)?.f_type as u32))
}

/// See the Linux version.
#[cfg(target_os = "macos")]
pub fn remote_file_system(dir: &Path) -> io::Result<Option<&'static str>> {
    let stat = statfs(dir)?;
    if stat.f_flags & libc::MNT_LOCAL as u32 != 0 {
        return Ok(None);
    }
    // SAFETY: statfs NUL-terminates the type name inside its buffer.
    let name = unsafe { std::ffi::CStr::from_ptr(stat.f_fstypename.as_ptr()) };
    Ok(Some(match name.to_bytes() {
        b"nfs" => "NFS",
        b"smbfs" => "SMB",
        b"afpfs" => "AFP",
        b"webdav" => "WebDAV",
        _ => "non-local",
    }))
}

/// See the Linux version.
#[cfg(windows)]
pub fn remote_file_system(dir: &Path) -> io::Result<Option<&'static str>> {
    use std::os::windows::ffi::OsStrExt;
    use windows_sys::Win32::Storage::FileSystem::{GetDriveTypeW, GetVolumePathNameW};
    use windows_sys::Win32::System::WindowsProgramming::DRIVE_REMOTE;

    // GetVolumePathNameW also resolves paths that do not exist.
    std::fs::metadata(dir)?;
    let path: Vec<u16> = dir.as_os_str().encode_wide().chain([0]).collect();
    // The volume's root is a prefix of the path, plus a separator.
    let mut volume = vec![0u16; path.len().max(260) + 1];
    let capacity = u32::try_from(volume.len()).map_err(io::Error::other)?;
    // SAFETY: `path` is NUL-terminated, and `capacity` is `volume`'s length.
    if unsafe { GetVolumePathNameW(path.as_ptr(), volume.as_mut_ptr(), capacity) } == 0 {
        return Err(io::Error::last_os_error());
    }
    // SAFETY: GetVolumePathNameW NUL-terminated `volume`.
    let remote = unsafe { GetDriveTypeW(volume.as_ptr()) } == DRIVE_REMOTE;
    Ok(remote.then_some("remote drive"))
}

/// See the Linux version.
#[cfg(not(any(
    target_os = "linux",
    target_os = "android",
    target_os = "macos",
    windows
)))]
pub fn remote_file_system(dir: &Path) -> io::Result<Option<&'static str>> {
    std::fs::metadata(dir).map(|_| None)
}

/// The network file system a Linux `statfs` type names: the magics of
/// `linux/magic.h`, and those the Lustre and GPFS clients report. FUSE is
/// not one: the kernel gives FUSE local `flock` semantics.
#[cfg(any(target_os = "linux", target_os = "android"))]
fn remote_kind(f_type: u32) -> Option<&'static str> {
    Some(match f_type {
        0x6969 => "NFS",
        0x517B => "SMB",
        0xFF53_4D42 => "CIFS",
        0xFE53_4D42 => "SMB2",
        0x5346_414F | 0x6B41_4653 => "AFS",
        0x00C3_6400 => "Ceph",
        0x0BD0_0BD0 => "Lustre",
        0x4750_4653 => "GPFS",
        _ => return None,
    })
}

#[cfg(any(target_os = "linux", target_os = "android", target_os = "macos"))]
fn statfs(dir: &Path) -> io::Result<libc::statfs> {
    use std::os::unix::ffi::OsStrExt;
    let path = std::ffi::CString::new(dir.as_os_str().as_bytes())
        .map_err(|e| io::Error::new(io::ErrorKind::InvalidInput, e))?;
    let mut stat = std::mem::MaybeUninit::<libc::statfs>::uninit();
    // SAFETY: `path` is NUL-terminated and `stat` has room for the result.
    if unsafe { libc::statfs(path.as_ptr(), stat.as_mut_ptr()) } != 0 {
        return Err(io::Error::last_os_error());
    }
    // SAFETY: statfs succeeded, so it filled `stat`.
    Ok(unsafe { stat.assume_init() })
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
        let start = Instant::now();
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
        // The waiter stays silent for a second, then says it waits.
        let waited = rx.recv_timeout(Duration::from_secs(30)).unwrap();
        assert!(start.elapsed() >= NOTICE_AFTER);
        assert_eq!(waited, path);
        std::thread::sleep(3 * LAST_POLL);
        assert!(!waiter.is_finished());
        drop(held);
        waiter.join().unwrap();
        // Once: the callback was consumed by the first notice.
        assert!(rx.try_recv().is_err());
    }

    #[test]
    fn a_short_wait_is_silent() {
        let tmp = tempfile::tempdir().unwrap();
        let path = tmp.path().join("k.lock");
        let held = FileLock::try_lock(&path).unwrap().unwrap();
        let release = std::thread::spawn(move || {
            std::thread::sleep(Duration::from_millis(50));
            drop(held);
        });
        let lock = block_on(FileLock::lock(&path, |_| {
            panic!("a short wait says nothing")
        }));
        drop(lock.unwrap());
        release.join().unwrap();
    }

    #[test]
    fn the_backoff_doubles_up_to_its_ceiling() {
        let mut backoff = Backoff::new();
        let pauses: Vec<Duration> = (0..8).map(|_| backoff.pause()).collect();
        let expected = [5, 10, 20, 40, 80, 100, 100, 100].map(Duration::from_millis);
        assert_eq!(pauses, expected);
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

    #[test]
    fn a_directory_cannot_be_locked() {
        let tmp = tempfile::tempdir().unwrap();
        assert!(FileLock::try_lock(tmp.path()).is_err());
        assert!(FileLock::probe(tmp.path()).is_err());
        assert!(block_on(FileLock::lock(tmp.path(), |_| {})).is_err());
    }

    #[cfg(any(target_os = "linux", target_os = "android"))]
    #[test]
    fn network_file_systems_are_named_and_local_ones_are_not() {
        let network = [
            (0x6969, "NFS"),
            (0x517B, "SMB"),
            (0xFF53_4D42, "CIFS"),
            (0xFE53_4D42, "SMB2"),
            (0x5346_414F, "AFS"),
            (0x6B41_4653, "AFS"),
            (0x00C3_6400, "Ceph"),
            (0x0BD0_0BD0, "Lustre"),
            (0x4750_4653, "GPFS"),
        ];
        for (magic, name) in network {
            assert_eq!(remote_kind(magic), Some(name), "{magic:#x}");
        }
        // ext4, xfs, btrfs, tmpfs, overlay, FUSE.
        for magic in [
            0xEF53,
            0x5846_5342,
            0x9123_683E,
            0x0102_1994,
            0x794C_7630,
            0x6573_5546,
        ] {
            assert_eq!(remote_kind(magic), None, "{magic:#x}");
        }
    }

    #[test]
    fn a_temporary_directory_is_local() {
        let tmp = tempfile::tempdir().unwrap();
        assert_eq!(remote_file_system(tmp.path()).unwrap(), None);
        assert!(remote_file_system(&tmp.path().join("absent")).is_err());
    }
}
