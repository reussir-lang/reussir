//! Making a staged tree durable before its row goes live
//! (docs/design/cache-ledger.md, "Entries").
//!
//! An entry's row is written only after every byte of its directory is on
//! the disk, so no row ever describes a file cut short. Nothing after that
//! needs to be durable: a rename or a commit that a power loss undoes
//! leaves one of the disagreements the ledger repairs.

use std::fs::File;
use std::io;
use std::path::Path;

/// Flush every file and directory under `dir` to the disk, and return the
/// bytes of its files: what the entry costs.
///
/// - Linux and the other Unixes: `fsync` every file and directory,
///   bottom-up.
/// - macOS: a plain `fsync(2)` each, then one `F_FULLFSYNC`, which flushes
///   the drive's cache once for the whole tree; std's `sync_all` would
///   flush it once per file.
/// - Windows: `FlushFileBuffers` on every file, opened for writing.
///   Directories are not flushed: std cannot open them, and NTFS journals
///   their entries.
///
/// Links are not followed, and count nothing.
pub fn durable(dir: &Path) -> io::Result<u64> {
    let size = sync_tree(dir)?;
    #[cfg(target_os = "macos")]
    full_fsync(&File::open(dir)?)?;
    Ok(size)
}

/// Flush the tree at `dir`, its contents before the directory itself.
fn sync_tree(dir: &Path) -> io::Result<u64> {
    let mut size = 0u64;
    for entry in std::fs::read_dir(dir)? {
        let entry = entry?;
        let kind = entry.file_type()?;
        if kind.is_dir() {
            size = size.saturating_add(sync_tree(&entry.path())?);
        } else if kind.is_file() {
            size = size.saturating_add(sync_file(&entry.path())?);
        }
    }
    sync_dir(dir)?;
    Ok(size)
}

/// Flush the file at `path`, and return its size.
fn sync_file(path: &Path) -> io::Result<u64> {
    // Windows flushes only a handle open for writing.
    let file = if cfg!(windows) {
        std::fs::OpenOptions::new().write(true).open(path)?
    } else {
        File::open(path)?
    };
    fsync(&file)?;
    Ok(file.metadata()?.len())
}

#[cfg(not(windows))]
fn sync_dir(dir: &Path) -> io::Result<()> {
    fsync(&File::open(dir)?)
}

#[cfg(windows)]
fn sync_dir(_dir: &Path) -> io::Result<()> {
    Ok(())
}

#[cfg(not(target_os = "macos"))]
fn fsync(file: &File) -> io::Result<()> {
    file.sync_all()
}

/// A plain `fsync(2)`, which on macOS reaches the drive but not past its
/// cache; [`durable`] flushes that once, at the end.
#[cfg(target_os = "macos")]
fn fsync(file: &File) -> io::Result<()> {
    use std::os::fd::AsRawFd;
    // SAFETY: the descriptor is open for the duration of the call.
    if unsafe { libc::fsync(file.as_raw_fd()) } != 0 {
        return Err(io::Error::last_os_error());
    }
    Ok(())
}

/// Ask the drive under `file` to flush its cache.
#[cfg(target_os = "macos")]
fn full_fsync(file: &File) -> io::Result<()> {
    use std::os::fd::AsRawFd;
    // SAFETY: the descriptor is open for the duration of the call.
    if unsafe { libc::fcntl(file.as_raw_fd(), libc::F_FULLFSYNC) } == -1 {
        return Err(io::Error::last_os_error());
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn durable_sums_a_nested_tree_with_empty_directories() {
        let tmp = tempfile::tempdir().unwrap();
        let tree = tmp.path().join("tree");
        std::fs::create_dir_all(tree.join("sub/empty")).unwrap();
        std::fs::create_dir_all(tree.join("also-empty")).unwrap();
        std::fs::write(tree.join("a"), "12345").unwrap();
        std::fs::write(tree.join("sub/b"), "123").unwrap();
        std::fs::write(tree.join("sub/c"), "").unwrap();
        assert_eq!(durable(&tree).unwrap(), 8);
        assert_eq!(durable(&tree.join("also-empty")).unwrap(), 0);
        assert!(durable(&tmp.path().join("absent")).is_err());
    }
}
