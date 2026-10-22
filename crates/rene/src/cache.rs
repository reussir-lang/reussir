//! The shared per-user cache for the dependencies every build has in common.
//!
//! The bundled `core` sources, the baked `reussir-rt` runtime, and the
//! compiled artifacts of bundled packages do not depend on the package being
//! built. They used to be materialized again in every build directory, so
//! each new project paid for a full cargo bake. They now live once per user,
//! under the platform cache directory (docs/design/system-rust-runtime.md
//! §4):
//!
//! - Linux and other Unix: `$XDG_CACHE_HOME/rene`, or `~/.cache/rene`;
//! - macOS: `~/Library/Caches/rene`;
//! - Windows: `%LOCALAPPDATA%\rene`.
//!
//! [`CACHE_DIR_ENV`] overrides the location, for hermetic builds and tests.
//!
//! # Layout and invariants
//!
//! Every entry is a directory `<root>/<kind>/<key>`, where the key hashes
//! everything the entry's contents depend on (content, target, toolchain,
//! …). An entry is therefore never updated in place: different inputs get a
//! different directory, and a published entry is immutable.
//!
//! - **Publication is atomic.** An entry is built in a private temporary
//!   sibling and renamed into place, or (for the runtime bake, whose cargo
//!   tree cannot move) completed by atomically renaming a marker file into
//!   it. A reader that finds the entry or its marker sees the whole thing.
//! - **Writers hold an advisory lock.** Before building an entry, a process
//!   takes an exclusive lock on `<root>/<kind>/<key>.lock`, then checks again
//!   whether someone else finished the entry while it waited. So concurrent
//!   builds wait for one bake instead of all running it. The lock is the
//!   operating system's file lock (`flock`/`LockFileEx`), so it is released
//!   when the holder exits, even when it crashes; a stale lock cannot wedge
//!   the cache. Lock files are never deleted, so two processes cannot end up
//!   locking different files of the same name.
//! - **Readers take no lock.** A complete entry is immutable, so reading it
//!   needs no coordination.
//! - **Failure degrades, it does not stop the build.** If the cache
//!   directory cannot be created, the cache moves into the build directory
//!   (where the build-directory lock already serializes writers), with a
//!   warning. If the file system cannot lock files, the entry is built
//!   without the lock, with a warning; atomic publication still keeps the
//!   result consistent, at the cost of duplicate work.
//!
//! The root carries a `CACHEDIR.TAG` (<https://bford.info/cachedir/>), so
//! backup tools skip it. Nothing removes old entries yet; deleting the
//! whole directory is always safe while no rene runs.

use std::fs::{File, OpenOptions, TryLockError};
use std::io::Write;
use std::path::{Path, PathBuf};
use std::time::{Duration, UNIX_EPOCH};

/// Overrides the cache location.
pub const CACHE_DIR_ENV: &str = "RENE_CACHE_DIR";

/// The cache directory's name under the platform cache directory.
const CACHE_NAME: &str = "rene";

/// The cache's name inside the build directory, when the shared location is
/// unusable.
const LOCAL_CACHE: &str = "cache";

/// How often a waiting process retries a held lock.
const LOCK_POLL: Duration = Duration::from_millis(100);

const CACHEDIR_TAG: &str = "Signature: 8a477f597d28d172789f06886806bc55\n\
# This file is a cache directory tag created by rene, the Reussir package manager.\n\
# For information about cache directory tags, see https://bford.info/cachedir/\n";

/// An open cache root.
#[derive(Debug, Clone)]
pub struct Cache {
    root: PathBuf,
}

impl Cache {
    /// Use `root` as the cache, creating it (and its `CACHEDIR.TAG`) if
    /// needed.
    pub fn at(root: impl Into<PathBuf>) -> Result<Self, String> {
        let root = root.into();
        std::fs::create_dir_all(&root)
            .map_err(|e| format!("cannot create the cache `{}`: {e}", root.display()))?;
        let tag = root.join("CACHEDIR.TAG");
        if !tag.exists() {
            // The tag is advisory, and racing writers write the same bytes.
            write_atomically(&tag, CACHEDIR_TAG.as_bytes())?;
        }
        Ok(Cache { root })
    }

    /// The shared cache: [`CACHE_DIR_ENV`], else the platform cache
    /// directory.
    pub fn shared() -> Result<Self, String> {
        let root = match std::env::var_os(CACHE_DIR_ENV) {
            Some(dir) if !dir.is_empty() => PathBuf::from(dir),
            _ => dirs::cache_dir()
                .map(|dir| dir.join(CACHE_NAME))
                .ok_or_else(|| format!("no user cache directory (set {CACHE_DIR_ENV})"))?,
        };
        Self::at(root)
    }

    /// The cache for a build in `build_dir`: the [`shared`](Self::shared)
    /// cache, else (with a warning) a cache inside the build directory.
    pub fn locate(build_dir: &Path) -> Self {
        Self::shared().unwrap_or_else(|e| {
            let local = build_dir.join(LOCAL_CACHE);
            tracing::warn!(
                "{e}; caching shared dependencies in `{}` instead",
                local.display()
            );
            Cache { root: local }
        })
    }

    pub fn root(&self) -> &Path {
        &self.root
    }

    /// The entry directory `<root>/<kind>/<key>`. Pure: nothing is created.
    pub fn entry(&self, kind: &str, key: &str) -> PathBuf {
        self.root.join(kind).join(key)
    }

    /// Take the exclusive lock guarding the entry `<kind>/<key>`, waiting
    /// for another process to release it if needed. `what` describes the
    /// work for the message shown while waiting (e.g. "baking reussir-rt");
    /// `notify` shows it, so callers with live progress bars can print
    /// around them.
    ///
    /// The returned guard releases the lock when dropped. If the file system
    /// cannot lock files, this warns and returns an unlocked guard.
    pub async fn lock(
        &self,
        kind: &str,
        key: &str,
        what: &str,
        notify: impl FnOnce(&str),
    ) -> Result<EntryLock, String> {
        let dir = self.root.join(kind);
        std::fs::create_dir_all(&dir)
            .map_err(|e| format!("cannot create `{}`: {e}", dir.display()))?;
        let path = dir.join(format!("{key}.lock"));
        let file = OpenOptions::new()
            .create(true)
            .truncate(false)
            .write(true)
            .open(&path)
            .map_err(|e| format!("cannot open the lock `{}`: {e}", path.display()))?;
        let mut notify = Some(notify);
        loop {
            match file.try_lock() {
                Ok(()) => return Ok(EntryLock { _file: Some(file) }),
                Err(TryLockError::WouldBlock) => {
                    if let Some(notify) = notify.take() {
                        notify(&format!(
                            "waiting for another rene to finish {what} (lock `{}`)",
                            path.display()
                        ));
                    }
                    compio::runtime::time::sleep(LOCK_POLL).await;
                }
                Err(TryLockError::Error(e)) => {
                    tracing::warn!(
                        "cannot lock `{}` ({e}); {what} without the lock",
                        path.display()
                    );
                    return Ok(EntryLock { _file: None });
                }
            }
        }
    }
}

/// A held entry lock. Dropping it closes the file, which releases the lock.
#[must_use = "the lock is released when the guard is dropped"]
pub struct EntryLock {
    _file: Option<File>,
}

/// A fresh temporary path next to `path`, unique to this process and call,
/// for building something that is then renamed into place.
pub fn temp_sibling(path: &Path) -> PathBuf {
    use std::sync::atomic::{AtomicU64, Ordering};
    static NEXT: AtomicU64 = AtomicU64::new(0);
    let mut name = path.file_name().unwrap_or_default().to_os_string();
    name.push(format!(
        ".tmp-{}-{}",
        std::process::id(),
        NEXT.fetch_add(1, Ordering::Relaxed)
    ));
    path.with_file_name(name)
}

/// Write `bytes` to `path` atomically: readers see either the old file or
/// the whole new one.
pub fn write_atomically(path: &Path, bytes: &[u8]) -> Result<(), String> {
    let tmp = temp_sibling(path);
    let result = (|| {
        let mut file = File::create(&tmp)?;
        file.write_all(bytes)?;
        file.sync_all()?;
        std::fs::rename(&tmp, path)
    })();
    result.map_err(|e| {
        let _ = std::fs::remove_file(&tmp);
        format!("cannot write `{}`: {e}", path.display())
    })
}

/// Move the finished directory `tmp` to `dest`. If another process
/// published `dest` first, keep theirs: entries with equal keys are
/// interchangeable. Either way `tmp` is gone afterwards.
pub fn publish_dir(tmp: &Path, dest: &Path) -> Result<(), String> {
    match std::fs::rename(tmp, dest) {
        Ok(()) => Ok(()),
        Err(_) if dest.is_dir() => {
            let _ = std::fs::remove_dir_all(tmp);
            Ok(())
        }
        Err(e) => {
            let _ = std::fs::remove_dir_all(tmp);
            Err(format!("cannot publish `{}`: {e}", dest.display()))
        }
    }
}

/// Hash cache-key components into an entry key (32 hex digits). Every
/// component is length-prefixed, so `["ab", "c"]` and `["a", "bc"]` differ.
pub fn key<'a>(parts: impl IntoIterator<Item = &'a [u8]>) -> String {
    let mut hasher = blake3::Hasher::new();
    for part in parts {
        hasher.update(&(part.len() as u64).to_le_bytes());
        hasher.update(part);
    }
    hasher.finalize().to_hex()[..32].to_string()
}

/// A cheap identity of the program `program` would run: its resolved path
/// (through `PATH` for a bare name), size, and modification time. A rebuilt
/// or replaced tool changes it, so cache keys that include it miss instead
/// of serving artifacts the old tool produced.
pub fn program_identity(program: &Path) -> String {
    let resolved = resolve_program(program).unwrap_or_else(|| program.to_owned());
    let resolved = resolved.canonicalize().unwrap_or(resolved);
    match std::fs::metadata(&resolved) {
        Ok(meta) => {
            let mtime = meta
                .modified()
                .ok()
                .and_then(|t| t.duration_since(UNIX_EPOCH).ok())
                .map_or(0, |d| d.as_nanos());
            format!("{} {} {mtime}", resolved.display(), meta.len())
        }
        Err(_) => format!("{} <missing>", resolved.display()),
    }
}

/// Where `program` resolves: itself when it names a path, else the first
/// match on `PATH` (with `.exe` on Windows).
fn resolve_program(program: &Path) -> Option<PathBuf> {
    if program.components().count() > 1 {
        return Some(program.to_owned());
    }
    let path = std::env::var_os("PATH")?;
    std::env::split_paths(&path).find_map(|dir| {
        let candidate = dir.join(program);
        if candidate.is_file() {
            return Some(candidate);
        }
        if cfg!(windows) {
            let exe = candidate.with_extension("exe");
            if exe.is_file() {
                return Some(exe);
            }
        }
        None
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    fn block_on<T>(fut: impl Future<Output = T>) -> T {
        compio::runtime::Runtime::new().unwrap().block_on(fut)
    }

    #[test]
    fn the_root_is_tagged_and_entries_are_pure_paths() {
        let tmp = tempfile::tempdir().unwrap();
        let cache = Cache::at(tmp.path().join("c")).unwrap();
        let tag = std::fs::read_to_string(cache.root().join("CACHEDIR.TAG")).unwrap();
        assert!(tag.starts_with("Signature: 8a477f597d28d172789f06886806bc55"));
        let entry = cache.entry("rt", "abc");
        assert_eq!(entry, cache.root().join("rt").join("abc"));
        assert!(!entry.exists());
    }

    #[test]
    fn an_unusable_shared_root_is_an_error() {
        let tmp = tempfile::tempdir().unwrap();
        let file = tmp.path().join("file");
        std::fs::write(&file, "").unwrap();
        assert!(Cache::at(file.join("c")).is_err());
    }

    #[test]
    fn keys_separate_their_components() {
        let ab_c = key([b"ab".as_slice(), b"c"]);
        let a_bc = key([b"a".as_slice(), b"bc"]);
        assert_ne!(ab_c, a_bc);
        assert_eq!(ab_c.len(), 32);
        assert_eq!(ab_c, key([b"ab".as_slice(), b"c"]));
    }

    #[test]
    fn a_held_lock_makes_the_next_locker_wait() {
        let tmp = tempfile::tempdir().unwrap();
        let cache = Cache::at(tmp.path()).unwrap();
        let held = block_on(cache.lock("k", "e", "testing", |_| {})).unwrap();

        let (tx, rx) = std::sync::mpsc::channel();
        let waiter = {
            let cache = cache.clone();
            std::thread::spawn(move || {
                let notified = std::sync::Mutex::new(None);
                let lock = block_on(cache.lock("k", "e", "testing", |msg| {
                    *notified.lock().unwrap() = Some(msg.to_owned());
                    tx.send(()).unwrap();
                }))
                .unwrap();
                drop(lock);
                notified.into_inner().unwrap()
            })
        };
        // The waiter announces that it waits, and only then is released.
        rx.recv_timeout(Duration::from_secs(30)).unwrap();
        assert!(!waiter.is_finished());
        drop(held);
        let message = waiter.join().unwrap().unwrap();
        assert!(message.contains("waiting for another rene to finish testing"));
        assert!(message.contains("e.lock"));
    }

    #[test]
    fn publishing_keeps_the_first_entry() {
        let tmp = tempfile::tempdir().unwrap();
        let dest = tmp.path().join("entry");
        for content in ["first", "second"] {
            let staging = temp_sibling(&dest);
            std::fs::create_dir(&staging).unwrap();
            std::fs::write(staging.join("f"), content).unwrap();
            publish_dir(&staging, &dest).unwrap();
            assert!(!staging.exists());
        }
        assert_eq!(std::fs::read_to_string(dest.join("f")).unwrap(), "first");
    }

    #[test]
    fn program_identity_tracks_the_resolved_file() {
        let tmp = tempfile::tempdir().unwrap();
        let tool = tmp.path().join("tool");
        std::fs::write(&tool, "v1").unwrap();
        let before = program_identity(&tool);
        std::fs::write(&tool, "version 2").unwrap();
        assert_ne!(before, program_identity(&tool));
        assert!(program_identity(&tmp.path().join("absent")).ends_with("<missing>"));
    }
}
