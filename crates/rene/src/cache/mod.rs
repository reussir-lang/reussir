//! The shared per-user cache for what every build has in common.
//!
//! Some of what a build needs does not depend on the package being built:
//! the sources of the libraries bundled inside rene, the `reussir-rt`
//! runtime baked with the user's toolchain, and those libraries' compiled
//! artifacts. Materializing them again in every build directory makes each
//! new project pay for them again, so they belong once per user, under the
//! platform cache directory (docs/design/system-rust-runtime.md §4):
//!
//! - Linux and other Unix: `$XDG_CACHE_HOME/rene`, or `~/.cache/rene`;
//! - macOS: `~/Library/Caches/rene`;
//! - Windows: `%LOCALAPPDATA%\rene`.
//!
//! [`CACHE_DIR_ENV`] overrides the location, for hermetic builds and tests.
//! A root that cannot be used moves the cache into the build directory,
//! with a warning ([`Cache::locate`]); the build directory's own lock then
//! serializes its writers. A root counts as unusable when it cannot be
//! created or when its file system cannot lock files: there is no unlocked
//! mode, since an entry built in place by two processes at once is
//! corrupted. The root carries a `CACHEDIR.TAG`
//! (<https://bford.info/cachedir/>), so backup tools skip it, and
//! `rene.lock`, the file that probe locks.
//!
//! # Layout
//!
//! An entry is the directory `<root>/<kind>/<key>` ([`EntryId`]). Its key
//! hashes everything the entry's contents depend on ([`key`]): content,
//! target, toolchain, and so on. Different inputs get a different directory,
//! so a published entry is never changed in place, only built whole and,
//! when stale or incomplete, replaced whole. Beside an entry live only names
//! derived from its key: the lock file guarding it, `<key>.lock`
//! ([`Cache::lock_path`]), and the unique siblings of [`sibling`],
//! `<key>.tmp-<pid>-<n>` for a copy being staged and `<key>.rm-<pid>-<n>`
//! for one being deleted. [`parse_sibling`] recognizes the latter two, so
//! whatever cleans up after a crashed process never has to guess.
//!
//! Consumers use entries through one protocol, [`Cache::acquire`] (see
//! [`entry`]), and every lock the cache takes is a [`FileLock`] ([`lock`]).

pub mod entry;
pub mod lock;

use std::ffi::OsString;
use std::fs::File;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicU64, Ordering};
use std::time::UNIX_EPOCH;

pub use entry::{Acquired, RecordedFile, Request, Slot, Staging, Usage};
pub use lock::{FileLock, Probe};

/// Overrides the cache location.
pub const CACHE_DIR_ENV: &str = "RENE_CACHE_DIR";

/// The cache directory's name under the platform cache directory.
const CACHE_NAME: &str = "rene";

/// The cache's name inside the build directory, when the shared location is
/// unusable.
const LOCAL_CACHE: &str = "cache";

/// The root's file [`Cache::at`] locks to probe whether locking works.
const LOCK_PROBE: &str = "rene.lock";

const CACHEDIR_TAG: &str = "Signature: 8a477f597d28d172789f06886806bc55\n\
# This file is a cache directory tag created by rene, the Reussir package manager.\n\
# For information about cache directory tags, see https://bford.info/cachedir/\n";

/// An open cache root.
#[derive(Debug, Clone)]
pub struct Cache {
    /// Always absolute: entry paths end up in build records, which must not
    /// depend on the directory rene happened to run in.
    root: PathBuf,
}

/// The kinds of entry, each a directory directly under the root. Besides
/// them the root holds only `CACHEDIR.TAG`, `rene.lock`, and the cache
/// [`ledger`](crate::ledger)'s `ledger/` and `sessions/`, so whatever walks
/// the entries walks exactly [`kind::ALL`].
pub mod kind {
    /// The bundled `core` package's sources ([`crate::bundled`]).
    pub const CORE_SRC: &str = "core-src";
    /// The bundled `std` package's sources ([`crate::bundled`]).
    pub const STD_SRC: &str = "std-src";
    /// `reussir-rt` bakes ([`crate::rt`]).
    pub const RT: &str = "rt";
    /// Compiled bundled packages ([`crate::shared`]).
    pub const DEPS: &str = "deps";

    /// Every kind.
    pub const ALL: &[&str] = &[CORE_SRC, STD_SRC, RT, DEPS];
}

/// One entry: the directory `<root>/<kind>/<key>`.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct EntryId {
    /// The sort of entry ([`kind`]), and the directory under the root
    /// holding every entry of that sort.
    pub kind: &'static str,
    /// Hashes the entry's inputs ([`key`]); a directory name.
    pub key: String,
}

impl EntryId {
    pub fn new(kind: &'static str, key: impl Into<String>) -> Self {
        EntryId {
            kind,
            key: key.into(),
        }
    }
}

impl std::fmt::Display for EntryId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}/{}", self.kind, self.key)
    }
}

impl Cache {
    /// Use `root`, made absolute, as the cache, creating it (and its
    /// `CACHEDIR.TAG`) if needed. A root whose file system cannot lock
    /// files is an error.
    pub fn at(root: impl AsRef<Path>) -> Result<Self, String> {
        let root = root.as_ref();
        let root = std::path::absolute(root)
            .map_err(|e| format!("cannot resolve the cache `{}`: {e}", root.display()))?;
        std::fs::create_dir_all(&root)
            .map_err(|e| format!("cannot create the cache `{}`: {e}", root.display()))?;
        let tag = root.join("CACHEDIR.TAG");
        if !tag.exists() {
            // The tag is advisory, and racing writers write the same bytes.
            write_atomically(&tag, CACHEDIR_TAG.as_bytes())?;
        }
        // Held or free, the probe only asks whether locking works at all.
        let probe = root.join(LOCK_PROBE);
        FileLock::try_lock(&probe).map_err(|e| {
            format!(
                "the cache `{}` cannot lock files (`{}`: {e})",
                root.display(),
                probe.display()
            )
        })?;
        Ok(Cache { root })
    }

    /// The shared cache: [`CACHE_DIR_ENV`], else the platform cache
    /// directory.
    pub fn shared() -> Result<Self, String> {
        Self::at(shared_root(std::env::var_os(CACHE_DIR_ENV))?)
    }

    /// The cache for a build in `build_dir`: the [`shared`](Self::shared)
    /// cache, else (with a warning) a cache inside the build directory.
    pub fn locate(build_dir: &Path) -> Result<Self, String> {
        Self::shared().or_else(|e| {
            let local = build_dir.join(LOCAL_CACHE);
            tracing::warn!(
                "{e}; caching shared dependencies in `{}` instead",
                local.display()
            );
            Self::at(local)
        })
    }

    pub fn root(&self) -> &Path {
        &self.root
    }

    /// The entry's directory. Pure: nothing is created, so callers can
    /// report where an entry lives without writing anything.
    pub fn entry(&self, id: &EntryId) -> PathBuf {
        self.root.join(id.kind).join(&id.key)
    }

    /// The lock file guarding the entry, `<kind>/<key>.lock`. Pure.
    pub fn lock_path(&self, id: &EntryId) -> PathBuf {
        self.root.join(id.kind).join(format!("{}.lock", id.key))
    }
}

/// The shared root, given [`CACHE_DIR_ENV`]'s value: that directory, else
/// the platform cache directory's `rene`.
fn shared_root(env: Option<OsString>) -> Result<PathBuf, String> {
    match env {
        Some(dir) if !dir.is_empty() => Ok(PathBuf::from(dir)),
        _ => dirs::cache_dir()
            .map(|dir| dir.join(CACHE_NAME))
            .ok_or_else(|| format!("no user cache directory (set {CACHE_DIR_ENV})")),
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

/// What a [`sibling`] is for.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Tag {
    /// A private copy being built, renamed into place once complete.
    Tmp,
    /// A directory moved out of the way, being deleted.
    Trash,
}

impl Tag {
    fn as_str(self) -> &'static str {
        match self {
            Tag::Tmp => "tmp",
            Tag::Trash => "rm",
        }
    }
}

/// A fresh path next to `path`, unique to this process and call:
/// `<name>.tmp-<pid>-<n>` or `<name>.rm-<pid>-<n>`.
pub fn sibling(path: &Path, tag: Tag) -> PathBuf {
    static NEXT: AtomicU64 = AtomicU64::new(0);
    let mut name = path.file_name().unwrap_or_default().to_os_string();
    name.push(format!(
        ".{}-{}-{}",
        tag.as_str(),
        std::process::id(),
        NEXT.fetch_add(1, Ordering::Relaxed)
    ));
    path.with_file_name(name)
}

/// The name a [`sibling`] was derived from, and its tag; `None` for any
/// other file name.
pub fn parse_sibling(name: &str) -> Option<(&str, Tag)> {
    let (base, suffix) = name.rsplit_once('.')?;
    let (tag, unique) = suffix.split_once('-')?;
    let tag = match tag {
        "tmp" => Tag::Tmp,
        "rm" => Tag::Trash,
        _ => return None,
    };
    let (pid, n) = unique.split_once('-')?;
    let numbered = pid.parse::<u32>().is_ok() && n.parse::<u64>().is_ok();
    (numbered && !base.is_empty()).then_some((base, tag))
}

/// Write `bytes` to `path` atomically: readers see either the old file or
/// the whole new one.
pub fn write_atomically(path: &Path, bytes: &[u8]) -> Result<(), String> {
    let tmp = sibling(path, Tag::Tmp);
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

    #[test]
    fn the_root_is_tagged_and_entries_are_pure_paths() {
        let tmp = tempfile::tempdir().unwrap();
        let cache = Cache::at(tmp.path().join("c")).unwrap();
        let tag = std::fs::read_to_string(cache.root().join("CACHEDIR.TAG")).unwrap();
        assert!(tag.starts_with("Signature: 8a477f597d28d172789f06886806bc55"));
        let id = EntryId::new("rt", "abc");
        assert_eq!(id.to_string(), "rt/abc");
        let entry = cache.entry(&id);
        assert_eq!(entry, cache.root().join("rt").join("abc"));
        assert_eq!(
            cache.lock_path(&id),
            cache.root().join("rt").join("abc.lock")
        );
        assert!(!entry.exists() && !entry.parent().unwrap().exists());
    }

    #[test]
    fn an_unusable_root_is_an_error() {
        let tmp = tempfile::tempdir().unwrap();
        let file = tmp.path().join("file");
        std::fs::write(&file, "").unwrap();
        assert!(Cache::at(file.join("c")).is_err());
    }

    /// A root where the lock probe fails is unusable, so the build falls
    /// back to its own directory instead of building entries unlocked.
    #[test]
    fn a_root_that_cannot_lock_is_unusable() {
        let tmp = tempfile::tempdir().unwrap();
        let root = tmp.path().join("c");
        Cache::at(&root).unwrap();
        assert!(root.join(LOCK_PROBE).is_file());
        // A directory where the probe file belongs cannot be locked.
        let broken = tmp.path().join("broken");
        std::fs::create_dir_all(broken.join(LOCK_PROBE)).unwrap();
        let err = Cache::at(&broken).unwrap_err();
        assert!(err.contains("cannot lock files"), "{err}");
    }

    /// A relative root (a relative `RENE_CACHE_DIR`, or a relative build
    /// directory's fallback) is resolved against the working directory once,
    /// so no recorded path depends on where a later rene runs.
    #[cfg(unix)]
    #[test]
    fn a_relative_root_is_made_absolute() {
        let tmp = tempfile::tempdir().unwrap();
        let cwd = std::env::current_dir().unwrap();
        let mut relative: PathBuf = cwd.components().skip(1).map(|_| "..").collect();
        relative.push(tmp.path().strip_prefix("/").unwrap());
        relative.push("c");
        assert!(relative.is_relative());
        let cache = Cache::at(&relative).unwrap();
        assert!(cache.root().is_absolute(), "{}", cache.root().display());
        assert!(tmp.path().join("c/CACHEDIR.TAG").is_file());
    }

    #[test]
    fn the_override_replaces_the_platform_cache_directory() {
        assert_eq!(
            shared_root(Some("elsewhere".into())).unwrap(),
            Path::new("elsewhere")
        );
        // An empty override is no override.
        if let Some(platform) = dirs::cache_dir() {
            assert_eq!(shared_root(Some("".into())).unwrap(), platform.join("rene"));
            assert_eq!(shared_root(None).unwrap(), platform.join("rene"));
        }
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
    fn siblings_are_unique_and_recognized() {
        let entry = Path::new("/cache/rt/0123abcd");
        let first = sibling(entry, Tag::Tmp);
        let second = sibling(entry, Tag::Tmp);
        let trash = sibling(entry, Tag::Trash);
        assert_ne!(first, second);
        for (path, tag) in [
            (&first, Tag::Tmp),
            (&second, Tag::Tmp),
            (&trash, Tag::Trash),
        ] {
            assert_eq!(path.parent(), entry.parent());
            let name = path.file_name().unwrap().to_str().unwrap();
            assert_eq!(parse_sibling(name), Some(("0123abcd", tag)), "{name}");
        }
        let record = sibling(Path::new("artifacts.json"), Tag::Tmp);
        assert_eq!(
            parse_sibling(record.to_str().unwrap()),
            Some(("artifacts.json", Tag::Tmp))
        );
        for name in [
            "0123abcd",
            "0123abcd.lock",
            "0123abcd.tmp-1",
            "0123abcd.tmp-x-1",
            "0123abcd.old-1-2",
            ".tmp-1-2",
        ] {
            assert_eq!(parse_sibling(name), None, "{name}");
        }
    }

    #[test]
    fn publishing_keeps_the_first_entry() {
        let tmp = tempfile::tempdir().unwrap();
        let dest = tmp.path().join("entry");
        for content in ["first", "second"] {
            let staging = sibling(&dest, Tag::Tmp);
            std::fs::create_dir(&staging).unwrap();
            write_atomically(&staging.join("f"), content.as_bytes()).unwrap();
            publish_dir(&staging, &dest).unwrap();
            assert!(!staging.exists());
        }
        assert_eq!(std::fs::read_to_string(dest.join("f")).unwrap(), "first");
        // The atomic write left nothing but the file behind.
        assert_eq!(std::fs::read_dir(&dest).unwrap().count(), 1);
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
