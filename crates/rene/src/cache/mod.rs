//! The per-user cache: what every build has in common, kept once per user
//! (docs/design/cache-ledger.md).
//!
//! Some of what a build needs does not depend on the package being built:
//! the sources of the libraries bundled inside rene, the `reussir-rt`
//! runtime baked with the user's toolchain, and those libraries' compiled
//! artifacts. They live under one root per user, which every build on the
//! machine shares:
//!
//! - Linux and the other Unixes: `$XDG_CACHE_HOME/rene`, or `~/.cache/rene`;
//! - macOS: `~/Library/Caches/rene`;
//! - Windows: `%LOCALAPPDATA%\rene`.
//!
//! [`CACHE_DIR_ENV`] names another root. The root is made absolute once,
//! since entry paths end up in build records, and carries a `CACHEDIR.TAG`
//! (<https://bford.info/cachedir/>) so that backup tools skip it.
//!
//! An entry is the directory `<root>/<kind>/<key>`
//! ([`EntryId::dir`](crate::ledger::EntryId::dir)), of one of the [`kind`]s.
//! Its [`key`] hashes everything the entry's contents depend on, so equal
//! keys mean interchangeable contents, and an entry never changes once it
//! exists. Whether a directory is an entry, and which rene wants it, is
//! recorded in the cache's [`ledger`](crate::ledger). Consumers use an
//! entry only through one protocol, [`Cache::acquire`] (see [`entry`]).
//!
//! Opening the cache starts this rene's [`Session`] in it, which ends when
//! the last clone of the [`Cache`] is dropped. A cache that cannot be used
//! safely (a root that cannot be created or written, one on a network file
//! system, a ledger that cannot be used) is an error that names the root
//! and suggests [`CACHE_DIR_ENV`]: there is no weaker mode.

use std::cell::Cell;
use std::ffi::OsString;
use std::path::{Path, PathBuf};
use std::rc::Rc;

pub use crate::ledger::CACHE_DIR_ENV;
use crate::ledger::{Session, unusable};

mod durable;
pub mod entry;

pub use durable::durable;
pub use entry::{Acquired, Request, Slot, Staging, Usage};

/// The cache's directory under the platform cache directory.
const CACHE_NAME: &str = "rene";

const TAG_FILE: &str = "CACHEDIR.TAG";
const TAG: &str = "Signature: 8a477f597d28d172789f06886806bc55\n\
# This file is a cache directory tag created by rene, the Reussir package manager.\n\
# For information about cache directory tags, see https://bford.info/cachedir/\n";

/// The kinds of entry, each a directory directly under the root.
pub mod kind {
    /// The bundled `core` package's manifest and sources.
    pub const CORE_SRC: &str = "core-src";
    /// The bundled `std` package's manifest and sources.
    pub const STD_SRC: &str = "std-src";
    /// `reussir-rt` bakes.
    pub const RT: &str = "rt";
    /// Compiled bundled packages.
    pub const DEPS: &str = "deps";

    /// Every kind: whatever walks the entries walks exactly these.
    pub const ALL: &[&str] = &[CORE_SRC, STD_SRC, RT, DEPS];
}

/// The cache at one root, with this rene's session in it. Clones share the
/// session; rene's runtime is single-threaded, so an `Rc` suffices.
#[derive(Debug, Clone)]
pub struct Cache {
    session: Rc<Session>,
    /// What [`Cache::total_published`] reports, shared by the clones.
    published: Rc<Cell<Option<u64>>>,
}

impl Cache {
    /// Open the cache rooted at `root`, made absolute: create the root and
    /// its `CACHEDIR.TAG` if needed, and start a session in it.
    pub async fn open(root: impl AsRef<Path>) -> Result<Cache, String> {
        let root = root.as_ref();
        let root = std::path::absolute(root)
            .map_err(|e| unusable(root, format_args!("cannot resolve it: {e}")))?;
        std::fs::create_dir_all(&root)
            .map_err(|e| unusable(&root, format_args!("cannot create it: {e}")))?;
        let tag = root.join(TAG_FILE);
        // Rewritten whole whenever it is not exactly the tag, so a write cut
        // short heals; concurrent writers write the same bytes.
        if std::fs::read(&tag).ok().as_deref() != Some(TAG.as_bytes()) {
            std::fs::write(&tag, TAG)
                .map_err(|e| unusable(&root, format_args!("cannot write `{TAG_FILE}`: {e}")))?;
        }
        let session = Session::start(&root).await?;
        Ok(Cache {
            session: Rc::new(session),
            published: Rc::default(),
        })
    }

    /// Open the user's cache: [`CACHE_DIR_ENV`], else the platform cache
    /// directory's `rene`.
    pub async fn open_user() -> Result<Cache, String> {
        Cache::open(user_root(std::env::var_os(CACHE_DIR_ENV))?).await
    }

    /// The root, always absolute.
    pub fn root(&self) -> &Path {
        self.session.root()
    }

    /// This rene's session in the cache.
    pub fn session(&self) -> &Session {
        &self.session
    }
}

/// The user's cache root, given [`CACHE_DIR_ENV`]'s value: that directory
/// unless it is empty, else the platform cache directory's `rene`.
fn user_root(env: Option<OsString>) -> Result<PathBuf, String> {
    match env {
        Some(dir) if !dir.is_empty() => Ok(PathBuf::from(dir)),
        _ => dirs::cache_dir()
            .map(|dir| dir.join(CACHE_NAME))
            .ok_or_else(|| {
                format!(
                    "cannot find this user's cache directory; set {CACHE_DIR_ENV} to a \
                     directory on a local file system that rene can write"
                )
            }),
    }
}

/// An entry key: 32 hex digits of a blake3 hash over `parts`, each prefixed
/// with its length, so `["ab", "c"]` and `["a", "bc"]` differ.
pub fn key<'a>(parts: impl IntoIterator<Item = &'a [u8]>) -> String {
    let mut hasher = blake3::Hasher::new();
    for part in parts {
        hasher.update(&(part.len() as u64).to_le_bytes());
        hasher.update(part);
    }
    hasher.finalize().to_hex()[..32].to_owned()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ledger::EntryId;

    fn block_on<T>(fut: impl Future<Output = T>) -> T {
        compio::runtime::Runtime::new().unwrap().block_on(fut)
    }

    #[test]
    fn the_root_is_tagged_and_entry_paths_are_pure() {
        let tmp = tempfile::tempdir().unwrap();
        let cache = block_on(Cache::open(tmp.path().join("c"))).unwrap();
        let tag = cache.root().join(TAG_FILE);
        assert!(
            std::fs::read_to_string(&tag)
                .unwrap()
                .starts_with("Signature: 8a477f597d28d172789f06886806bc55\n")
        );
        let id = EntryId {
            kind: kind::RT,
            key: key([b"rt".as_slice()]),
        };
        let dir = id.dir(cache.root());
        assert_eq!(dir, cache.root().join("rt").join(&id.key));
        assert!(!dir.parent().unwrap().exists());
        assert_eq!(EntryId::parse(&id.to_string(), kind::ALL), Some(id));

        // A tag cut short is written again by the next rene.
        std::fs::write(&tag, "Signature").unwrap();
        let again = block_on(Cache::open(cache.root())).unwrap();
        assert_eq!(std::fs::read_to_string(&tag).unwrap(), TAG);
        assert_ne!(again.session().id(), cache.session().id());
    }

    /// A relative root is resolved against the working directory once, so
    /// no recorded path depends on where a later rene runs.
    #[cfg(unix)]
    #[test]
    fn a_relative_root_is_made_absolute() {
        let tmp = tempfile::tempdir().unwrap();
        let cwd = std::env::current_dir().unwrap();
        let mut relative: PathBuf = cwd.components().skip(1).map(|_| "..").collect();
        relative.push(tmp.path().strip_prefix("/").unwrap());
        relative.push("c");
        assert!(relative.is_relative());
        let cache = block_on(Cache::open(&relative)).unwrap();
        assert!(cache.root().is_absolute(), "{}", cache.root().display());
        assert!(tmp.path().join("c").join(TAG_FILE).is_file());
    }

    #[test]
    fn the_override_wins_and_an_empty_one_is_ignored() {
        assert_eq!(
            user_root(Some("elsewhere".into())).unwrap(),
            Path::new("elsewhere")
        );
        if let Some(platform) = dirs::cache_dir() {
            assert_eq!(user_root(Some("".into())).unwrap(), platform.join("rene"));
            assert_eq!(user_root(None).unwrap(), platform.join("rene"));
        }
    }

    #[test]
    fn an_unusable_root_fails_naming_it() {
        let tmp = tempfile::tempdir().unwrap();
        let file = tmp.path().join("file");
        std::fs::write(&file, "").unwrap();
        for root in [file.clone(), file.join("c")] {
            let err = block_on(Cache::open(&root)).unwrap_err();
            assert!(err.contains(&format!("`{}`", root.display())), "{err}");
            assert!(err.contains(CACHE_DIR_ENV), "{err}");
        }
    }

    #[test]
    fn keys_separate_their_components() {
        let ab_c = key([b"ab".as_slice(), b"c"]);
        let a_bc = key([b"a".as_slice(), b"bc"]);
        assert_ne!(ab_c, a_bc);
        assert_eq!(ab_c.len(), 32);
        assert!(ab_c.bytes().all(|b| b.is_ascii_hexdigit()));
        assert_eq!(ab_c, key([b"ab".as_slice(), b"c"]));
    }
}
