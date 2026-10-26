//! The library packages bundled inside rene: `core` and `std`.
//!
//! Both are written in Reussir and compiled like any other dependency, but
//! shipped inside rene, like the `reussir-rt` bundle: a zstd-compressed tar
//! of each package's manifest and `src/` tree, packed by `build.rs` in
//! sorted order with a fixed mtime, so the bytes, the unpacked files, and
//! the source graphs recorded against them depend only on the sources.
//! `core` is injected into every package's dependency graph unless the
//! package sets `package.no_core`.
//!
//! This table is also the list of reserved package names: `rrc --builtin`
//! lifts the reservation for exactly these packages' builds ([`find`]).
//!
//! A bundle's sources unpack once into the per-user [`Cache`], keyed by the
//! bundle's content, and every build reads them in place. A rene upgrade
//! unpacks a new entry, and the per-package freshness checks see the
//! change: a dependency whose recorded sources no longer lie under its
//! directory is rebuilt.

use std::path::PathBuf;

use crate::cache::{self, Acquired, Cache, Request, Usage, kind};
use crate::ledger::EntryId;
use crate::manifest::Loaded;
use crate::resolve::{self, Graph};

/// One bundled package.
#[derive(Debug)]
pub struct Bundle {
    /// The package's name, which dependents reference it by.
    pub name: &'static str,
    /// The kind of the cache entries holding its sources.
    kind: &'static str,
    /// The package's manifest and sources (tar.zst), packed by `build.rs`.
    bytes: &'static [u8],
}

/// The bundled `core`.
pub static CORE: Bundle = Bundle {
    name: "core",
    kind: kind::CORE_SRC,
    bytes: include_bytes!(concat!(env!("OUT_DIR"), "/core-src.tar.zst")),
};

/// The bundled `std`.
pub static STD: Bundle = Bundle {
    name: "std",
    kind: kind::STD_SRC,
    bytes: include_bytes!(concat!(env!("OUT_DIR"), "/std-src.tar.zst")),
};

/// Every bundle.
pub static ALL: [&Bundle; 2] = [&CORE, &STD];

/// The bundle named `name`, if rene ships one.
pub fn find(name: &str) -> Option<&'static Bundle> {
    ALL.iter().copied().find(|bundle| bundle.name == name)
}

/// Load the dependency graph rooted at `root`, with the bundled `core`
/// unpacked into `cache`.
pub async fn load_graph(cache: &Cache, root: &Loaded) -> Result<Graph, String> {
    let core = CORE.unpack(cache).await?;
    resolve::load_graph_with(root, Some(&core))
}

impl Bundle {
    /// The bundle's content hash, which keys its sources' cache entry.
    pub fn content_hash(&self) -> String {
        cache::key([self.bytes])
    }

    fn id(&self) -> EntryId {
        EntryId {
            kind: self.kind,
            key: self.content_hash(),
        }
    }

    /// The directory of the bundle's sources in `cache`, unpacked there
    /// the first time any rene needs them. The sources are read in place,
    /// so the session keeps them pinned.
    pub async fn unpack(&self, cache: &Cache) -> Result<PathBuf, String> {
        let name = self.name;
        let request = Request {
            id: self.id(),
            usage: Usage::InPlace,
            what: &format!("unpacking the bundled {name} sources"),
            notify: &|message| tracing::info!("{message}"),
        };
        let slot = match cache
            .acquire(&request, |dir, ()| Ok(dir.to_owned()))
            .await?
        {
            Acquired::Ready(dir) => return Ok(dir),
            Acquired::Build(slot) => slot,
        };
        let staging = slot.stage()?;
        let unpacked = zstd::Decoder::new(self.bytes)
            .and_then(|decoder| tar::Archive::new(decoder).unpack(staging.path()));
        unpacked.map_err(|e| {
            let path = staging.path().display();
            format!("cannot unpack the bundled {name} sources into `{path}`: {e}")
        })?;
        slot.publish(staging, &()).await
    }
}

#[cfg(test)]
mod tests {
    use std::path::Path;

    use super::*;

    fn block_on<T>(fut: impl Future<Output = T>) -> T {
        compio::runtime::Runtime::new().unwrap().block_on(fut)
    }

    #[test]
    fn every_bundle_unpacks_to_a_loadable_package_once() {
        let tmp = tempfile::tempdir().unwrap();
        let cache = block_on(Cache::open(tmp.path())).unwrap();
        for bundle in ALL {
            let dir = block_on(bundle.unpack(&cache)).unwrap();
            assert_eq!(dir, bundle.id().dir(cache.root()));
            let loaded = crate::manifest::load(&dir.join("rene.ncl")).unwrap();
            assert_eq!(loaded.manifest.package.name, bundle.name);
            assert!(dir.join("src/lib.rr").is_file(), "{}", dir.display());
            // The live entry is used as it is.
            std::fs::write(dir.join("marker"), "").unwrap();
            assert_eq!(block_on(bundle.unpack(&cache)).unwrap(), dir);
            assert!(dir.join("marker").is_file());
        }
        // The core entry keeps the kind name lit checks for.
        assert!(
            CORE.id()
                .dir(cache.root())
                .starts_with(tmp.path().join("core-src"))
        );
    }

    #[test]
    fn bundles_are_found_by_name() {
        assert_eq!(find("core").map(|bundle| bundle.name), Some("core"));
        assert_eq!(find("std").map(|bundle| bundle.name), Some("std"));
        assert!(find("serde").is_none());
    }

    /// A bundle holds exactly the package's manifest and sources: nothing a
    /// build or an editor left in the library's directory.
    #[test]
    fn a_bundle_holds_the_manifest_and_the_sources() {
        fn sources(dir: &Path, rel: &Path, out: &mut Vec<String>) {
            for entry in std::fs::read_dir(dir).unwrap() {
                let entry = entry.unwrap();
                let name = entry.file_name().into_string().unwrap();
                if name.starts_with('.') {
                    continue;
                }
                let rel = rel.join(&name);
                if entry.file_type().unwrap().is_dir() {
                    sources(&entry.path(), &rel, out);
                } else {
                    out.push(rel.to_str().unwrap().replace('\\', "/"));
                }
            }
        }
        let library = Path::new(env!("CARGO_MANIFEST_DIR")).join("../../library");
        for bundle in ALL {
            let mut expected = vec!["rene.ncl".to_owned()];
            let package = library.join(bundle.name);
            sources(&package.join("src"), Path::new("src"), &mut expected);
            expected.sort();

            let decoder = zstd::Decoder::new(bundle.bytes).unwrap();
            let mut archive = tar::Archive::new(decoder);
            let mut packed: Vec<String> = archive
                .entries()
                .unwrap()
                .map(|entry| entry.unwrap().path().unwrap().to_str().unwrap().to_owned())
                .collect();
            packed.sort();
            assert_eq!(packed, expected, "{}", bundle.name);
        }
    }
}
