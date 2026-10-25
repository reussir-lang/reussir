//! The library packages bundled inside rene: `core` and `std`.
//!
//! Both are written in Reussir and compiled like any other dependency, but
//! shipped *inside* rene, exactly like the `reussir-rt` bundle: a
//! zstd-compressed tar of each package's manifest and sources, packed by
//! `build.rs`.
//!
//! - `core` is the language-coupled layer. It is injected into every
//!   build's dependency graph unless a package opts out with
//!   `package.no_core`.
//! - `std` is opt-in: a package depends on it with
//!   `dependencies.std = { bundled = true }`.
//!
//! This table is also the list of reserved package names, which `rrc
//! --builtin` lifts for exactly these packages' builds.
//!
//! A bundle's sources unpack into the shared [`Cache`], under its content
//! hash, the first time a graph uses it ([`load_graph`]). Every build of
//! every package reads the same tree, a rene upgrade materializes a new one,
//! and the ordinary per-package freshness machinery sees the change: a
//! dependency whose recorded sources no longer lie under its directory is
//! rebuilt. Their compiled artifacts are shared through the cache as well
//! ([`crate::shared`]).

use std::collections::BTreeMap;
use std::path::PathBuf;

use crate::cache::{self, Acquired, Cache, EntryId, Request, Usage};
use crate::manifest::Loaded;
use crate::resolve::{self, Graph};

/// One bundled package.
pub struct Bundle {
    /// The package name, which dependents reference it by.
    pub name: &'static str,
    /// The cache kind holding its unpacked sources.
    kind: &'static str,
    /// The package's manifest and sources (tar.zst), packed by `build.rs`
    /// with fixed entry metadata: the bytes depend only on the sources.
    bytes: &'static [u8],
}

/// The bundled `core`.
pub static CORE: Bundle = Bundle {
    name: "core",
    kind: cache::kind::CORE_SRC,
    bytes: include_bytes!(concat!(env!("OUT_DIR"), "/core-src.tar.zst")),
};

/// The bundled `std`.
pub static STD: Bundle = Bundle {
    name: "std",
    kind: cache::kind::STD_SRC,
    bytes: include_bytes!(concat!(env!("OUT_DIR"), "/std-src.tar.zst")),
};

/// Every bundle.
pub static ALL: [&Bundle; 2] = [&CORE, &STD];

/// The bundle named `name`, if rene ships one.
pub fn find(name: &str) -> Option<&'static Bundle> {
    ALL.iter().copied().find(|bundle| bundle.name == name)
}

/// The names of the bundles, for diagnostics.
pub fn names() -> String {
    ALL.iter()
        .map(|bundle| format!("`{}`", bundle.name))
        .collect::<Vec<_>>()
        .join(", ")
}

/// Load the dependency graph rooted at `root`, unpacking into `cache`
/// exactly the bundles it uses: those its `bundled = true` dependencies
/// name, and `core` unless every package opts out of it.
pub async fn load_graph(cache: &Cache, root: &Loaded) -> Result<Graph, String> {
    let mut loader = resolve::Loader::new(root);
    while let Some(name) = loader.next_bundle()? {
        // Manifest validation admits only the names of bundles.
        let bundle = find(&name).ok_or_else(|| format!("rene bundles no `{name}`"))?;
        loader.provide(&name, bundle.unpack(cache).await?);
    }
    if loader.wants_core() {
        loader.provide(CORE.name, CORE.unpack(cache).await?);
    }
    loader.finish()
}

/// The bundles `graph` builds from their bundled sources, as name to
/// content hash: the packages [`crate::shared::Shared`] may share. A package
/// of the same name brought by path is built locally like any other.
pub fn in_graph(graph: &Graph, cache: &Cache) -> BTreeMap<String, String> {
    ALL.iter()
        .filter(|bundle| {
            // Node directories are canonical.
            let dir = bundle.dir(cache).canonicalize();
            graph
                .nodes
                .get(bundle.name)
                .is_some_and(|node| dir.is_ok_and(|dir| dir == node.dir))
        })
        .map(|bundle| (bundle.name.to_owned(), bundle.content_hash()))
        .collect()
}

impl Bundle {
    /// The bundle's content hash: the entry key that ties unpacked sources
    /// to this rene build.
    pub fn content_hash(&self) -> String {
        blake3::hash(self.bytes).to_hex()[..16].to_string()
    }

    fn id(&self) -> EntryId {
        EntryId::new(self.kind, self.content_hash())
    }

    /// Where the bundle lives (or will live) in `cache`. Pure: inspect
    /// paths print without writing anything.
    pub fn dir(&self, cache: &Cache) -> PathBuf {
        cache.entry(&self.id())
    }

    /// Materialize the bundle in `cache`, once, and return its root. The
    /// sources are read in place for the rest of the build. An entry is
    /// complete once its manifest is there, since it is unpacked privately
    /// and renamed into place.
    pub async fn unpack(&self, cache: &Cache) -> Result<PathBuf, String> {
        let name = self.name;
        let request = Request {
            id: self.id(),
            usage: Usage::InPlace,
            what: &format!("unpacking the {name} sources"),
            notify: &|msg| tracing::info!("{msg}"),
        };
        let ready = |dir: &std::path::Path| {
            let manifest = dir.join(crate::manifest::MANIFEST_FILE);
            manifest.is_file().then(|| dir.to_owned())
        };
        let slot = match cache.acquire(&request, ready).await? {
            Acquired::Ready(dir) => return Ok(dir),
            Acquired::Build(slot) => slot,
        };
        let staging = slot.stage()?;
        let decoder = zstd::Decoder::new(self.bytes)
            .map_err(|e| format!("cannot decode the bundled {name} sources: {e}"))?;
        tar::Archive::new(decoder)
            .unpack(staging.path())
            .map_err(|e| {
                format!(
                    "cannot unpack the bundled {name} sources into `{}`: {e}",
                    staging.path().display()
                )
            })?;
        slot.publish(staging).await
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
    fn every_bundle_unpacks_to_a_package_once() {
        let tmp = tempfile::tempdir().unwrap();
        let cache = Cache::at(tmp.path()).unwrap();
        for bundle in ALL {
            let dir = block_on(bundle.unpack(&cache)).unwrap();
            assert_eq!(dir, bundle.dir(&cache));
            let manifest = crate::manifest::load(&dir.join("rene.ncl")).unwrap();
            assert_eq!(manifest.manifest.package.name, bundle.name);
            assert!(dir.join("src/lib.rr").is_file(), "{}", dir.display());
            // Idempotent: the complete entry is used as it is.
            std::fs::write(dir.join("marker"), "").unwrap();
            assert_eq!(block_on(bundle.unpack(&cache)).unwrap(), dir);
            assert!(dir.join("marker").is_file());
        }
        // The core entry keeps its established name, which lit checks.
        assert!(CORE.dir(&cache).starts_with(tmp.path().join("core-src")));
    }

    #[test]
    fn bundles_are_found_by_name() {
        assert_eq!(find("std").map(|b| b.name), Some("std"));
        assert!(find("serde").is_none());
        assert_eq!(names(), "`core`, `std`");
    }

    /// A bundle holds exactly the package's manifest and sources: nothing a
    /// build or an editor left in the library's directory.
    #[test]
    fn a_bundle_holds_the_manifest_and_sources() {
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
