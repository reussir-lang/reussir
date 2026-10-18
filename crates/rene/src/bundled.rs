//! The library packages bundled inside rene: `core` and `std`.
//!
//! Both are written in Reussir and compiled like any other dependency, but
//! shipped *inside* rene, exactly like the `reussir-rt` bundle: a
//! zstd-compressed tar per package, packed by `build.rs`.
//!
//! - `core` is the language-coupled layer. It is injected into every
//!   build's dependency graph unless a package opts out with
//!   `package.no_core`.
//! - `std` is opt-in: a package depends on it with
//!   `dependencies.std = { bundled = true }`.
//!
//! The sources unpack once into the shared [`Cache`], under a
//! content-hashed name. Every build of every package reads the same tree, a
//! rene upgrade materializes a new one, and the ordinary per-package
//! freshness machinery sees the change. Their compiled artifacts are shared
//! through the cache as well ([`crate::shared`]).

use std::collections::BTreeMap;
use std::path::{Path, PathBuf};

use crate::cache::{self, Cache};
use crate::resolve::Graph;

/// One bundled package.
pub struct Bundle {
    /// The package name, which dependents reference it by.
    pub name: &'static str,
    /// The package source (tar.zst), packed by `build.rs` with fixed entry
    /// metadata — the bytes depend only on the sources.
    bytes: &'static [u8],
}

/// The bundled `core`.
pub static CORE: Bundle = Bundle {
    name: "core",
    bytes: include_bytes!(concat!(env!("OUT_DIR"), "/core-src.tar.zst")),
};

/// The bundled `std`.
pub static STD: Bundle = Bundle {
    name: "std",
    bytes: include_bytes!(concat!(env!("OUT_DIR"), "/std-src.tar.zst")),
};

/// Every bundle, in dependency order.
pub static ALL: [&Bundle; 2] = [&CORE, &STD];

/// The bundle named `name`, if rene ships one.
pub fn find(name: &str) -> Option<&'static Bundle> {
    ALL.iter().copied().find(|bundle| bundle.name == name)
}

/// The names of the bundles a dependency may ask for, for diagnostics.
pub fn names() -> String {
    ALL.iter()
        .map(|bundle| format!("`{}`", bundle.name))
        .collect::<Vec<_>>()
        .join(", ")
}

impl Bundle {
    /// The cache kind holding this bundle's unpacked sources.
    fn kind(&self) -> String {
        format!("{}-src", self.name)
    }

    /// The bundle's content hash — the directory name component that keys
    /// unpacked sources to this rene build.
    pub fn content_hash(&self) -> String {
        blake3::hash(self.bytes).to_hex()[..16].to_string()
    }

    /// Where the bundle lives (or will live) in `cache`. Pure — inspect
    /// paths print without writing anything.
    pub fn dir(&self, cache: &Cache) -> PathBuf {
        cache.entry(&self.kind(), &self.content_hash())
    }

    /// Materialize the bundle in `cache`, idempotently, and return its root.
    /// The entry is content-keyed and published by an atomic rename, so its
    /// existence means the whole bundle is there. The unpack holds the entry
    /// lock, so concurrent builds unpack it once.
    pub async fn unpack(&self, cache: &Cache) -> Result<PathBuf, String> {
        let root = self.dir(cache);
        if root.join("rene.ncl").is_file() {
            return Ok(root);
        }
        let name = self.name;
        let _lock = cache
            .lock(
                &self.kind(),
                &self.content_hash(),
                &format!("unpacking the {name} sources"),
                |msg| tracing::info!("{msg}"),
            )
            .await?;
        if root.join("rene.ncl").is_file() {
            return Ok(root);
        }
        if root.exists() {
            // Not ours to trust: no complete entry ever lacks its manifest.
            std::fs::remove_dir_all(&root)
                .map_err(|e| format!("cannot clear `{}`: {e}", root.display()))?;
        }
        let staging = cache::temp_sibling(&root);
        let unpacked = zstd::Decoder::new(self.bytes)
            .map_err(|e| format!("cannot decode the bundled {name} source: {e}"))
            .and_then(|decoder| {
                tar::Archive::new(decoder).unpack(&staging).map_err(|e| {
                    format!(
                        "cannot unpack the bundled {name} source into {}: {e}",
                        staging.display()
                    )
                })
            });
        if let Err(e) = unpacked {
            let _ = std::fs::remove_dir_all(&staging);
            return Err(e);
        }
        cache::publish_dir(&staging, &root)?;
        Ok(root)
    }
}

/// Every bundle unpacked in `cache`: name to source directory. Unpacking is
/// a one-time cost per rene version, so all of them are materialized
/// whether or not this build asks for `std`.
pub async fn unpack_all(cache: &Cache) -> Result<BTreeMap<String, PathBuf>, String> {
    let mut dirs = BTreeMap::new();
    for bundle in ALL {
        dirs.insert(bundle.name.to_owned(), bundle.unpack(cache).await?);
    }
    Ok(dirs)
}

/// The bundled packages `graph` actually uses — those whose node loads from
/// the bundle's directory — as name to content hash, the packages
/// [`crate::shared::Shared`] may share.
pub fn in_graph(graph: &Graph, bundles: &BTreeMap<String, PathBuf>) -> BTreeMap<String, String> {
    let canonical = |dir: &Path| dir.canonicalize().unwrap_or_else(|_| dir.to_owned());
    ALL.iter()
        .filter(|bundle| {
            let (Some(node), Some(dir)) = (graph.nodes.get(bundle.name), bundles.get(bundle.name))
            else {
                return false;
            };
            canonical(&node.dir) == canonical(dir)
        })
        .map(|bundle| (bundle.name.to_owned(), bundle.content_hash()))
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn block_on<T>(fut: impl Future<Output = T>) -> T {
        compio::runtime::Runtime::new().unwrap().block_on(fut)
    }

    #[test]
    fn every_bundle_unpacks_to_a_package() {
        let tmp = tempfile::tempdir().unwrap();
        let cache = Cache::at(tmp.path()).unwrap();
        let dirs = block_on(unpack_all(&cache)).unwrap();
        for bundle in ALL {
            let dir = &dirs[bundle.name];
            assert_eq!(*dir, bundle.dir(&cache));
            let manifest = crate::manifest::load(&dir.join("rene.ncl")).unwrap();
            assert_eq!(manifest.manifest.package.name, bundle.name);
            assert!(dir.join("src/lib.rr").is_file(), "{}", dir.display());
        }
        // The core entry keeps its established name.
        assert!(dirs["core"].starts_with(tmp.path().join("core-src")));
        // Idempotent.
        assert_eq!(block_on(unpack_all(&cache)).unwrap(), dirs);
    }

    #[test]
    fn bundles_are_found_by_name() {
        assert_eq!(find("std").map(|b| b.name), Some("std"));
        assert!(find("serde").is_none());
        assert_eq!(names(), "`core`, `std`");
    }
}
