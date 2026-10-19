//! The bundled `core` package sources.
//!
//! `core` is the language-coupled layer of the standard library, written in
//! Reussir and compiled like any other dependency — but shipped *inside*
//! rene, exactly like the `reussir-rt` bundle: a zstd-compressed tar packed
//! by `build.rs`, injected into every build's dependency graph unless a
//! package opts out with `package.no_core`. The sources unpack once into
//! the shared [`Cache`] under a content-hashed name, so every build of
//! every package reads the same tree, a rene upgrade materializes a new
//! one, and the ordinary per-package freshness machinery sees the change.

use std::path::PathBuf;
use std::time::Duration;

use crate::cache::{self, Cache};
use crate::ledger::{Lookup, Recheck, Session};

/// The bundled package source (tar.zst), packed by `build.rs` with fixed
/// entry metadata — the bytes depend only on the sources.
static CORE_BUNDLE: &[u8] = include_bytes!(concat!(env!("OUT_DIR"), "/core-src.tar.zst"));

/// The cache kind holding unpacked core sources.
const KIND: &str = "core-src";

/// The bundle's content hash — the directory name component that keys
/// unpacked sources to this rene build.
pub fn content_hash() -> String {
    blake3::hash(CORE_BUNDLE).to_hex()[..16].to_string()
}

/// Where the bundle lives (or will live) in `cache`. Pure — inspect paths
/// print without writing anything.
pub fn dir(cache: &Cache) -> PathBuf {
    cache.entry(KIND, &content_hash())
}

/// How long to wait before asking again for an entry being removed.
const WAIT: Duration = Duration::from_millis(100);

/// Materialize the bundle in `cache`, idempotently, and return its root.
/// The entry is content-keyed and published by an atomic rename, so its
/// existence means the whole bundle is there. The `session` pins the entry
/// for the rest of the build, since the sources are read in place; the
/// unpack holds the entry lock, so concurrent builds unpack it once.
pub async fn unpack(cache: &Cache, session: &Session) -> Result<PathBuf, String> {
    let key = content_hash();
    let root = dir(cache);
    let complete = || root.join("rene.ncl").is_file();
    loop {
        match session.lookup(KIND, &key, &root).await {
            Lookup::Live if complete() => return Ok(root),
            Lookup::Live | Lookup::Build => {}
            Lookup::Wait => {
                compio::runtime::time::sleep(WAIT).await;
                continue;
            }
        }
        let _lock = cache
            .lock(KIND, &key, "unpacking the core sources", |msg| {
                tracing::info!("{msg}")
            })
            .await?;
        match session.recheck(KIND, &key, &root, complete()).await {
            Recheck::Live => return Ok(root),
            Recheck::Build => {}
            Recheck::Wait => {
                drop(_lock);
                compio::runtime::time::sleep(WAIT).await;
                continue;
            }
        }
        if root.exists() {
            // Not ours to trust: no complete entry ever lacks its manifest.
            std::fs::remove_dir_all(&root)
                .map_err(|e| format!("cannot clear `{}`: {e}", root.display()))?;
        }
        let staging = cache::temp_sibling(&root);
        let unpacked = zstd::Decoder::new(CORE_BUNDLE)
            .map_err(|e| format!("cannot decode the bundled core source: {e}"))
            .and_then(|decoder| {
                tar::Archive::new(decoder).unpack(&staging).map_err(|e| {
                    format!(
                        "cannot unpack the bundled core source into {}: {e}",
                        staging.display()
                    )
                })
            });
        if let Err(e) = unpacked {
            let _ = std::fs::remove_dir_all(&staging);
            return Err(e);
        }
        cache::publish_dir(&staging, &root)?;
        session.register(KIND, &key, cache::dir_size(&root)).await;
        return Ok(root);
    }
}
