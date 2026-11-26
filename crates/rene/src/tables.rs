//! Key namespaces for the build-status database (`reussir-build/rene.meta`).
//!
//! Every namespace and key the database holds is declared here, so the on-disk
//! schema can be reviewed in one place. Values are JSON strings — the schema
//! of each value is owned by the module that writes it (referenced per key
//! below).

/// Build status: string keys (the `*_KEY` constants) to JSON-encoded values.
/// The prefix separates status keys from source paths in TurboKV's keyspace.
pub const STATUS: &str = "status/";

/// The package's source graph, as last reported by `rrc --scan-deps`: one
/// row per file, keyed by its path, holding what staleness is judged from.
/// Keys are this prefix followed by the UTF-8 path; values are JSON-encoded
/// [`crate::deps::SourceRecord`]s, including module segments and the digest's
/// 32 bytes. Hex is for [`crate::deps::SourceFile::to_json`] to render.
///
/// The graph is a *set* of files: nothing in rene depends on the order the
/// scan walked them in (staleness checks every row, and `rrc` rediscovers
/// the graph itself), so rows simply come back in path order. Written by
/// [`crate::deps`], always wholesale — the graph is a single snapshot, so a
/// rebuild replaces every row rather than updating one.
pub const SOURCES: &str = "sources/";

/// Blake3 hex digest of the evaluated manifest the [`SOURCES`] snapshot was
/// taken under (a bare hex string). A mismatch invalidates the snapshot: a
/// changed configuration may change the package's layout.
pub const SOURCES_CONFIG_HASH_KEY: &str = "sources.config-hash";

/// Status key for the Blake3 hex digest of the bundled `reussir-rt` source
/// archive baked for `target`. Each target keeps an independent record and
/// source tree, so switching targets does not evict a usable bake.
pub fn rt_source_hash_key(target: &str) -> String {
    format!("rt.{target}.source-hash")
}

/// Status key for a target's baked runtime artifact record, a JSON
/// [`crate::rt::RtArtifacts`]: target, toolchain identity, and artifact
/// paths. The fuzzer runtime is a separate bake with its own record.
pub fn rt_artifacts_key(target: &str, fuzzer: bool) -> String {
    if fuzzer {
        format!("rt.{target}.fuzzer-artifacts")
    } else {
        format!("rt.{target}.artifacts")
    }
}

/// The instrumentation that a build compiles with. Each variant has its own
/// product records, so one variant does not replace the products of another.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct Variant {
    /// `rrc --fuzzer`.
    pub fuzzer: bool,
    /// `rrc --instrument-coverage`.
    pub coverage: bool,
}

/// Put the records of a coverage build in their own `coverage.` namespace.
fn coverage_key(key: String, variant: Variant) -> String {
    if variant.coverage {
        format!("coverage.{key}")
    } else {
        key
    }
}

/// The status key of one built product's record, a JSON
/// [`crate::compile::ProductRecord`]: the fingerprint it was built under and
/// where the artifact landed. Records of targets that leave the manifest
/// simply go stale in place — nothing reads them again, and a config change
/// re-fingerprints the rest. A fuzzer target has its own name, so only
/// coverage separates the records of one target.
pub fn product_key(profile: &str, target: &str, variant: Variant) -> String {
    coverage_key(format!("product.{profile}.{target}"), variant)
}

/// The status key of one built dependency's record, a JSON
/// [`crate::fresh::DepRecord`]: the components its freshness is judged
/// from, and where its interface and archive landed.
pub fn dep_product_key(profile: &str, name: &str) -> String {
    format!("product.{profile}.dep.{name}")
}

pub fn dep_variant_key(profile: &str, name: &str, variant: Variant) -> String {
    let key = if variant.fuzzer {
        format!("fuzzer-dep-product.{profile}.{name}")
    } else {
        dep_product_key(profile, name)
    };
    coverage_key(key, variant)
}

/// The source graph used to build this dependency variant, a JSON
/// `Vec<`[`crate::deps::SourceFile`]`>` — the per-dependency counterpart of
/// the root's [`SOURCES`] table, compact because dependencies are read-only
/// inputs scanned wholesale.
pub fn dep_sources_key(profile: &str, name: &str, variant: Variant) -> String {
    format!("sources.{}", dep_variant_key(profile, name, variant))
}

/// Teardown marker (JSON `true`), set by `clean` while it still holds the
/// lock, right before it deletes the directory. A database carrying it is a
/// directory whose deletion is in flight or was interrupted: `build` refuses
/// it and directs the user back to `rene clean`, which is the only thing
/// that clears the marker (by deleting the directory).
pub const CLEANING_KEY: &str = "cleaning";

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn fuzz_dependency_records_do_not_alias_dotted_names() {
        let fuzzer = Variant {
            fuzzer: true,
            coverage: false,
        };
        let fuzz = dep_variant_key("dev", "foo", fuzzer);
        assert_ne!(
            fuzz,
            dep_variant_key("dev", "foo.fuzzer", Variant::default())
        );
        assert_ne!(
            fuzz,
            product_key("dev", "fuzzer-dep.foo", Variant::default())
        );
    }

    /// Coverage records have their own keys, and the other keys do not change.
    #[test]
    fn coverage_records_do_not_alias_the_other_variants() {
        let variants =
            [false, true].map(|coverage| [false, true].map(|fuzzer| Variant { fuzzer, coverage }));
        let mut keys: Vec<String> = variants
            .iter()
            .flatten()
            .flat_map(|&variant| {
                [
                    product_key("dev", "foo", variant),
                    dep_variant_key("dev", "foo", variant),
                    dep_sources_key("dev", "foo", variant),
                ]
            })
            .collect();
        assert_eq!(keys[0], "product.dev.foo");
        assert_eq!(keys[1], "product.dev.dep.foo");
        keys.sort();
        keys.dedup();
        // The fuzzer instrumentation does not separate the root products.
        assert_eq!(keys.len(), 10, "{keys:#?}");
    }
}
