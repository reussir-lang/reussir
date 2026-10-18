//! Compiled artifacts of the bundled packages, shared through the cache.
//!
//! A bundled package (`core`, `std`) is the same source in every build,
//! so compiling it again in each build directory is wasted work. When the
//! dependency pipeline has to build one, it first takes the package's
//! [`Cache`] entry lock and looks for a complete entry. If it finds one, it
//! copies the interface, the archive, and any token-reuse report into the
//! build directory and records the cached source graph, the same record a
//! local build would write. Otherwise it builds locally as usual, then
//! publishes the outputs to the entry. Either way the build directory ends
//! up with its own copy, so freshness, the plan dump, and linking are
//! unchanged: the cache only replaces the compile.
//!
//! The entry key hashes everything the outputs depend on:
//!
//! - the package's bundled content hash;
//! - the planned `rrc` argv, with the build directory's `deps/` path
//!   abstracted away, which covers the target, the profile's flags, the
//!   linker, the polyffi library directories, and the token-reuse flag;
//! - the digests of the upstream artifacts the compile consumes;
//! - the runtime bake the compile links against (its toolchain and paths);
//! - the identity of the `rrc` binary and of this rene.
//!
//! A cache failure is never a build failure: rene warns and builds locally.

use std::collections::BTreeMap;
use std::path::{Path, PathBuf};

use serde::{Deserialize, Serialize};

use crate::cache::{self, Cache};
use crate::deps::{self, SourceFile};
use crate::fresh;
use crate::plan::{self, PlannedCommand};
use crate::rt::RtArtifacts;

/// The cache kind holding compiled bundled packages.
const KIND: &str = "deps";

/// The entry's record, written last: its presence marks the entry complete.
const RECORD: &str = "record.json";

/// Stands for the build directory's `deps/` path in hashed argv.
const DEPS_DIR: &str = "<deps-dir>";

/// The bundled packages a build may share, and where they are cached.
pub struct Shared {
    cache: Cache,
    /// Package name to its bundled content hash.
    packages: BTreeMap<String, String>,
    /// [`cache::program_identity`] of `rrc`, resolved once per build.
    rrc: String,
}

/// An entry's record.
#[derive(Serialize, Deserialize)]
struct Record {
    /// The output files' names, all inside the entry and, once restored,
    /// inside the build directory's `deps/`.
    files: Vec<String>,
    /// The package's source graph, as the compile's scan reported it.
    sources: Vec<SourceFile>,
}

/// A bundled package's claim on its entry: the held lock and the entry
/// path, for [`Shared::publish`] after a local build.
pub struct Claim {
    entry: PathBuf,
    _lock: cache::EntryLock,
}

impl Shared {
    /// Share the `packages` (name to bundled content hash) through `cache`.
    pub fn new(cache: Cache, packages: BTreeMap<String, String>) -> Self {
        Shared {
            cache,
            packages,
            rrc: cache::program_identity(&deps::resolve_rrc()),
        }
    }

    /// Claim the entry for `node`, if it is a shared package, waiting for
    /// another rene building the same entry (`notify` says so). `None` means
    /// the node is not shared, or the cache cannot be used: build locally.
    pub(crate) async fn claim(
        &self,
        node: &Node<'_>,
        rt: &RtArtifacts,
        notify: impl FnOnce(&str),
    ) -> Option<Outcome> {
        let name = node.name;
        let content = self.packages.get(name)?;
        let key = self.key(node, content, rt);
        let lock = match self
            .cache
            .lock(KIND, &key, &format!("building `{name}`"), notify)
            .await
        {
            Ok(lock) => lock,
            Err(e) => {
                tracing::warn!("{e}; building `{name}` without the shared cache");
                return None;
            }
        };
        let entry = self.cache.entry(KIND, &key);
        match restore(&entry, node.deps_dir) {
            Ok(Some(sources)) => {
                tracing::debug!(package = name, entry = %entry.display(), "restored from the cache");
                Some(Outcome::Restored(sources))
            }
            Ok(None) => Some(Outcome::Build(Claim { entry, _lock: lock })),
            Err(e) => {
                tracing::warn!("{e}; building `{name}` locally");
                Some(Outcome::Build(Claim { entry, _lock: lock }))
            }
        }
    }

    /// Publish a local build's outputs (those of [`outputs`] that exist) to
    /// the claimed entry. Best effort: a failure only warns, since the build
    /// itself succeeded.
    pub fn publish(claim: Claim, outputs: &[PathBuf], sources: &[SourceFile]) {
        if let Err(e) = publish(&claim.entry, outputs, sources) {
            tracing::warn!("cannot share the build through the cache: {e}");
        }
    }

    fn key(&self, node: &Node<'_>, content: &str, rt: &RtArtifacts) -> String {
        let deps_prefix = node.deps_dir.display().to_string();
        let argv = node
            .commands
            .iter()
            .flat_map(|command| {
                std::iter::once(command.emit.to_owned()).chain(
                    command
                        .args
                        .iter()
                        .map(|arg| arg.replace(&deps_prefix, DEPS_DIR)),
                )
            })
            .collect::<Vec<_>>()
            .join("\0");
        let upstream = node
            .dependencies
            .iter()
            .map(|dep| {
                let digest = fresh::artifact_digest(node.deps_dir, dep, node.target);
                format!("{dep}={digest}")
            })
            .collect::<Vec<_>>()
            .join("\0");
        let rt = serde_json::to_string(rt).unwrap_or_default();
        cache::key([
            b"shared package v1".as_slice(),
            env!("CARGO_PKG_VERSION").as_bytes(),
            node.name.as_bytes(),
            content.as_bytes(),
            argv.as_bytes(),
            upstream.as_bytes(),
            rt.as_bytes(),
            self.rrc.as_bytes(),
        ])
    }
}

/// The dependency node a build is about to compile.
pub(crate) struct Node<'a> {
    pub(crate) name: &'a str,
    /// Its direct dependencies, whose artifacts the compile consumes.
    pub(crate) dependencies: &'a [String],
    /// The commands that build it ([`plan::node_commands`]).
    pub(crate) commands: &'a [PlannedCommand],
    /// The build directory's `deps/`, where the outputs land.
    pub(crate) deps_dir: &'a Path,
    pub(crate) target: &'a str,
}

/// What [`Shared::claim`] found.
pub enum Outcome {
    /// The outputs were copied in; the cached source graph.
    Restored(Vec<SourceFile>),
    /// No complete entry: build locally, then [`Shared::publish`].
    Build(Claim),
}

/// The files a node's build may produce: each command's output and the
/// token-reuse sidecar it would write next to it. [`Shared::publish`] shares
/// the ones that exist once the build is done.
pub(crate) fn outputs(commands: &[PlannedCommand]) -> Vec<PathBuf> {
    commands
        .iter()
        .flat_map(|command| {
            [
                command.out.clone(),
                plan::token_reuse_report_path(&command.out),
            ]
        })
        .collect()
}

/// Copy a complete entry's outputs into `deps_dir`. `Ok(None)` when the
/// entry is absent or incomplete.
fn restore(entry: &Path, deps_dir: &Path) -> Result<Option<Vec<SourceFile>>, String> {
    let Ok(record) = std::fs::read_to_string(entry.join(RECORD)) else {
        return Ok(None);
    };
    let Ok(record) = serde_json::from_str::<Record>(&record) else {
        return Ok(None);
    };
    if record.files.iter().any(|file| !entry.join(file).is_file()) {
        return Ok(None);
    }
    for file in &record.files {
        let dest = deps_dir.join(file);
        let tmp = cache::temp_sibling(&dest);
        std::fs::copy(entry.join(file), &tmp)
            .and_then(|_| std::fs::rename(&tmp, &dest))
            .map_err(|e| {
                let _ = std::fs::remove_file(&tmp);
                format!("cannot restore `{}` from the cache: {e}", dest.display())
            })?;
    }
    Ok(Some(record.sources))
}

/// Stage the outputs in a temporary sibling of `entry`, then rename it into
/// place, replacing an incomplete entry.
fn publish(entry: &Path, outputs: &[PathBuf], sources: &[SourceFile]) -> Result<(), String> {
    let staging = cache::temp_sibling(entry);
    let staged = (|| {
        std::fs::create_dir_all(&staging)
            .map_err(|e| format!("cannot create `{}`: {e}", staging.display()))?;
        let mut files = Vec::new();
        for output in outputs.iter().filter(|output| output.is_file()) {
            let name = output
                .file_name()
                .ok_or_else(|| format!("`{}` has no file name", output.display()))?;
            std::fs::copy(output, staging.join(name))
                .map_err(|e| format!("cannot copy `{}`: {e}", output.display()))?;
            files.push(name.to_string_lossy().into_owned());
        }
        let record = Record {
            files,
            sources: sources.to_vec(),
        };
        let record = serde_json::to_string(&record).map_err(|e| e.to_string())?;
        cache::write_atomically(&staging.join(RECORD), record.as_bytes())
    })();
    if let Err(e) = staged {
        let _ = std::fs::remove_dir_all(&staging);
        return Err(e);
    }
    if entry.exists() {
        // An incomplete entry (the claim found no usable record). We hold
        // the lock, so no one else is publishing it.
        std::fs::remove_dir_all(entry)
            .map_err(|e| format!("cannot clear `{}`: {e}", entry.display()))?;
    }
    cache::publish_dir(&staging, entry)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn source(path: &Path) -> SourceFile {
        SourceFile {
            path: path.display().to_string(),
            record: deps::SourceRecord {
                module: vec!["core".to_owned()],
                mtime_ns: 1,
                size: 2,
                hash: [3; blake3::OUT_LEN],
            },
        }
    }

    #[test]
    fn a_published_entry_restores_into_another_build_directory() {
        let tmp = tempfile::tempdir().unwrap();
        let first = tmp.path().join("first");
        std::fs::create_dir_all(&first).unwrap();
        let rri = first.join("core.rri");
        let archive = first.join("libcore.a");
        std::fs::write(&rri, "interface").unwrap();
        std::fs::write(&archive, "archive").unwrap();
        let entry = tmp.path().join("cache").join("entry");
        std::fs::create_dir_all(entry.parent().unwrap()).unwrap();
        let sources = vec![source(&tmp.path().join("lib.rr"))];

        assert!(restore(&entry, &first).unwrap().is_none());
        publish(&entry, &[rri, archive], &sources).unwrap();

        let second = tmp.path().join("second");
        std::fs::create_dir_all(&second).unwrap();
        assert_eq!(restore(&entry, &second).unwrap(), Some(sources));
        assert_eq!(
            std::fs::read_to_string(second.join("core.rri")).unwrap(),
            "interface"
        );
        assert_eq!(
            std::fs::read_to_string(second.join("libcore.a")).unwrap(),
            "archive"
        );
    }

    #[test]
    fn an_entry_missing_a_file_is_not_restored() {
        let tmp = tempfile::tempdir().unwrap();
        let out = tmp.path().join("core.rri");
        std::fs::write(&out, "interface").unwrap();
        let entry = tmp.path().join("entry");
        publish(&entry, &[out], &[]).unwrap();
        std::fs::remove_file(entry.join("core.rri")).unwrap();
        assert!(restore(&entry, tmp.path()).unwrap().is_none());

        // Publishing again replaces the incomplete entry.
        let out = tmp.path().join("core.rri");
        publish(&entry, std::slice::from_ref(&out), &[]).unwrap();
        assert!(restore(&entry, tmp.path()).unwrap().is_some());
    }
}
