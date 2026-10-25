//! Compiled artifacts of the bundled packages, shared through the cache.
//!
//! A bundled package (`core` or `std`) is the same source in every build,
//! so compiling it again in each build directory is wasted work. When the
//! dependency pipeline has to build one, it first claims the package's
//! [`Cache`] entry. A complete entry is copied into the build directory (the
//! interface, the archive, and any token-reuse report) along with the
//! cached source graph, and the build records it exactly as it records a
//! local compile. Otherwise the claim holds the entry's lock: the package is
//! built locally as usual, and the outputs its commands declare are then
//! published to the entry. Either way the build directory ends up with its
//! own copy, so freshness, the plan dump, and linking are unchanged; the
//! cache only replaces the compile.
//!
//! The entry key hashes everything the outputs depend on:
//!
//! - the package's bundled content hash;
//! - the planned `rrc` argv, with the build directory's `deps/` path
//!   abstracted away, which covers the sources' location
//!   (`--package-root`), the target, the profile's flags, the linker, the
//!   polyffi library directories, and the token-reuse report;
//! - the digests of the upstream artifacts the compile consumes;
//! - the runtime bake the compile links against (its toolchain and paths);
//! - the identity of the `rrc` binary and of this rene.
//!
//! A cache failure is never a build failure: rene warns and builds locally.

use std::collections::BTreeMap;
use std::path::{Path, PathBuf};

use serde::{Deserialize, Serialize};

use crate::cache::{self, Acquired, Cache, EntryId, RecordedFile, Request, Slot, Usage};
use crate::deps::{self, SourceFile};
use crate::fresh;
use crate::plan::PlannedCommand;
use crate::rt::RtArtifacts;

/// The entry's record. Published with the outputs, in the same rename.
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
    /// The outputs, inside the entry and, once restored, inside the build
    /// directory's `deps/`.
    files: Vec<RecordedFile>,
    /// The package's source graph, as the compile's scan reported it.
    sources: Vec<SourceFile>,
}

/// The dependency node a build is about to compile.
pub struct Node<'a> {
    pub(crate) name: &'a str,
    /// Its direct dependencies, whose artifacts the compile consumes.
    pub(crate) dependencies: &'a [String],
    /// The commands that build it ([`crate::plan::node_commands`]).
    pub(crate) commands: &'a [PlannedCommand],
    /// The build directory's `deps/`, where the outputs land.
    pub(crate) deps_dir: &'a Path,
    pub(crate) target: &'a str,
}

/// What [`Shared::claim`] found.
pub enum Claim {
    /// The outputs were copied in; the cached source graph.
    Restored(Vec<SourceFile>),
    /// No complete entry: build locally, then publish.
    Build(Publication),
}

/// The right to publish a local build of a shared package: the entry's
/// slot, and the outputs the node's commands declare. Only these are
/// published, never whatever else lies in `deps/` (a report an earlier build
/// with remarks left there).
pub struct Publication {
    slot: Slot,
    outputs: Vec<PathBuf>,
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

    /// Claim the entry for `node`, waiting for another rene building the
    /// same entry (`notify` says so). `None` means the node is not shared,
    /// or the cache cannot be used: build locally.
    pub async fn claim(
        &self,
        node: &Node<'_>,
        rt: &RtArtifacts,
        notify: &dyn Fn(&str),
    ) -> Option<Claim> {
        let name = node.name;
        let content = self.packages.get(name)?;
        let request = Request {
            id: EntryId::new(cache::kind::DEPS, self.key(node, content, rt)),
            usage: Usage::Copied,
            what: &format!("building `{name}`"),
            notify,
        };
        let ready = |entry: &Path| restore(entry, node.deps_dir);
        match self.cache.acquire(&request, ready).await {
            Ok(Acquired::Ready(Ok(sources))) => {
                tracing::debug!(package = name, "restored from the cache");
                Some(Claim::Restored(sources))
            }
            Ok(Acquired::Ready(Err(e))) => {
                tracing::warn!("{e}; building `{name}` locally");
                None
            }
            Ok(Acquired::Build(slot)) => Some(Claim::Build(Publication {
                slot,
                outputs: outputs(node.commands),
            })),
            Err(e) => {
                tracing::warn!("{e}; building `{name}` without the shared cache");
                None
            }
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

impl Publication {
    /// Publish the local build, with its source graph `sources`, as the
    /// claimed entry. Best effort: a failure only warns, since the build
    /// itself succeeded.
    pub async fn publish(self, sources: &[SourceFile]) {
        if let Err(e) = publish(self.slot, &self.outputs, sources).await {
            tracing::warn!("cannot share the build through the cache: {e}");
        }
    }
}

/// The files the commands declare they write: each command's artifact and
/// the token-reuse report it was asked for.
fn outputs(commands: &[PlannedCommand]) -> Vec<PathBuf> {
    commands
        .iter()
        .flat_map(|command| std::iter::once(&command.out).chain(&command.report))
        .cloned()
        .collect()
}

/// A complete entry's outputs, copied into `deps_dir`, and its source
/// graph: the entry's `ready` check. `None` while the entry is absent or
/// incomplete; an error when the copy fails.
fn restore(entry: &Path, deps_dir: &Path) -> Option<Result<Vec<SourceFile>, String>> {
    let record = std::fs::read_to_string(entry.join(RECORD)).ok()?;
    let record = serde_json::from_str::<Record>(&record).ok()?;
    if !record.files.iter().all(|file| file.intact(entry)) {
        return None;
    }
    let copied = record.files.iter().try_for_each(|file| {
        let dest = deps_dir.join(&file.path);
        let tmp = cache::sibling(&dest, cache::Tag::Tmp);
        std::fs::copy(entry.join(&file.path), &tmp)
            .and_then(|_| std::fs::rename(&tmp, &dest))
            .map_err(|e| {
                let _ = std::fs::remove_file(&tmp);
                format!("cannot restore `{}` from the cache: {e}", dest.display())
            })
    });
    Some(copied.map(|()| record.sources))
}

/// Stage `outputs` and their record, then publish them as the slot's
/// entry.
async fn publish(slot: Slot, outputs: &[PathBuf], sources: &[SourceFile]) -> Result<(), String> {
    let staging = slot.stage()?;
    let mut files = Vec::new();
    for output in outputs {
        let name = output
            .file_name()
            .ok_or_else(|| format!("`{}` has no file name", output.display()))?;
        let staged = staging.path().join(name);
        std::fs::copy(output, &staged)
            .map_err(|e| format!("cannot copy `{}`: {e}", output.display()))?;
        files.push(RecordedFile::measure(staging.path(), &staged)?);
    }
    let record = Record {
        files,
        sources: sources.to_vec(),
    };
    let record = serde_json::to_string(&record).map_err(|e| e.to_string())?;
    cache::write_atomically(&staging.path().join(RECORD), record.as_bytes())?;
    slot.publish(staging).await?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn block_on<T>(fut: impl Future<Output = T>) -> T {
        compio::runtime::Runtime::new().unwrap().block_on(fut)
    }

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

    /// The slot for a fresh build of the entry `key`.
    fn slot(cache: &Cache, key: &str) -> Slot {
        let request = Request {
            id: EntryId::new(cache::kind::DEPS, key),
            usage: Usage::Copied,
            what: "testing",
            notify: &|_| {},
        };
        match block_on(cache.acquire(&request, |_| None::<()>)).unwrap() {
            Acquired::Build(slot) => slot,
            Acquired::Ready(()) => unreachable!(),
        }
    }

    /// The outputs a build left in `dir`: an interface and an archive.
    fn outputs_in(dir: &Path) -> Vec<PathBuf> {
        std::fs::create_dir_all(dir).unwrap();
        let rri = dir.join("core.rri");
        let archive = dir.join("libcore.a");
        std::fs::write(&rri, "interface").unwrap();
        std::fs::write(&archive, "archive").unwrap();
        vec![rri, archive]
    }

    #[test]
    fn a_published_entry_restores_into_another_build_directory() {
        let tmp = tempfile::tempdir().unwrap();
        let cache = Cache::at(tmp.path().join("cache")).unwrap();
        let first = tmp.path().join("first");
        let sources = vec![source(&tmp.path().join("lib.rr"))];
        block_on(publish(slot(&cache, "k"), &outputs_in(&first), &sources)).unwrap();

        let second = tmp.path().join("second");
        std::fs::create_dir_all(&second).unwrap();
        let entry = cache.entry(&EntryId::new(cache::kind::DEPS, "k"));
        assert_eq!(restore(&entry, &second), Some(Ok(sources)));
        assert_eq!(
            std::fs::read_to_string(second.join("core.rri")).unwrap(),
            "interface"
        );
        assert_eq!(
            std::fs::read_to_string(second.join("libcore.a")).unwrap(),
            "archive"
        );
    }

    /// An entry missing a file, or holding one of another size, is not
    /// restored, and the next publication replaces it.
    #[test]
    fn an_incomplete_entry_is_not_restored() {
        let tmp = tempfile::tempdir().unwrap();
        let cache = Cache::at(tmp.path().join("cache")).unwrap();
        let build = tmp.path().join("build");
        let entry = cache.entry(&EntryId::new(cache::kind::DEPS, "k"));
        block_on(publish(slot(&cache, "k"), &outputs_in(&build), &[])).unwrap();
        std::fs::write(entry.join("libcore.a"), "arch").unwrap();
        assert_eq!(restore(&entry, &build), None);
        std::fs::remove_file(entry.join("core.rri")).unwrap();
        assert_eq!(restore(&entry, &build), None);

        block_on(publish(slot(&cache, "k"), &outputs_in(&build), &[])).unwrap();
        assert_eq!(restore(&entry, &build), Some(Ok(Vec::new())));
    }

    /// Only the declared outputs are shared: a token-reuse report an
    /// earlier build with remarks left beside the archive is not.
    #[test]
    fn a_stale_report_is_not_published() {
        let tmp = tempfile::tempdir().unwrap();
        let cache = Cache::at(tmp.path().join("cache")).unwrap();
        let build = tmp.path().join("build");
        let rri = build.join("core.rri");
        let archive = build.join("libcore.a");
        let commands = [&rri, &archive].map(|out| PlannedCommand {
            emit: "staticlib",
            target: None,
            out: out.clone(),
            report: None,
            args: Vec::new(),
        });
        outputs_in(&build);
        std::fs::write(build.join("libcore.a.token-reuse.json"), "stale").unwrap();
        block_on(publish(slot(&cache, "k"), &outputs(&commands), &[])).unwrap();

        let entry = cache.entry(&EntryId::new(cache::kind::DEPS, "k"));
        let mut names: Vec<String> = std::fs::read_dir(&entry)
            .unwrap()
            .map(|file| file.unwrap().file_name().into_string().unwrap())
            .collect();
        names.sort();
        assert_eq!(names, ["core.rri", "libcore.a", "record.json"]);
    }
}
