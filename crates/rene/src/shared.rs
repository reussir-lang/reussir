//! Compiled bundled packages, shared through the per-user cache
//! (docs/design/cache-ledger.md, "Consumers").
//!
//! A bundled package (`core`, `std`) is the same source in every build.
//! Before the dependency pipeline compiles one, it claims the package's
//! entry, `deps/<key>` ([`Shared::claim`]). A live entry's files are copied
//! into the build directory's `deps/` and its source graph comes back, so
//! the build records the package as it records a local compile. Otherwise
//! the package compiles locally, and the outputs its commands declare are
//! published with the scanned source graph ([`Publication::publish`]).
//! Either way the build directory has its own copy, so freshness, the plan
//! dump and linking are unchanged. The entry is used [`Usage::Copied`].
//!
//! The key hashes the bundle's content, the planned `rrc` argv with the
//! build directory's `deps/` abstracted (it keeps `--package-root`, and
//! covers the target, the profile's flags, the linker, the polyffi library
//! directories and the token-reuse report), the digests of the upstream
//! artifacts the compile reads, the runtime bake, the identity of the `rrc`
//! binary, and rene's version. The record names the files and holds the
//! source graph relative to the cache root, so it depends only on the key.

use std::collections::BTreeMap;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::time::UNIX_EPOCH;

use async_lock::{Mutex, MutexGuardArc};
use serde::{Deserialize, Serialize};

use crate::cache::{self, Acquired, Cache, Request, Slot, Usage, kind};
use crate::deps::{self, SourceFile};
use crate::fresh;
use crate::ledger::EntryId;
use crate::plan::PlannedCommand;
use crate::rt::RtArtifacts;

/// Stands for the build directory's `deps/` in the hashed argv.
const DEPS_DIR: &str = "<deps-dir>";

/// The bundled packages a build shares, and the cache it shares them
/// through.
pub struct Shared {
    cache: Cache,
    /// The cache root, canonical like the source paths `rrc` reports.
    root: PathBuf,
    /// Package name to its bundle's content hash.
    packages: BTreeMap<String, String>,
    /// [`program_identity`] of `rrc`, taken once per build.
    rrc: String,
    /// Held from a claim until it is restored or published, so that this
    /// session never waits for an entry lock while it holds another one.
    claiming: Arc<Mutex<()>>,
}

/// The dependency node a build is about to compile.
pub struct Node<'a> {
    pub(crate) name: &'a str,
    /// The packages whose artifacts the compile reads: its whole upstream
    /// cone, each one an `--extern`.
    pub(crate) upstream: &'a [String],
    /// The commands that build it ([`crate::plan::node_commands`]).
    pub(crate) commands: &'a [PlannedCommand],
    /// The build directory's `deps/`, where the outputs land.
    pub(crate) deps_dir: &'a Path,
    pub(crate) target: &'a str,
}

/// What [`Shared::claim`] found.
pub enum Claim {
    /// The outputs were copied into the build directory; this is the
    /// package's source graph.
    Restored(Vec<SourceFile>),
    /// No live entry: compile locally, then publish.
    Build(Publication),
}

/// The right to publish a local compile of a shared package: the entry's
/// slot, and the outputs the node's commands declare. Only these are
/// published, never whatever else lies in `deps/`, such as a token-reuse
/// report an earlier build with remarks left there.
#[must_use = "the entry's lock is released when the publication is dropped"]
pub struct Publication {
    slot: Slot,
    outputs: Vec<PathBuf>,
    root: PathBuf,
    claiming: MutexGuardArc<()>,
}

/// An entry's record.
#[derive(Serialize, Deserialize)]
struct Record {
    /// The outputs' file names, in the entry and in `deps/`.
    files: Vec<String>,
    /// The package's source graph, paths relative to the cache root.
    sources: Vec<SourceFile>,
}

impl Shared {
    /// Share the `packages` (name to bundle content hash) through `cache`.
    pub fn new(cache: Cache, packages: BTreeMap<String, String>) -> Shared {
        let root = cache.root();
        Shared {
            root: root.canonicalize().unwrap_or_else(|_| root.to_owned()),
            cache,
            packages,
            rrc: program_identity(&deps::resolve_rrc()),
            claiming: Arc::default(),
        }
    }

    /// Claim the entry of `node` compiled against `rt`, waiting for
    /// another rene compiling it (`notify` says so once a second has
    /// passed). `None` means the node is not shared: compile it locally.
    pub async fn claim(
        &self,
        node: &Node<'_>,
        rt: &RtArtifacts,
        notify: &dyn Fn(&str),
    ) -> Result<Option<Claim>, String> {
        let Some(content) = self.packages.get(node.name) else {
            return Ok(None);
        };
        let claiming = self.claiming.lock_arc().await;
        let name = node.name;
        let request = Request {
            id: EntryId {
                kind: kind::DEPS,
                key: self.key(node, content, rt)?,
            },
            usage: Usage::Copied,
            what: &format!("compiling the bundled {name}"),
            notify,
        };
        let root = &self.root;
        let restore = |entry: &Path, record| restore(entry, record, node.deps_dir, root);
        Ok(Some(match self.cache.acquire(&request, restore).await? {
            Acquired::Ready(sources) => Claim::Restored(sources),
            Acquired::Build(slot) => Claim::Build(Publication {
                slot,
                outputs: outputs(node.commands),
                root: root.clone(),
                claiming,
            }),
        }))
    }

    /// The key of `node`'s entry, its bundle's content hash being
    /// `content`.
    fn key(&self, node: &Node<'_>, content: &str, rt: &RtArtifacts) -> Result<String, String> {
        let deps_dir = node.deps_dir.display().to_string();
        let abstracted = |command: &PlannedCommand| -> Vec<String> {
            let args = command.args.iter();
            args.map(|arg| arg.replace(&deps_dir, DEPS_DIR)).collect()
        };
        let commands: Vec<_> = node.commands.iter().map(abstracted).collect();
        let digest = |dep| fresh::artifact_digest(node.deps_dir, dep, node.target);
        let upstream: BTreeMap<_, _> = node.upstream.iter().map(|dep| (dep, digest(dep))).collect();
        let inputs = serde_json::to_vec(&(commands, upstream, rt))
            .map_err(|e| format!("cannot key the compiled bundled {}: {e}", node.name))?;
        Ok(cache::key([
            b"compiled bundled package".as_slice(),
            env!("CARGO_PKG_VERSION").as_bytes(),
            node.name.as_bytes(),
            content.as_bytes(),
            &inputs,
            self.rrc.as_bytes(),
        ]))
    }
}

impl Publication {
    /// Publish the local compile, whose source graph is `sources`, as the
    /// claimed entry.
    pub async fn publish(self, sources: &[SourceFile]) -> Result<(), String> {
        let Publication {
            slot,
            outputs,
            root,
            claiming,
        } = self;
        let staging = slot.stage()?;
        let mut files = Vec::new();
        for output in &outputs {
            let file = output.file_name().and_then(|file| file.to_str());
            let file = file.ok_or_else(|| format!("`{}` has no file name", output.display()))?;
            let staged = staging.path().join(file);
            std::fs::copy(output, &staged).map_err(|e| copying(output, &staged, e))?;
            files.push(file.to_owned());
        }
        let sources = sources.iter().map(|file| {
            let path = Path::new(&file.path);
            let relative = path.strip_prefix(&root).map_err(|_| {
                let (path, root) = (path.display(), root.display());
                format!("a bundled source, `{path}`, lies outside the cache `{root}`")
            })?;
            Ok(SourceFile {
                path: relative.to_string_lossy().into_owned(),
                record: file.record.clone(),
            })
        });
        let record = Record {
            files,
            sources: sources.collect::<Result<_, String>>()?,
        };
        slot.publish(staging, &record).await?;
        drop(claiming);
        Ok(())
    }
}

/// The files `commands` declare they write: each command's artifact, and
/// the token-reuse report it was asked for.
fn outputs(commands: &[PlannedCommand]) -> Vec<PathBuf> {
    let declared = commands
        .iter()
        .flat_map(|c| std::iter::once(&c.out).chain(&c.report));
    declared.cloned().collect()
}

/// Copy the live entry's files into `deps_dir`, and return its source
/// graph, resolved against the cache root `root`.
fn restore(
    entry: &Path,
    record: Record,
    deps_dir: &Path,
    root: &Path,
) -> Result<Vec<SourceFile>, String> {
    for file in &record.files {
        let (from, to) = (entry.join(file), deps_dir.join(file));
        std::fs::copy(&from, &to).map_err(|e| copying(&from, &to, e))?;
    }
    let sources = record.sources.into_iter().map(|file| SourceFile {
        path: root.join(&file.path).display().to_string(),
        record: file.record,
    });
    Ok(sources.collect())
}

fn copying(from: &Path, to: &Path, e: std::io::Error) -> String {
    let (from, to) = (from.display(), to.display());
    format!("cannot copy `{from}` to `{to}`: {e}")
}

/// A cheap identity of the program `program` names: its resolved path
/// (through `PATH` for a bare name), size and modification time. A rebuilt
/// or replaced `rrc` changes it, so the entries it keys miss instead of
/// serving what the old one compiled.
fn program_identity(program: &Path) -> String {
    let resolved = resolve_program(program).unwrap_or_else(|| program.to_owned());
    let resolved = resolved.canonicalize().unwrap_or(resolved);
    match std::fs::metadata(&resolved) {
        Ok(meta) => {
            let modified = meta.modified().ok();
            let since = modified.and_then(|time| time.duration_since(UNIX_EPOCH).ok());
            let mtime = since.map_or(0, |since| since.as_nanos());
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
        let mut candidate = dir.join(program);
        if cfg!(windows) && candidate.extension().is_none() {
            candidate.set_extension("exe");
        }
        candidate.is_file().then_some(candidate)
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::deps::SourceRecord;

    fn block_on<T>(fut: impl Future<Output = T>) -> T {
        compio::runtime::Runtime::new().unwrap().block_on(fut)
    }

    fn bake(version: &str) -> RtArtifacts {
        let path = PathBuf::from;
        RtArtifacts {
            target: "x86_64-unknown-fake".to_owned(),
            rustc: path("/rustc"),
            rustc_version: version.to_owned(),
            rust_libdir: path("/lib"),
            deps_dirs: Vec::new(),
            staticlib: path("/libreussir_rt.a"),
            rlib: path("/libreussir_rt.rlib"),
        }
    }

    /// Sharing `core` through the cache under `tmp`.
    fn sharing(tmp: &Path) -> Shared {
        let cache = block_on(Cache::open(tmp.join("cache"))).unwrap();
        Shared::new(cache, BTreeMap::from([("core".into(), "content".into())]))
    }

    /// A build directory's `deps/`, and the commands compiling `core`, which
    /// reads `base`, into it.
    struct Build(PathBuf, Vec<PlannedCommand>, Vec<String>);

    impl Build {
        /// `report` asks the archive for its token-reuse report, and `extra`
        /// adds arguments.
        fn new(deps_dir: PathBuf, report: bool, extra: &[&str]) -> Build {
            let (rri, archive) = (deps_dir.join("core.rri"), deps_dir.join("libcore.a"));
            let report = report.then(|| crate::plan::token_reuse_report_path(&archive));
            let mut args = vec![format!("-o{}", archive.display())];
            let remarks = report
                .iter()
                .map(|r| format!("--token-reuse-remarks={}", r.display()));
            args.extend(remarks.chain(extra.iter().map(|arg| arg.to_string())));
            let command = |emit, out: PathBuf, report, args| PlannedCommand {
                emit,
                target: None,
                out,
                report,
                args,
            };
            let interface = vec![format!("-o{}", rri.display())];
            let commands = vec![
                command("rri", rri, None, interface),
                command("staticlib", archive, report, args),
            ];
            Build(deps_dir, commands, vec!["base".to_owned()])
        }

        fn node(&self) -> Node<'_> {
            Node {
                name: "core",
                upstream: &self.2,
                commands: &self.1,
                deps_dir: &self.0,
                target: "x86_64-unknown-fake",
            }
        }

        fn key(&self, shared: &Shared, version: &str) -> String {
            shared.key(&self.node(), "content", &bake(version)).unwrap()
        }
    }

    fn claim(shared: &Shared, build: &Build) -> Claim {
        let claim = block_on(shared.claim(&build.node(), &bake("9"), &|_| {}));
        claim.unwrap().expect("core is shared")
    }

    /// Compile `build` "locally", each output holding its own name, publish
    /// it with `sources`, and return its entry.
    fn publish(shared: &Shared, build: &Build, sources: &[SourceFile]) -> EntryId {
        let Claim::Build(publication) = claim(shared, build) else {
            panic!("an absent entry was restored");
        };
        std::fs::create_dir_all(&build.0).unwrap();
        for output in outputs(&build.1) {
            std::fs::write(&output, output.file_name().unwrap().as_encoded_bytes()).unwrap();
        }
        block_on(publication.publish(sources)).unwrap();
        let key = build.key(shared, "9");
        EntryId {
            kind: kind::DEPS,
            key,
        }
    }

    fn names(dir: &Path) -> Vec<String> {
        let entries = std::fs::read_dir(dir).unwrap();
        let names = entries.map(|entry| entry.unwrap().file_name().into_string().unwrap());
        let mut names: Vec<String> = names.collect();
        names.sort();
        names
    }

    #[test]
    fn a_published_entry_restores_into_another_build_directory() {
        let tmp = tempfile::tempdir().unwrap();
        let shared = sharing(tmp.path());
        let root = &shared.root;
        let source = SourceFile {
            path: root.join("core-src/k/src/lib.rr").display().to_string(),
            record: SourceRecord {
                module: vec!["core".to_owned()],
                mtime_ns: 1,
                size: 2,
                hash: [3; blake3::OUT_LEN],
            },
        };
        let first = Build::new(tmp.path().join("first"), false, &[]);
        let id = publish(&shared, &first, std::slice::from_ref(&source));
        // The record names the files, and holds the graph relative to the root.
        let live = block_on(shared.cache.session().lookup(&id))
            .unwrap()
            .unwrap();
        let record: Record = serde_json::from_str(&live.record).unwrap();
        assert_eq!(record.files, ["core.rri", "libcore.a"]);
        let relative = Path::new(&record.sources[0].path);
        assert_eq!(relative, Path::new("core-src/k/src/lib.rr"));

        // Another build directory has the same key, and restores the entry.
        let second = Build::new(tmp.path().join("second"), false, &[]);
        std::fs::create_dir_all(&second.0).unwrap();
        let Claim::Restored(sources) = claim(&shared, &second) else {
            panic!("a live entry was compiled again");
        };
        assert_eq!(sources, [source]);
        assert_eq!(names(&second.0), ["core.rri", "libcore.a"]);
        assert_eq!(
            std::fs::read(second.0.join("core.rri")).unwrap(),
            b"core.rri"
        );
    }

    #[test]
    fn the_key_changes_with_argv_upstream_digests_the_bake_and_rrc() {
        let tmp = tempfile::tempdir().unwrap();
        let rrc = tmp.path().join("rrc");
        std::fs::write(&rrc, "v1").unwrap();
        let identified = || Shared {
            rrc: program_identity(&rrc),
            ..sharing(tmp.path())
        };
        let shared = identified();
        let build = Build::new(tmp.path().join("deps"), false, &[]);
        let base = build.key(&shared, "9");
        // The build directory's `deps/` is not part of the key.
        let elsewhere = Build::new(tmp.path().join("elsewhere"), false, &[]);
        assert_eq!(elsewhere.key(&shared, "9"), base);

        let optimized = Build::new(tmp.path().join("deps"), false, &["-O3"]);
        let mut keys = vec![base, optimized.key(&shared, "9"), build.key(&shared, "8")];
        std::fs::write(&rrc, "version 2").unwrap();
        keys.push(build.key(&identified(), "9"));
        std::fs::create_dir_all(&build.0).unwrap();
        std::fs::write(build.0.join("base.rri"), "changed").unwrap();
        keys.push(build.key(&shared, "9"));
        let distinct: std::collections::BTreeSet<_> = keys.iter().collect();
        assert_eq!(distinct.len(), keys.len(), "{keys:#?}");
        let absent = program_identity(&tmp.path().join("absent"));
        assert!(absent.ends_with("<missing>"), "{absent}");
    }

    /// Only the declared outputs are shared: a token-reuse report an
    /// earlier build with remarks left beside the archive is not, while one
    /// the commands ask for is.
    #[test]
    fn a_stale_token_reuse_report_is_not_published() {
        let tmp = tempfile::tempdir().unwrap();
        let shared = sharing(tmp.path());
        let root = shared.cache.root();
        let deps_dir = tmp.path().join("deps");
        std::fs::create_dir_all(&deps_dir).unwrap();
        std::fs::write(deps_dir.join("libcore.a.token-reuse.json"), "stale").unwrap();

        let plain = Build::new(deps_dir.clone(), false, &[]);
        let entry = publish(&shared, &plain, &[]).dir(root);
        assert_eq!(names(&entry), ["core.rri", "libcore.a"]);
        let with_report = Build::new(deps_dir, true, &[]);
        let entry = publish(&shared, &with_report, &[]).dir(root);
        let report = "libcore.a.token-reuse.json";
        assert_eq!(names(&entry), ["core.rri", "libcore.a", report]);
    }
}
