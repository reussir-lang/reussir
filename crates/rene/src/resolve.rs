//! Dependency resolution over the local path graph.
//!
//! Every dependency is a filesystem path today, so each package exists in
//! exactly one version — its manifest's `package.version`. Resolution is
//! therefore a **feasibility check**, not solution finding: load the
//! transitive graph, hand pubgrub a universe with one version per package,
//! and let it verify every constraint is satisfiable together (or explain
//! the conflict). Running the real solver now, instead of a hand-rolled
//! constraint loop, is deliberate — it pins the manifest schema and the
//! error-reporting surface to what a future registry (many candidate
//! versions per package) will need, with only the provider changing.

use std::collections::BTreeMap;
use std::path::{Path, PathBuf};

use pubgrub::{
    DefaultStringReporter, OfflineDependencyProvider, PubGrubError, Ranges, Reporter,
    SemanticVersion,
};

use crate::manifest::{self, Loaded};

/// One package of the loaded graph.
#[derive(Debug)]
pub struct Node {
    /// The manifest directory (canonical), the package's identity on disk.
    pub dir: PathBuf,
    /// The declared `package.version` (defaulted to `0.0.0`).
    pub version: SemanticVersion,
    /// The evaluated manifest.
    pub loaded: Loaded,
    /// Dependency names, in name order (the manifest map is sorted).
    pub dependencies: Vec<String>,
}

/// The transitive path-dependency graph, keyed by package name. One
/// namespace: two directories claiming one name is an error, as is a
/// dependency key that differs from the package's own declared name.
#[derive(Debug)]
pub struct Graph {
    /// The root package's name.
    pub root: String,
    pub nodes: BTreeMap<String, Node>,
}

/// The checked outcome: every package pinned to its (single) version, in
/// name order.
#[derive(Debug)]
pub struct Solution {
    pub pinned: BTreeMap<String, SemanticVersion>,
}

/// The bundled package every package depends on unless it opts out.
const CORE: &str = "core";

/// Load the transitive graph rooted at `root`'s manifest, with no bundled
/// packages: no implicit `core`, and a `bundled = true` dependency is an
/// error.
pub fn load_graph(root: &Loaded) -> Result<Graph, String> {
    let mut loader = Loader::new(root);
    if let Some(name) = loader.next_bundle()? {
        return Err(format!(
            "a dependency asks for the bundled package `{name}`, which this \
             build does not provide"
        ));
    }
    loader.finish()
}

/// Loads the transitive graph rooted at a manifest, asking its driver for
/// each bundled package as the graph first needs it, so a bundle is
/// unpacked only when some package uses it. The driver loops on
/// [`next_bundle`](Self::next_bundle), [`provide`](Self::provide)s each
/// bundle it names, provides `core` if the graph
/// [`wants_core`](Self::wants_core), and [`finish`](Self::finish)es. No
/// manifest is evaluated twice.
pub struct Loader {
    root: String,
    nodes: BTreeMap<String, Node>,
    /// Packages whose manifests are loaded, but not yet their dependencies'.
    queue: Vec<(String, Loaded)>,
    /// The provided bundles' source directories, by package name.
    bundles: BTreeMap<String, PathBuf>,
}

impl Loader {
    pub fn new(root: &Loaded) -> Self {
        let root_name = root.manifest.package.name.clone();
        Loader {
            queue: vec![(root_name.clone(), root.clone())],
            root: root_name,
            nodes: BTreeMap::new(),
            bundles: BTreeMap::new(),
        }
    }

    /// Load every package the graph reaches with the bundles provided so
    /// far, and name the next bundle a dependency asks for
    /// (`bundled = true`) that is not provided yet; `None` once every
    /// package is loaded.
    pub fn next_bundle(&mut self) -> Result<Option<String>, String> {
        while let Some((name, loaded)) = self.queue.pop() {
            let missing = loaded
                .manifest
                .dependencies
                .iter()
                .find(|(dep_name, dep)| dep.bundled && !self.bundles.contains_key(*dep_name))
                .map(|(dep_name, _)| dep_name.clone());
            if let Some(missing) = missing {
                self.queue.push((name, loaded));
                return Ok(Some(missing));
            }
            self.load(name, loaded)?;
        }
        Ok(None)
    }

    /// Supply the source directory of the bundled package `name`.
    pub fn provide(&mut self, name: &str, dir: PathBuf) {
        self.bundles.insert(name.to_owned(), dir);
    }

    /// Whether the loaded graph wants the bundled `core`: some package does
    /// not set `package.no_core`, and none is itself named `core`.
    pub fn wants_core(&self) -> bool {
        !self.nodes.contains_key(CORE)
            && self
                .nodes
                .values()
                .any(|node| !node.loaded.manifest.package.no_core)
    }

    /// The graph, once [`next_bundle`](Self::next_bundle) asks for nothing
    /// more. A provided bundled `core` the graph wants is grafted in: loaded
    /// like any other node and made an implicit dependency of every package
    /// that does not set `package.no_core`. Its edge carries no manifest
    /// entry, so constraint lookups fall back to `*`.
    pub fn finish(mut self) -> Result<Graph, String> {
        if let Some(core_dir) = self.bundles.get(CORE)
            && self.wants_core()
        {
            let manifest = core_dir.join(manifest::MANIFEST_FILE);
            let loaded = manifest::load(&manifest)
                .map_err(|e| format!("loading the bundled core package: {e}"))?;
            let version = declared_version(&loaded)?;
            self.nodes.insert(
                CORE.to_owned(),
                Node {
                    dir: manifest_dir(&loaded)?,
                    version,
                    loaded,
                    dependencies: Vec::new(),
                },
            );
            for (name, node) in self.nodes.iter_mut() {
                if name != CORE
                    && !node.loaded.manifest.package.no_core
                    && !node.dependencies.iter().any(|dep| dep == CORE)
                {
                    node.dependencies.push(CORE.to_owned());
                }
            }
        }
        Ok(Graph {
            root: self.root,
            nodes: self.nodes,
        })
    }

    /// Make `loaded` the node `name`, loading its dependencies' manifests
    /// onto the queue. Every bundle it asks for is provided.
    fn load(&mut self, name: String, loaded: Loaded) -> Result<(), String> {
        let dir = manifest_dir(&loaded)?;
        if let Some(existing) = self.nodes.get(&name) {
            if existing.dir != dir {
                return Err(format!(
                    "two packages claim the name `{name}`: `{}` and `{}`",
                    existing.dir.display(),
                    dir.display()
                ));
            }
            return Ok(());
        }
        let mut dependencies = Vec::new();
        for (dep_name, dep) in &loaded.manifest.dependencies {
            let dep_dir = if dep.bundled {
                self.bundles[dep_name].clone()
            } else {
                dir.join(dep.path())
            };
            let dep_manifest = dep_dir.join(manifest::MANIFEST_FILE);
            let dep_loaded = manifest::load(&dep_manifest).map_err(|e| {
                format!(
                    "loading dependency `{dep_name}` of `{name}` \
                     (`{}`): {e}",
                    dep_manifest.display()
                )
            })?;
            // The key is the name everything references the package by;
            // renaming would silently fork one package into two identities.
            if dep_loaded.manifest.package.name != *dep_name {
                return Err(format!(
                    "dependency `{dep_name}` of `{name}` declares itself \
                     `{}` (in `{}`); the key and the package name must agree",
                    dep_loaded.manifest.package.name,
                    dep_manifest.display()
                ));
            }
            dependencies.push(dep_name.clone());
            self.queue.push((dep_name.clone(), dep_loaded));
        }
        let version = declared_version(&loaded)?;
        self.nodes.insert(
            name,
            Node {
                dir,
                version,
                loaded,
                dependencies,
            },
        );
        Ok(())
    }
}

/// Run the pubgrub feasibility check over the loaded graph.
pub fn check(graph: &Graph) -> Result<Solution, String> {
    let mut provider = OfflineDependencyProvider::<String, Ranges<SemanticVersion>>::new();
    for (name, node) in &graph.nodes {
        let deps: Vec<(String, Ranges<SemanticVersion>)> = node
            .dependencies
            .iter()
            .map(|dep_name| {
                // An implicit `core` edge has no manifest entry: any version.
                let constraint = node
                    .loaded
                    .manifest
                    .dependencies
                    .get(dep_name)
                    .and_then(|d| d.version.as_deref());
                Ok((dep_name.clone(), parse_constraint(constraint)?))
            })
            .collect::<Result<_, String>>()?;
        provider.add_dependencies(name.clone(), node.version, deps);
    }
    let root = &graph.nodes[&graph.root];
    let solution =
        pubgrub::resolve(&provider, graph.root.clone(), root.version).map_err(|err| match err {
            PubGrubError::NoSolution(mut tree) => {
                tree.collapse_no_versions();
                format!(
                    "dependency resolution failed:\n{}",
                    DefaultStringReporter::report(&tree)
                )
            }
            other => format!("dependency resolution failed: {other}"),
        })?;
    Ok(Solution {
        pinned: solution.into_iter().collect(),
    })
}

/// The manifest's directory, canonicalized — the identity that detects one
/// package reached through two different relative spellings.
fn manifest_dir(loaded: &Loaded) -> Result<PathBuf, String> {
    let dir = loaded
        .path
        .parent()
        .filter(|d| !d.as_os_str().is_empty())
        .unwrap_or(Path::new("."));
    dir.canonicalize()
        .map_err(|e| format!("cannot canonicalize `{}`: {e}", dir.display()))
}

/// The package's declared version, `0.0.0` when absent.
fn declared_version(loaded: &Loaded) -> Result<SemanticVersion, String> {
    match &loaded.manifest.package.version {
        None => Ok(SemanticVersion::zero()),
        Some(text) => text.parse().map_err(|e| {
            format!(
                "package `{}` declares version `{text}`: {e}",
                loaded.manifest.package.name
            )
        }),
    }
}

/// Parse a manifest constraint into a version range: `*`/absent (any),
/// `1.2.3` (exact), `^x[.y[.z]]` (compatible: up to the next breaking
/// version, semver's caret rule), `1.2` / `1` (prefix).
fn parse_constraint(text: Option<&str>) -> Result<Ranges<SemanticVersion>, String> {
    let Some(text) = text else {
        return Ok(Ranges::full());
    };
    let text = text.trim();
    if text == "*" {
        return Ok(Ranges::full());
    }
    let parse = |t: &str| -> Result<SemanticVersion, String> {
        t.parse()
            .map_err(|e| format!("invalid version constraint `{text}`: {e}"))
    };
    if let Some(rest) = text.strip_prefix('^') {
        let (low, given) = parse_prefix(rest, &parse)?;
        return Ok(Ranges::between(low, bump_breaking(low, given)));
    }
    let (low, given) = parse_prefix(text, &parse)?;
    match given {
        3 => Ok(Ranges::singleton(low)),
        2 => Ok(Ranges::between(low, low.bump_minor())),
        _ => Ok(Ranges::between(low, low.bump_major())),
    }
}

/// Parse a possibly-partial version (`1`, `1.2`, `1.2.3`), zero-padded, and
/// how many components were given.
fn parse_prefix(
    text: &str,
    parse: &impl Fn(&str) -> Result<SemanticVersion, String>,
) -> Result<(SemanticVersion, usize), String> {
    let given = text.split('.').count();
    let padded = match given {
        3 => text.to_owned(),
        2 => format!("{text}.0"),
        1 => format!("{text}.0.0"),
        _ => {
            return Err(format!(
                "invalid version constraint `{text}`: expected `x`, `x.y`, \
                 `x.y.z`, optionally behind `^`, or `*`"
            ));
        }
    };
    Ok((parse(&padded)?, given))
}

/// The smallest version `^low` excludes: the next breaking one under
/// semver's caret rule, honoring how many components were written
/// (`^1.2.3` < 2.0.0, `^0.2` < 0.3.0, `^0.0.3` < 0.0.4, `^0` < 1.0.0,
/// `^0.0` < 0.1.0).
fn bump_breaking(low: SemanticVersion, given: usize) -> SemanticVersion {
    let (major, minor, _patch): (u32, u32, u32) = low.into();
    match (major, minor, given) {
        (0, 0, 3) => low.bump_patch(),
        (0, 0, 2) => low.bump_minor(),
        (0, 0, _) => low.bump_major(),
        (0, _, _) => low.bump_minor(),
        _ => low.bump_major(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn write_pkg(
        dir: &Path,
        name: &str,
        version: Option<&str>,
        deps: &[(&str, &str, Option<&str>)],
    ) {
        std::fs::create_dir_all(dir).unwrap();
        let version = version.map_or(String::new(), |v| format!("version = \"{v}\","));
        let deps = deps
            .iter()
            .map(|(dep, path, constraint)| {
                let constraint =
                    constraint.map_or(String::new(), |c| format!(", version = \"{c}\""));
                format!("dependencies.{dep} = {{ path = \"{path}\"{constraint} }},")
            })
            .collect::<Vec<_>>()
            .join("\n");
        std::fs::write(
            dir.join(manifest::MANIFEST_FILE),
            format!("{{ package = {{ name = \"{name}\", {version} }}, {deps} }}"),
        )
        .unwrap();
    }

    fn graph_of(root: &Path) -> Result<Graph, String> {
        let loaded = manifest::load(&root.join(manifest::MANIFEST_FILE)).unwrap();
        load_graph(&loaded)
    }

    /// The transitive graph loads, and the feasibility check pins every
    /// package at its declared version.
    #[test]
    fn a_satisfiable_graph_solves_to_the_declared_versions() {
        let tmp = tempfile::tempdir().unwrap();
        write_pkg(
            &tmp.path().join("app"),
            "app",
            Some("0.1.0"),
            &[("util", "../util", Some("^1.2")), ("logs", "../logs", None)],
        );
        write_pkg(
            &tmp.path().join("util"),
            "util",
            Some("1.2.9"),
            &[("logs", "../logs", Some("0.3"))],
        );
        write_pkg(&tmp.path().join("logs"), "logs", Some("0.3.4"), &[]);

        let graph = graph_of(&tmp.path().join("app")).unwrap();
        assert_eq!(graph.root, "app");
        assert_eq!(graph.nodes.len(), 3);
        let solution = check(&graph).unwrap();
        assert_eq!(solution.pinned["app"].to_string(), "0.1.0");
        assert_eq!(solution.pinned["util"].to_string(), "1.2.9");
        assert_eq!(solution.pinned["logs"].to_string(), "0.3.4");
    }

    /// An unsatisfiable constraint fails with pubgrub's derivation, naming
    /// the packages involved.
    #[test]
    fn an_unsatisfiable_constraint_reports_the_conflict() {
        let tmp = tempfile::tempdir().unwrap();
        write_pkg(
            &tmp.path().join("app"),
            "app",
            Some("0.1.0"),
            &[("util", "../util", Some("^2.0.0"))],
        );
        write_pkg(&tmp.path().join("util"), "util", Some("1.2.9"), &[]);

        let err = check(&graph_of(&tmp.path().join("app")).unwrap()).unwrap_err();
        assert!(err.contains("dependency resolution failed"), "{err}");
        assert!(err.contains("util"), "{err}");
    }

    /// A shared dependency reached through two spellings is one node; a key
    /// that contradicts the package's declared name is refused.
    #[test]
    fn identities_are_by_name_and_directory() {
        let tmp = tempfile::tempdir().unwrap();
        write_pkg(
            &tmp.path().join("app"),
            "app",
            None,
            &[("mislabeled", "../util", None)],
        );
        write_pkg(&tmp.path().join("util"), "util", Some("1.0.0"), &[]);
        let err = graph_of(&tmp.path().join("app")).unwrap_err();
        assert!(
            err.contains("the key and the package name must agree"),
            "{err}"
        );
    }

    /// A `bundled = true` dependency loads from the directory its driver
    /// provides, once asked for; without bundles it is refused by name. The
    /// bundled `core` is wanted only while some package does not opt out.
    #[test]
    fn bundles_are_asked_for_as_the_graph_needs_them() {
        let tmp = tempfile::tempdir().unwrap();
        let app = tmp.path().join("app");
        std::fs::create_dir_all(&app).unwrap();
        std::fs::write(
            app.join(manifest::MANIFEST_FILE),
            r#"{ package.name = "app", dependencies.std = { bundled = true, version = "^0.1" } }"#,
        )
        .unwrap();
        let bundle = tmp.path().join("cache/std-src/abc");
        write_pkg(&bundle, "std", Some("0.1.0"), &[]);
        let loaded = manifest::load(&app.join(manifest::MANIFEST_FILE)).unwrap();

        let mut loader = Loader::new(&loaded);
        assert_eq!(loader.next_bundle().unwrap().as_deref(), Some("std"));
        loader.provide("std", bundle.clone());
        assert_eq!(loader.next_bundle().unwrap(), None);
        assert!(loader.wants_core());
        let graph = loader.finish().unwrap();
        assert_eq!(graph.nodes["app"].dependencies, ["std"]);
        assert_eq!(graph.nodes["std"].dir, bundle.canonicalize().unwrap());
        check(&graph).unwrap();

        let err = load_graph(&loaded).unwrap_err();
        assert!(err.contains("the bundled package `std`"), "{err}");

        std::fs::write(
            app.join(manifest::MANIFEST_FILE),
            r#"{ package = { name = "app", no_core = true } }"#,
        )
        .unwrap();
        let loaded = manifest::load(&app.join(manifest::MANIFEST_FILE)).unwrap();
        let mut loader = Loader::new(&loaded);
        assert_eq!(loader.next_bundle().unwrap(), None);
        assert!(!loader.wants_core());
    }

    /// The constraint grammar, at its edges.
    #[test]
    fn constraints_parse_to_semver_ranges() {
        let v = |t: &str| t.parse::<SemanticVersion>().unwrap();
        let within = |c: Option<&str>, t: &str| parse_constraint(c).unwrap().contains(&v(t));
        assert!(within(None, "9.9.9"));
        assert!(within(Some("*"), "0.0.1"));
        assert!(within(Some("1.2.3"), "1.2.3") && !within(Some("1.2.3"), "1.2.4"));
        assert!(within(Some("^1.2.3"), "1.9.0") && !within(Some("^1.2.3"), "2.0.0"));
        assert!(within(Some("^0.2.3"), "0.2.9") && !within(Some("^0.2.3"), "0.3.0"));
        assert!(within(Some("^0.0.3"), "0.0.3") && !within(Some("^0.0.3"), "0.0.4"));
        assert!(within(Some("1.2"), "1.2.7") && !within(Some("1.2"), "1.3.0"));
        assert!(within(Some("1"), "1.9.9") && !within(Some("1"), "2.0.0"));
        assert!(within(Some("^1.2"), "1.9.0") && !within(Some("^1.2"), "2.0.0"));
        assert!(!within(Some("^1.2"), "1.1.9"));
        assert!(within(Some("^0"), "0.9.9") && !within(Some("^0"), "1.0.0"));
        assert!(within(Some("^0.0"), "0.0.9") && !within(Some("^0.0"), "0.1.0"));
        assert!(parse_constraint(Some("nope")).is_err());
        assert!(parse_constraint(Some("1.2.3.4")).is_err());
    }
}
