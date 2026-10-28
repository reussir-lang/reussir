//! Typed failures propagated to the CLI without erasing their causes.

use std::path::{Path, PathBuf};
use std::process::ExitStatus;

pub type Result<T> = std::result::Result<T, Error>;
type Provider =
    pubgrub::OfflineDependencyProvider<String, pubgrub::Ranges<pubgrub::SemanticVersion>>;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error(transparent)]
    Io(#[from] std::io::Error),
    #[error(transparent)]
    Json(#[from] serde_json::Error),
    #[error(transparent)]
    Database(#[from] crate::db::DbError),
    #[error(transparent)]
    Manifest(#[from] crate::manifest::ManifestError),
    #[error(transparent)]
    ProgramLookup(#[from] which::Error),
    #[error("compiler cache (RENE_CACHE_DIR): {0}")]
    Cache(#[from] compcache::Error),
    #[error("cannot {operation} `{}`: {source}", path.display())]
    File {
        operation: &'static str,
        path: PathBuf,
        #[source]
        source: std::io::Error,
    },
    #[error("cannot run `{}`: {source}", program.display())]
    Spawn {
        program: PathBuf,
        #[source]
        source: std::io::Error,
    },
    #[error("`{} {args}` failed ({status}):\n{diagnostics}", program.display())]
    CommandFailed {
        program: PathBuf,
        args: String,
        status: ExitStatus,
        diagnostics: String,
    },
    #[error("compiling `{}` failed (see rrc's diagnostics above)", output.display())]
    CompilationFailed { output: PathBuf, status: ExitStatus },
    #[error("building reussir-rt failed ({status}):\n{diagnostics}")]
    RuntimeBuildFailed {
        status: ExitStatus,
        diagnostics: String,
    },
    #[error("cargo succeeded but did not report the runtime's rlib and staticlib")]
    MissingRuntimeArtifacts,
    #[error("`{} -vV` reported no `host:` line:\n{output}", rustc.display())]
    MissingHost { rustc: PathBuf, output: String },
    #[error(
        "invalid target triple `{target}`: custom target JSON specifications are not supported"
    )]
    CustomTargetUnsupported { target: String },
    #[error(
        "invalid target triple `{target}`: expected only ASCII letters, digits, `-`, `_`, or `.`"
    )]
    InvalidTarget { target: String },
    #[error("cached artifact `{}` is outside `{}`: {source}", path.display(), root.display())]
    ArtifactOutsideRoot {
        path: PathBuf,
        root: PathBuf,
        #[source]
        source: std::path::StripPrefixError,
    },
    #[error("{stage} worker panicked")]
    WorkerPanicked { stage: &'static str },
    #[error("current dependency `{package}` has no recorded sources")]
    MissingRecordedSources { package: String },
    #[error("dependency artifacts already selected for `{package}`")]
    ArtifactsAlreadySelected { package: String },
    #[error("target `{name}` missing from the plan")]
    MissingPlannedTarget { name: String },
    #[error("recording a build needs an open build directory")]
    MissingBuildDirectory,
    #[error("bundle `{package}` has no manifest")]
    MissingBundleManifest { package: &'static str },
    #[error("no `{}` found in `{}` or any parent directory", crate::manifest::MANIFEST_FILE, directory.display())]
    ManifestNotFound { directory: PathBuf },
    #[error("build directory `{}` has a pending or interrupted clean; run `rene clean` first", path.display())]
    PendingClean { path: PathBuf },
    #[error(
        "no profile named `{name}`: the manifest declares none by that name and it is not a built-in (`dev`, `release`)"
    )]
    UnknownProfile { name: String },
    #[error("no {group} target named `{name}`: {available}")]
    UnknownSelection {
        name: String,
        group: &'static str,
        available: String,
    },
    #[error("target `{name}` is {actual}, not {expected}; select it with `{selection} {name}`")]
    SelectionKind {
        name: String,
        actual: &'static str,
        expected: &'static str,
        selection: &'static str,
    },
    #[error("source `{path}` has different module identities under separate target roots")]
    ConflictingSourceIdentity { path: String },
    #[error("package source root `{}` does not exist; a package's crate root is `src/lib.rr`", path.display())]
    MissingSourceRoot { path: PathBuf },
    #[error("target crate root `{}` does not exist or is not a file", path.display())]
    MissingCrateRoot { path: PathBuf },
    #[error("`{} --scan-deps` printed invalid UTF-8: {source}", program.display())]
    ScanUtf8 {
        program: PathBuf,
        #[source]
        source: std::string::FromUtf8Error,
    },
    #[error("cannot parse the `--scan-deps` output: {0}")]
    ScanJson(#[source] serde_json::Error),
    #[error("the `--scan-deps` output has no `{field}` {kind}")]
    MissingScanField {
        field: &'static str,
        kind: &'static str,
    },
    #[error("two packages claim the name `{name}`: `{}` and `{}`", first.display(), second.display())]
    ConflictingPackages {
        name: String,
        first: PathBuf,
        second: PathBuf,
    },
    #[error("loading dependency `{dependency}` of `{package}` (`{}`): {source}", path.display())]
    DependencyManifest {
        dependency: String,
        package: String,
        path: PathBuf,
        #[source]
        source: crate::manifest::ManifestError,
    },
    #[error("dependency `{dependency}` of `{package}` declares itself `{declared}` (in `{}`); the key and the package name must agree", path.display())]
    DependencyName {
        dependency: String,
        package: String,
        declared: String,
        path: PathBuf,
    },
    #[error("loading the bundled core package: {0}")]
    CoreManifest(#[source] crate::manifest::ManifestError),
    #[error("package `{package}` declares version `{version}`: {source}")]
    PackageVersion {
        package: String,
        version: String,
        #[source]
        source: pubgrub::VersionParseError,
    },
    #[error("invalid version constraint `{constraint}`: {source}")]
    VersionConstraint {
        constraint: String,
        #[source]
        source: pubgrub::VersionParseError,
    },
    #[error(
        "invalid version constraint `{constraint}`: expected `x`, `x.y`, `x.y.z`, optionally behind `^`, or `*`"
    )]
    InvalidConstraintShape { constraint: String },
    #[error("dependency resolution failed:\n{diagnostic}")]
    Resolution {
        diagnostic: String,
        #[source]
        source: Box<pubgrub::PubGrubError<Provider>>,
    },
    #[error("dependency `{dependency}` has no source path")]
    MissingDependencyPath { dependency: String },
    #[error("bundled std resolution requires a build directory")]
    MissingBundleDirectory,
    #[error("`{}` exists and is not a directory", path.display())]
    DestinationNotDirectory { path: PathBuf },
    #[error("destination `{}` already exists and is not empty", path.display())]
    DestinationNotEmpty { path: PathBuf },
    #[error(
        "`{name}` cannot name a package: the name becomes the first segment of every item's module path, so it must be an identifier (`[A-Za-z_][A-Za-z0-9_]*`); pick one with `--name`"
    )]
    InvalidPackageName { name: String },
    #[error(
        "`{value}` is not a plausible {kind}: expected only ASCII letters, digits, `.`, `-`, `_`, or `+`"
    )]
    InvalidLiteral { value: String, kind: String },
    #[error("cannot render the {template} template: {source}")]
    Template {
        template: String,
        #[source]
        source: sailfish::RenderError,
    },
    #[error("cancelled; nothing was created")]
    Cancelled,
    #[error("cannot {operation}: {source}")]
    Terminal {
        operation: &'static str,
        #[source]
        source: std::io::Error,
    },
}

impl Error {
    pub(crate) fn file(operation: &'static str, path: &Path, source: std::io::Error) -> Self {
        Self::File {
            operation,
            path: path.to_owned(),
            source,
        }
    }
    pub(crate) fn spawn(program: &Path, source: std::io::Error) -> Self {
        Self::Spawn {
            program: program.to_owned(),
            source,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::error::Error as _;

    #[test]
    fn cache_production_preserves_typed_causes() {
        let error = Error::Cache(compcache::Error::producer(Error::file(
            "read",
            Path::new("core.rri"),
            std::io::Error::from(std::io::ErrorKind::NotFound),
        )));
        let mut cause = error.source();
        let mut found_file = false;
        let mut found_io = false;
        while let Some(source) = cause {
            if let Some(Error::File { path, .. }) = source.downcast_ref::<Error>() {
                assert_eq!(path, Path::new("core.rri"));
                found_file = true;
            }
            if let Some(io) = source.downcast_ref::<std::io::Error>() {
                assert_eq!(io.kind(), std::io::ErrorKind::NotFound);
                found_io = true;
            }
            cause = source.source();
        }
        assert!(found_file && found_io);
    }
}
