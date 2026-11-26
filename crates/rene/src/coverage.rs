//! Source-based code coverage reports of fuzz targets (`rene coverage`).
//!
//! The coverage build of a fuzz target runs each input of its corpus once
//! (`-runs=0`). At exit, the LLVM profile runtime writes a raw profile.
//! `llvm-profdata` merges the raw profiles, and `llvm-cov` writes the report
//! from the merged profile and the coverage mapping of the executable.
//! Reussir symbols use the Rust v0 mangling, so `llvm-cxxfilt` demangles
//! the function names in the report.

use std::ffi::OsString;
use std::path::{Path, PathBuf};

use compio::process::Command;

use crate::Error;

/// The `llvm-cov` command that writes the report.
#[derive(Debug, Clone, Copy, PartialEq, Eq, palc::ValueEnum)]
pub enum LlvmCov {
    /// A coverage summary for each source file. The default.
    Report,
    /// Each source line with its execution count.
    Show,
    /// The coverage data as JSON.
    Export,
}

impl LlvmCov {
    fn as_str(self) -> &'static str {
        match self {
            LlvmCov::Report => "report",
            LlvmCov::Show => "show",
            LlvmCov::Export => "export",
        }
    }
}

/// The LLVM tools of a report.
pub struct Tools {
    pub profdata: PathBuf,
    pub cov: PathBuf,
    pub demangler: PathBuf,
}

impl Tools {
    /// Find each tool as rene finds `rrc`: the path in its `REUSSIR_`
    /// variable, else the tool name on `PATH`.
    pub fn find() -> Result<Self, Error> {
        Ok(Self {
            profdata: find("REUSSIR_LLVM_PROFDATA", "llvm-profdata")?,
            cov: find("REUSSIR_LLVM_COV", "llvm-cov")?,
            demangler: find("REUSSIR_LLVM_CXXFILT", "llvm-cxxfilt")?,
        })
    }
}

fn find(variable: &'static str, name: &str) -> Result<PathBuf, Error> {
    let program = std::env::var_os(variable).map_or_else(|| PathBuf::from(name), PathBuf::from);
    which::which(&program).map_err(|source| Error::ToolLookup {
        program,
        variable,
        source,
    })
}

/// The profile files of one report, beside the executable as its
/// token-reuse report is: `demo` has `demo.profraw/` and `demo.profdata`.
pub struct Profiles {
    /// The directory of the raw profiles of the last run.
    pub raw: PathBuf,
    /// The merged profile that `llvm-cov` reads.
    pub merged: PathBuf,
}

impl Profiles {
    pub fn new(executable: &Path) -> Self {
        let beside = |suffix: &str| {
            let mut name = executable.file_name().unwrap_or_default().to_os_string();
            name.push(suffix);
            executable.with_file_name(name)
        };
        Self {
            raw: beside(".profraw"),
            merged: beside(".profdata"),
        }
    }
}

/// Run `executable` over `corpus`, merge its raw profiles, and let `llvm-cov`
/// write the report to stdout. `llvm_cov_args` go to `llvm-cov` last.
pub async fn report(
    tools: &Tools,
    executable: &Path,
    corpus: &[PathBuf],
    profiles: &Profiles,
    llvm_cov: LlvmCov,
    llvm_cov_args: &[OsString],
) -> Result<(), Error> {
    // Remove the raw profiles of an earlier run, so that the report shows
    // this corpus only.
    match std::fs::remove_dir_all(&profiles.raw) {
        Err(error) if error.kind() != std::io::ErrorKind::NotFound => {
            return Err(Error::file("remove", &profiles.raw, error));
        }
        _ => {}
    }
    std::fs::create_dir_all(&profiles.raw)
        .map_err(|source| Error::file("create", &profiles.raw, source))?;
    tracing::info!(executable = %executable.display(), "running the corpus");
    status(&mut run_command(executable, corpus, profiles)).await?;
    status(&mut merge_command(tools, profiles)).await?;
    status(&mut report_command(
        tools,
        llvm_cov,
        executable,
        profiles,
        llvm_cov_args,
    ))
    .await
}

async fn status(command: &mut Command) -> Result<(), Error> {
    let program = Path::new(command.get_program()).to_owned();
    let status = command
        .status()
        .await
        .map_err(|source| Error::spawn(&program, source))?;
    if !status.success() {
        return Err(Error::ToolFailed { program, status });
    }
    Ok(())
}

fn run_command(executable: &Path, corpus: &[PathBuf], profiles: &Profiles) -> Command {
    let mut command = Command::new(executable);
    // `%m` names the binary and `%p` the process, so each process writes
    // its own file.
    command
        .env("LLVM_PROFILE_FILE", profiles.raw.join("%m-%p.profraw"))
        .arg("-runs=0")
        .args(corpus);
    // Only the report goes to the stdout of rene.
    command
        .stdout(std::io::stderr())
        .expect("redirect stdout to stderr");
    command
}

fn merge_command(tools: &Tools, profiles: &Profiles) -> Command {
    let mut command = Command::new(&tools.profdata);
    command
        .args(["merge", "-sparse"])
        .arg(&profiles.raw)
        .arg("-o")
        .arg(&profiles.merged);
    command
}

fn report_command(
    tools: &Tools,
    llvm_cov: LlvmCov,
    executable: &Path,
    profiles: &Profiles,
    llvm_cov_args: &[OsString],
) -> Command {
    let mut command = Command::new(&tools.cov);
    command
        .arg(llvm_cov.as_str())
        .arg(executable)
        .arg("-instr-profile")
        .arg(&profiles.merged)
        .arg("-Xdemangler")
        .arg(&tools.demangler)
        .args(llvm_cov_args);
    command
}

#[cfg(test)]
mod tests {
    use super::*;

    fn tools() -> Tools {
        Tools {
            profdata: "/llvm/llvm-profdata".into(),
            cov: "/llvm/llvm-cov".into(),
            demangler: "/llvm/llvm-cxxfilt".into(),
        }
    }

    fn argv(command: &Command) -> Vec<&std::ffi::OsStr> {
        std::iter::once(command.get_program())
            .chain(command.get_args())
            .collect()
    }

    #[test]
    fn the_run_executes_each_corpus_input_once() {
        let profiles = Profiles::new(Path::new("/b/fuzz/coverage/list"));
        let command = run_command(
            Path::new("/b/fuzz/coverage/list"),
            &["seeds".into(), "crashes".into()],
            &profiles,
        );
        assert_eq!(
            argv(&command),
            ["/b/fuzz/coverage/list", "-runs=0", "seeds", "crashes"]
        );
        let profile_file = profiles.raw.join("%m-%p.profraw");
        assert_eq!(
            command.get_envs().collect::<Vec<_>>(),
            [(
                std::ffi::OsStr::new("LLVM_PROFILE_FILE"),
                Some(profile_file.as_os_str())
            )]
        );
    }

    #[test]
    fn the_merge_reads_the_raw_profiles_of_the_run() {
        let profiles = Profiles::new(Path::new("/b/fuzz/coverage/list"));
        assert_eq!(
            argv(&merge_command(&tools(), &profiles)),
            [
                "/llvm/llvm-profdata",
                "merge",
                "-sparse",
                "/b/fuzz/coverage/list.profraw",
                "-o",
                "/b/fuzz/coverage/list.profdata",
            ]
        );
    }

    #[test]
    fn the_report_demangles_and_ends_with_the_extra_arguments() {
        let profiles = Profiles::new(Path::new("/b/fuzz/coverage/list"));
        for (llvm_cov, name) in [
            (LlvmCov::Report, "report"),
            (LlvmCov::Show, "show"),
            (LlvmCov::Export, "export"),
        ] {
            let command = report_command(
                &tools(),
                llvm_cov,
                Path::new("/b/fuzz/coverage/list"),
                &profiles,
                &["-format=lcov".into()],
            );
            assert_eq!(
                argv(&command),
                [
                    "/llvm/llvm-cov",
                    name,
                    "/b/fuzz/coverage/list",
                    "-instr-profile",
                    "/b/fuzz/coverage/list.profdata",
                    "-Xdemangler",
                    "/llvm/llvm-cxxfilt",
                    "-format=lcov",
                ]
            );
        }
    }
}
