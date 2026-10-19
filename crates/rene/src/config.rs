//! The per-user configuration file.
//!
//! `rene` reads `config.ncl` from the platform configuration directory:
//!
//! - Linux and other Unix: `$XDG_CONFIG_HOME/rene`, or `~/.config/rene`;
//! - macOS: `~/Library/Application Support/rene`;
//! - Windows: `%APPDATA%\rene`.
//!
//! [`CONFIG_DIR_ENV`] overrides the directory, for hermetic builds and
//! tests. The file is a Nickel program evaluated exactly like a package
//! manifest (imports, contracts, merging), and a missing file means the
//! defaults. An unknown field is an error, not a silently ignored knob.
//!
//! ```nickel
//! {
//!   cache = {
//!     limit = "10 GiB",   # prune once the cache is larger than this
//!     max_age = "120d",   # entries unused this long may be removed
//!   },
//! }
//! ```
//!
//! Sizes take a number of bytes or a string with a unit (`B`, `KB`, `MB`,
//! `GB`, `TB`, or the binary `KiB`, `MiB`, `GiB`, `TiB`). Ages take a
//! number of seconds or a string with a unit (`s`, `min`, `h`, `d`, `w`,
//! `months`, `y`, singular or plural). `null` for either turns automatic
//! pruning off.

use std::path::{Path, PathBuf};
use std::time::Duration;

use nickel_lang::Context;
use serde::Deserialize;

/// Overrides the configuration directory.
pub const CONFIG_DIR_ENV: &str = "RENE_CONFIG_DIR";

/// The configuration directory's name under the platform directory.
const CONFIG_NAME: &str = "rene";

/// The configuration file's name inside the directory.
pub const CONFIG_FILE: &str = "config.ncl";

/// The cache size above which a build that just added an entry prunes.
pub const DEFAULT_LIMIT: u64 = 10 * 1024 * 1024 * 1024;

/// How long an entry must go unused before it may be removed.
pub const DEFAULT_MAX_AGE: Duration = Duration::from_secs(120 * 24 * 60 * 60);

/// The evaluated configuration.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Config {
    pub cache: CacheConfig,
}

/// The `cache` section.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CacheConfig {
    /// `None` turns automatic pruning off.
    pub limit: Option<u64>,
    /// `None` turns automatic pruning off.
    pub max_age: Option<Duration>,
}

impl Default for Config {
    fn default() -> Self {
        Config {
            cache: CacheConfig {
                limit: Some(DEFAULT_LIMIT),
                max_age: Some(DEFAULT_MAX_AGE),
            },
        }
    }
}

impl Config {
    /// Load the user's configuration, or the defaults when there is no
    /// file. An unreadable or invalid file is an error: a typo must not
    /// silently restore defaults that then delete things.
    pub fn load() -> Result<Config, String> {
        match path() {
            Some(path) if path.is_file() => load_file(&path),
            _ => Ok(Config::default()),
        }
    }
}

/// Where the configuration file is (or would be): [`CONFIG_DIR_ENV`], else
/// the platform directory. `None` when neither is known.
pub fn path() -> Option<PathBuf> {
    let dir = match std::env::var_os(CONFIG_DIR_ENV) {
        Some(dir) if !dir.is_empty() => PathBuf::from(dir),
        _ => dirs::config_dir()?.join(CONFIG_NAME),
    };
    Some(dir.join(CONFIG_FILE))
}

/// The raw shape of the file, before units are parsed.
#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct Raw {
    #[serde(default)]
    cache: RawCache,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct RawCache {
    #[serde(default = "default_limit")]
    limit: Option<Quantity>,
    #[serde(default = "default_max_age")]
    max_age: Option<Quantity>,
}

impl Default for RawCache {
    fn default() -> Self {
        RawCache {
            limit: default_limit(),
            max_age: default_max_age(),
        }
    }
}

fn default_limit() -> Option<Quantity> {
    Some(Quantity::Number(DEFAULT_LIMIT as f64))
}

fn default_max_age() -> Option<Quantity> {
    Some(Quantity::Number(DEFAULT_MAX_AGE.as_secs() as f64))
}

/// A number in the base unit, or a string with a unit.
#[derive(Deserialize)]
#[serde(untagged)]
enum Quantity {
    Number(f64),
    Text(String),
}

/// Evaluate the configuration at `path`.
pub fn load_file(path: &Path) -> Result<Config, String> {
    tracing::debug!(config = %path.display(), "evaluating configuration");
    let source = std::fs::read_to_string(path)
        .map_err(|e| format!("cannot read `{}`: {e}", path.display()))?;
    let mut ctx = Context::new().with_source_name(path.display().to_string());
    if let Some(dir) = path.parent().filter(|d| !d.as_os_str().is_empty()) {
        ctx = ctx.with_added_import_paths(vec![dir.as_os_str().to_owned()]);
    }
    let expr = ctx
        .eval_deep_for_export(&source)
        .map_err(|e| format!("cannot evaluate `{}`:\n{}", path.display(), render(&e)))?;
    let raw: Raw = expr
        .to_serde()
        .map_err(|e| format!("invalid configuration `{}`: {e}", path.display()))?;
    let limit = raw
        .cache
        .limit
        .map(|q| parse_size(&q))
        .transpose()
        .map_err(|e| format!("invalid `cache.limit` in `{}`: {e}", path.display()))?;
    let max_age = raw
        .cache
        .max_age
        .map(|q| parse_age(&q))
        .transpose()
        .map_err(|e| format!("invalid `cache.max_age` in `{}`: {e}", path.display()))?;
    Ok(Config {
        cache: CacheConfig { limit, max_age },
    })
}

fn render(error: &nickel_lang::Error) -> String {
    let mut out = Vec::new();
    match error.format(&mut out, nickel_lang::ErrorFormat::Text) {
        Ok(()) => String::from_utf8_lossy(&out).trim_end().to_owned(),
        Err(e) => format!("(diagnostics unavailable: {e})"),
    }
}

/// Split `text` into its number and its unit, both trimmed.
fn split_unit(text: &str) -> Result<(f64, &str), String> {
    let text = text.trim();
    let digits = text
        .find(|c: char| !(c.is_ascii_digit() || c == '.'))
        .unwrap_or(text.len());
    let (number, unit) = text.split_at(digits);
    let number: f64 = number
        .parse()
        .map_err(|_| format!("`{text}` does not start with a number"))?;
    if number < 0.0 {
        return Err(format!("`{text}` is negative"));
    }
    Ok((number, unit.trim()))
}

/// Bytes, from a number or a string such as `"10 GiB"` or `"500MB"`.
fn parse_size(quantity: &Quantity) -> Result<u64, String> {
    match quantity {
        Quantity::Number(n) if *n >= 0.0 => Ok(*n as u64),
        Quantity::Number(n) => Err(format!("`{n}` is negative")),
        Quantity::Text(text) => {
            let (number, unit) = split_unit(text)?;
            let scale: f64 = match unit.to_ascii_lowercase().as_str() {
                "" | "b" => 1.0,
                "kb" => 1e3,
                "mb" => 1e6,
                "gb" => 1e9,
                "tb" => 1e12,
                "kib" => 1024.0,
                "mib" => 1024.0 * 1024.0,
                "gib" => 1024.0 * 1024.0 * 1024.0,
                "tib" => 1024.0 * 1024.0 * 1024.0 * 1024.0,
                other => return Err(format!("unknown size unit `{other}` in `{text}`")),
            };
            Ok((number * scale) as u64)
        }
    }
}

/// An age, from a number of seconds or a string such as `"120d"`,
/// `"4 months"`, or `"1 year"`.
fn parse_age(quantity: &Quantity) -> Result<Duration, String> {
    match quantity {
        Quantity::Number(n) if *n >= 0.0 => Ok(Duration::from_secs_f64(*n)),
        Quantity::Number(n) => Err(format!("`{n}` is negative")),
        Quantity::Text(text) => {
            let (number, unit) = split_unit(text)?;
            let unit = unit.to_ascii_lowercase();
            let unit = unit.strip_suffix('s').unwrap_or(&unit);
            let seconds: f64 = match unit {
                "" | "sec" | "second" => 1.0,
                "min" | "minute" => 60.0,
                "h" | "hr" | "hour" => 3600.0,
                "d" | "day" => 86_400.0,
                "w" | "week" => 7.0 * 86_400.0,
                "mo" | "month" => 30.0 * 86_400.0,
                "y" | "year" => 365.0 * 86_400.0,
                other => return Err(format!("unknown age unit `{other}` in `{text}`")),
            };
            Ok(Duration::from_secs_f64(number * seconds))
        }
    }
}

/// Parse an age given on the command line (`--max-age 30d`).
pub fn parse_age_text(text: &str) -> Result<Duration, String> {
    parse_age(&Quantity::Text(text.to_owned()))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn write(dir: &Path, text: &str) -> PathBuf {
        let path = dir.join(CONFIG_FILE);
        std::fs::write(&path, text).unwrap();
        path
    }

    #[test]
    fn the_defaults_prune_automatically() {
        let config = Config::default();
        assert_eq!(config.cache.limit, Some(10 * 1024 * 1024 * 1024));
        assert_eq!(
            config.cache.max_age,
            Some(Duration::from_secs(120 * 86_400))
        );
        assert_eq!(
            parse_age_text("30d").unwrap(),
            Duration::from_secs(30 * 86_400)
        );
    }

    #[test]
    fn units_are_understood() {
        let tmp = tempfile::tempdir().unwrap();
        let path = write(
            tmp.path(),
            "{ cache = { limit = \"2 GiB\", max_age = \"4 months\" } }",
        );
        let config = load_file(&path).unwrap();
        assert_eq!(config.cache.limit, Some(2 * 1024 * 1024 * 1024));
        assert_eq!(
            config.cache.max_age,
            Some(Duration::from_secs(4 * 30 * 86_400))
        );

        let path = write(tmp.path(), "{ cache.limit = 4096, cache.max_age = 90 }");
        let config = load_file(&path).unwrap();
        assert_eq!(config.cache.limit, Some(4096));
        assert_eq!(config.cache.max_age, Some(Duration::from_secs(90)));

        let path = write(tmp.path(), "{ cache.max_age = \"0s\" }");
        let config = load_file(&path).unwrap();
        assert_eq!(config.cache.limit, Some(DEFAULT_LIMIT));
        assert_eq!(config.cache.max_age, Some(Duration::ZERO));
    }

    #[test]
    fn null_turns_pruning_off() {
        let tmp = tempfile::tempdir().unwrap();
        let path = write(tmp.path(), "{ cache = { limit = null } }");
        let config = load_file(&path).unwrap();
        assert_eq!(config.cache.limit, None);
        assert_eq!(config.cache.max_age, Some(DEFAULT_MAX_AGE));
    }

    #[test]
    fn mistakes_are_errors() {
        let tmp = tempfile::tempdir().unwrap();
        let path = write(tmp.path(), "{ cache = { limti = \"1 GiB\" } }");
        let err = load_file(&path).unwrap_err();
        assert!(err.contains("limti"), "{err}");

        let path = write(tmp.path(), "{ cache = { limit = \"ten\" } }");
        let err = load_file(&path).unwrap_err();
        assert!(err.contains("cache.limit"), "{err}");

        let path = write(tmp.path(), "{ cache = { max_age = \"3 fortnights\" } }");
        let err = load_file(&path).unwrap_err();
        assert!(err.contains("fortnight"), "{err}");

        let path = write(tmp.path(), "{ cache = { max_age = 1 + } }");
        let err = load_file(&path).unwrap_err();
        assert!(err.contains("cannot evaluate"), "{err}");
    }
}
