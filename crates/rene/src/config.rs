//! The per-user configuration file (docs/design/cache-ledger.md,
//! "Configuration").
//!
//! `rene` reads `config.ncl` from the platform configuration directory:
//!
//! - Linux and other Unix: `$XDG_CONFIG_HOME/rene`, or `~/.config/rene`;
//! - macOS: `~/Library/Application Support/rene`;
//! - Windows: `%APPDATA%\rene`.
//!
//! [`CONFIG_DIR_ENV`] overrides the directory, for hermetic builds and
//! tests. The file is a Nickel program evaluated like a package manifest
//! ([`crate::nickel`]), and a missing file means the defaults. An unknown
//! field, a bad unit, or a value out of range is an error rather than a
//! silently ignored knob: a typo must not restore defaults that then delete
//! things.
//!
//! ```nickel
//! {
//!   cache = {
//!     limit = "10 GiB",   # a build prunes once the cache is larger than this
//!     max_age = "120d",   # entries unused this long may be removed
//!   },
//! }
//! ```
//!
//! Sizes take a number of bytes or a string with a unit (`B`, `KB`, `MB`,
//! `GB`, `TB`, or the binary `KiB`, `MiB`, `GiB`, `TiB`). Ages take a
//! number of seconds or a string with a unit (`s`, `min`, `h`, `d`, `w`,
//! `months`, `y`, singular or plural). `null` for either setting stops
//! builds from removing entries; it does not stop the hourly repair, which
//! removes only what crashed renes left.

use std::ffi::OsString;
use std::path::{Path, PathBuf};
use std::time::Duration;

use serde::Deserialize;

/// Overrides the configuration directory.
pub const CONFIG_DIR_ENV: &str = "RENE_CONFIG_DIR";

/// The configuration directory's name under the platform directory.
const CONFIG_NAME: &str = "rene";

/// The configuration file's name inside the directory.
pub const CONFIG_FILE: &str = "config.ncl";

/// The cache size above which a build that published an entry prunes.
pub const DEFAULT_LIMIT: u64 = 10 * 1024 * 1024 * 1024;

/// How long an entry must go unused before it may be removed.
pub const DEFAULT_MAX_AGE: Duration = Duration::from_secs(120 * DAY);

const DAY: u64 = 24 * 60 * 60;

/// The evaluated configuration.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Config {
    pub cache: CacheConfig,
}

/// The `cache` section: when builds prune the per-user cache.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CacheConfig {
    /// The total size over which a build that published an entry prunes;
    /// `None`: builds never remove entries.
    pub limit: Option<u64>,
    /// How long an entry must go unused before a prune may remove it;
    /// `None`: builds never remove entries.
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
    /// The user's configuration, or the defaults when there is no file.
    pub fn load() -> Result<Config, String> {
        match path() {
            Some(path) if path.is_file() => load_file(&path),
            _ => Ok(Config::default()),
        }
    }
}

/// Where the configuration file is (or would be): in [`CONFIG_DIR_ENV`],
/// else in the platform directory. `None` when neither is known.
pub fn path() -> Option<PathBuf> {
    config_dir(std::env::var_os(CONFIG_DIR_ENV)).map(|dir| dir.join(CONFIG_FILE))
}

/// The configuration directory, given [`CONFIG_DIR_ENV`]'s value: that
/// directory, made absolute so diagnostics name it wherever rene runs,
/// else the platform directory's `rene`.
fn config_dir(env: Option<OsString>) -> Option<PathBuf> {
    match env {
        Some(dir) if !dir.is_empty() => {
            let dir = PathBuf::from(dir);
            Some(std::path::absolute(&dir).unwrap_or(dir))
        }
        _ => Some(dirs::config_dir()?.join(CONFIG_NAME)),
    }
}

/// The shape of the file, before units are parsed.
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
    Some(Quantity::Number(DEFAULT_MAX_AGE.as_secs_f64()))
}

/// A number in the base unit, or a string with a unit.
#[derive(Deserialize)]
#[serde(untagged)]
enum Quantity {
    Number(f64),
    Text(String),
}

/// Evaluate the configuration file at `path`.
pub fn load_file(path: &Path) -> Result<Config, String> {
    tracing::debug!(config = %path.display(), "evaluating the configuration");
    let source = std::fs::read_to_string(path)
        .map_err(|e| format!("cannot read `{}`: {e}", path.display()))?;
    let raw: Raw = crate::nickel::evaluate(path, &source)
        .map_err(|e| format!("cannot evaluate `{}`:\n{e}", path.display()))?
        .to_serde()
        .map_err(|e| format!("invalid configuration `{}`: {e}", path.display()))?;
    let invalid =
        |field: &str, e: String| format!("invalid `cache.{field}` in `{}`: {e}", path.display());
    let limit = raw.cache.limit.map(|q| size(&q)).transpose();
    let max_age = raw.cache.max_age.map(|q| age(&q)).transpose();
    Ok(Config {
        cache: CacheConfig {
            limit: limit.map_err(|e| invalid("limit", e))?,
            max_age: max_age.map_err(|e| invalid("max_age", e))?,
        },
    })
}

/// Parse an age given on the command line (`--max-age 30d`).
pub fn parse_age(text: &str) -> Result<Duration, String> {
    age(&Quantity::Text(text.to_owned()))
}

/// Bytes, from a number or a string such as `"10 GiB"` or `"500MB"`.
fn size(quantity: &Quantity) -> Result<u64, String> {
    let bytes = match quantity {
        Quantity::Number(n) => non_negative(*n)?,
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
            number * scale
        }
    };
    // `u64::MAX as f64` rounds up to 2^64, which no u64 holds.
    if bytes >= u64::MAX as f64 {
        return Err("too large a size".to_owned());
    }
    Ok(bytes as u64)
}

/// An age, from a number of seconds or a string such as `"120d"`,
/// `"4 months"`, or `"1 year"`.
fn age(quantity: &Quantity) -> Result<Duration, String> {
    let seconds = match quantity {
        Quantity::Number(n) => non_negative(*n)?,
        Quantity::Text(text) => {
            let (number, unit) = split_unit(text)?;
            let unit = unit.to_ascii_lowercase();
            let unit = unit.strip_suffix('s').unwrap_or(&unit);
            let scale = match unit {
                "" | "sec" | "second" => 1,
                "min" | "minute" => 60,
                "h" | "hr" | "hour" => 60 * 60,
                "d" | "day" => DAY,
                "w" | "week" => 7 * DAY,
                "mo" | "month" => 30 * DAY,
                "y" | "year" => 365 * DAY,
                other => return Err(format!("unknown age unit `{other}` in `{text}`")),
            };
            number * scale as f64
        }
    };
    Duration::try_from_secs_f64(seconds).map_err(|_| "too long an age".to_owned())
}

/// `n`, if it is a number no less than zero.
fn non_negative(n: f64) -> Result<f64, String> {
    if n >= 0.0 {
        Ok(n)
    } else {
        Err(format!("`{n}` is not a non-negative number"))
    }
}

/// Split `text` into its number and its unit, both trimmed.
fn split_unit(text: &str) -> Result<(f64, &str), String> {
    let text = text.trim();
    let digits = text
        .find(|c: char| !(c.is_ascii_digit() || c == '.'))
        .unwrap_or(text.len());
    let (number, unit) = text.split_at(digits);
    let number = number
        .parse()
        .map_err(|_| format!("`{text}` does not start with a number"))?;
    Ok((number, unit.trim()))
}

/// `1.5 GiB`, `312.0 MiB`, `17.0 KiB`, `9 B`.
pub fn human_size(bytes: u64) -> String {
    const UNITS: [&str; 5] = ["B", "KiB", "MiB", "GiB", "TiB"];
    let mut value = bytes as f64;
    let mut unit = 0;
    while value >= 1024.0 && unit < UNITS.len() - 1 {
        value /= 1024.0;
        unit += 1;
    }
    if unit == 0 {
        format!("{bytes} B")
    } else {
        format!("{value:.1} {}", UNITS[unit])
    }
}

/// `120 days`, `36 hours`, `90 seconds`: the largest unit that divides the
/// age exactly.
pub fn human_age(age: Duration) -> String {
    let secs = age.as_secs();
    let (n, unit) = [(DAY, "day"), (60 * 60, "hour"), (60, "minute")]
        .into_iter()
        .find(|(scale, _)| secs >= *scale && secs.is_multiple_of(*scale))
        .map_or((secs, "second"), |(scale, unit)| (secs / scale, unit));
    format!("{n} {unit}{}", if n == 1 { "" } else { "s" })
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
        assert_eq!(config.cache.max_age, Some(Duration::from_secs(120 * DAY)));
        let tmp = tempfile::tempdir().unwrap();
        assert_eq!(load_file(&write(tmp.path(), "{}")).unwrap(), config);
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
        assert_eq!(config.cache.max_age, Some(Duration::from_secs(120 * DAY)));

        let path = write(tmp.path(), "{ cache.limit = 4096, cache.max_age = 90 }");
        let config = load_file(&path).unwrap();
        assert_eq!(config.cache.limit, Some(4096));
        assert_eq!(config.cache.max_age, Some(Duration::from_secs(90)));

        let path = write(tmp.path(), "{ cache.max_age = \"0s\" }");
        let config = load_file(&path).unwrap();
        assert_eq!(config.cache.limit, Some(DEFAULT_LIMIT));
        assert_eq!(config.cache.max_age, Some(Duration::ZERO));
        assert_eq!(parse_age("30d").unwrap(), Duration::from_secs(30 * DAY));
        assert_eq!(parse_age("1 year").unwrap(), Duration::from_secs(365 * DAY));
    }

    #[test]
    fn null_is_read_as_none() {
        let tmp = tempfile::tempdir().unwrap();
        let path = write(tmp.path(), "{ cache = { limit = null } }");
        let config = load_file(&path).unwrap();
        assert_eq!(config.cache.limit, None);
        assert_eq!(config.cache.max_age, Some(DEFAULT_MAX_AGE));
    }

    #[test]
    fn mistakes_are_errors() {
        let tmp = tempfile::tempdir().unwrap();
        for (text, expected) in [
            ("{ cache = { limti = \"1 GiB\" } }", "limti"),
            ("{ cash = {} }", "cash"),
            ("{ cache = { limit = \"ten\" } }", "cache.limit"),
            ("{ cache = { limit = -1 } }", "cache.limit"),
            ("{ cache = { max_age = \"3 fortnights\" } }", "fortnight"),
            ("{ cache = { max_age = 1 + } }", "cannot evaluate"),
        ] {
            let err = load_file(&write(tmp.path(), text)).unwrap_err();
            assert!(err.contains(expected), "{text}: {err}");
        }
    }

    /// A value no size or age can hold is a configuration error, not a
    /// panic and not a silently saturated limit.
    #[test]
    fn values_out_of_range_are_errors() {
        let tmp = tempfile::tempdir().unwrap();
        for text in [
            "{ cache.max_age = 1e300 }",
            "{ cache.max_age = \"99999999999999999999y\" }",
            "{ cache.limit = 1e300 }",
            "{ cache.limit = \"99999999999 TiB\" }",
        ] {
            let err = load_file(&write(tmp.path(), text)).unwrap_err();
            assert!(err.contains("too"), "{text}: {err}");
        }
        assert!(parse_age("99999999999999999999y").is_err());
    }

    #[test]
    fn the_override_replaces_the_platform_directory() {
        let dir = config_dir(Some("elsewhere".into())).unwrap();
        assert!(dir.is_absolute() && dir.ends_with("elsewhere"), "{dir:?}");
        // An empty override is no override.
        assert_eq!(config_dir(Some("".into())), config_dir(None));
        if let Some(platform) = dirs::config_dir() {
            assert_eq!(config_dir(None), Some(platform.join("rene")));
        }
    }

    #[test]
    fn sizes_and_ages_read_well() {
        assert_eq!(human_size(9), "9 B");
        assert_eq!(human_size(17 * 1024), "17.0 KiB");
        assert_eq!(human_size(3 * 1024 * 1024 * 1024 / 2), "1.5 GiB");
        assert_eq!(human_age(Duration::from_secs(120 * DAY)), "120 days");
        assert_eq!(human_age(Duration::from_secs(3600)), "1 hour");
        assert_eq!(human_age(Duration::from_secs(90)), "90 seconds");
        assert_eq!(human_age(Duration::ZERO), "0 seconds");
    }
}
