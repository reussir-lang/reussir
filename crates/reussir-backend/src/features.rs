//! The optional compiler features this build was configured with.
//!
//! The list is declared once in `cmake/ReussirFeatures.cmake` and reaches Rust
//! through the C API, so the drivers, the frontend and the tests all agree on
//! which features exist and which are enabled.

use std::ffi::{CStr, c_char};

use reussir_backend_sys as sys;

/// One optional feature.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Feature {
    /// The lowercase name, e.g. `tpde`.
    pub name: &'static str,
    /// One line of plain text describing what the feature provides.
    pub description: &'static str,
    /// Whether this build includes the feature.
    pub enabled: bool,
}

/// Returns every optional feature, enabled or not, in declaration order.
pub fn all() -> impl Iterator<Item = Feature> {
    // SAFETY: the C API returns static entries for every index below the count.
    let count = unsafe { sys::reussirFeatureCount() };
    (0..count).map(|index| {
        let raw = unsafe { sys::reussirFeatureGet(index) };
        Feature {
            name: static_str(raw.name),
            description: static_str(raw.description),
            enabled: raw.enabled,
        }
    })
}

/// Returns whether the feature called `name` is enabled.
///
/// # Panics
///
/// Panics if no feature is called `name`: the names are fixed by the build, so
/// an unknown one is a typo in the caller.
pub fn enabled(name: &str) -> bool {
    all()
        .find(|feature| feature.name == name)
        .unwrap_or_else(|| panic!("unknown compiler feature `{name}`"))
        .enabled
}

/// The feature list the drivers append to their `--help`.
pub fn help_section() -> String {
    let features: Vec<_> = all().collect();
    let width = features.iter().map(|f| f.name.len()).max().unwrap_or(0);
    let mut section = String::from("Features:\n");
    for feature in features {
        let status = if feature.enabled {
            "enabled"
        } else {
            "disabled"
        };
        section.push_str(&format!(
            "  {:<width$}  {status:<8}  {}\n",
            feature.name, feature.description
        ));
    }
    section
}

fn static_str(ptr: *const c_char) -> &'static str {
    // SAFETY: the C API hands out pointers to static, NUL-terminated strings.
    unsafe { CStr::from_ptr(ptr) }
        .to_str()
        .expect("feature strings are ASCII")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn features_are_unique_and_described() {
        let features: Vec<_> = all().collect();
        assert!(features.iter().any(|feature| feature.name == "tpde"));
        assert!(features.iter().any(|feature| feature.name == "openxla"));
        for (index, feature) in features.iter().enumerate() {
            assert!(!feature.description.is_empty(), "{feature:?}");
            assert!(
                features[..index]
                    .iter()
                    .all(|other| other.name != feature.name),
                "duplicate feature {feature:?}"
            );
            assert_eq!(enabled(feature.name), feature.enabled);
        }
    }

    #[test]
    fn help_section_lists_every_feature() {
        let section = help_section();
        for feature in all() {
            let status = if feature.enabled {
                "enabled"
            } else {
                "disabled"
            };
            assert!(
                section.lines().any(|line| {
                    let words: Vec<_> = line.split_whitespace().collect();
                    words.starts_with(&[feature.name, status])
                }),
                "{section}"
            );
        }
    }

    #[test]
    #[should_panic(expected = "unknown compiler feature `no-such-feature`")]
    fn unknown_feature_panics() {
        enabled("no-such-feature");
    }
}
