//! Checked compile-time launch settings. Placement remains a separate descriptor.

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct LaunchConfig {
    pub backend_optimization_level: Option<u8>,
    pub disable_donation: bool,
}

impl LaunchConfig {
    /// Canonical textual metadata, shared by HIR, RRI, and MIR.
    pub fn encode(self) -> String {
        format!(
            "optimization={},donation={}",
            self.backend_optimization_level
                .map_or_else(|| "default".into(), |n| n.to_string()),
            if self.disable_donation {
                "none"
            } else {
                "auto"
            }
        )
    }

    pub fn parse(text: &str) -> Result<Self, &'static str> {
        let Some((level, donation)) = text
            .strip_prefix("optimization=")
            .and_then(|s| s.split_once(",donation="))
        else {
            return Err("invalid launch configuration metadata");
        };
        let backend_optimization_level = match level {
            "default" => None,
            "0" => Some(0),
            "1" => Some(1),
            "2" => Some(2),
            "3" => Some(3),
            _ => return Err("backend optimization level must be 0, 1, 2, or 3"),
        };
        let disable_donation = match donation {
            "auto" => false,
            "none" => true,
            _ => return Err("donation must be auto or none"),
        };
        Ok(Self {
            backend_optimization_level,
            disable_donation,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::LaunchConfig;
    #[test]
    fn metadata_is_checked_and_roundtrips() {
        for level in [None, Some(0), Some(1), Some(2), Some(3)] {
            for disable_donation in [false, true] {
                let config = LaunchConfig {
                    backend_optimization_level: level,
                    disable_donation,
                };
                assert_eq!(LaunchConfig::parse(&config.encode()), Ok(config));
            }
        }
        for text in [
            "",
            "optimization=4,donation=auto",
            "optimization=-1,donation=none",
            "optimization=default,donation=force",
            "optimization=2,donation=none,extra=true",
        ] {
            assert!(LaunchConfig::parse(text).is_err(), "{text}");
        }
    }
}
