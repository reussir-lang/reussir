//! Checked compile-time launch settings. Placement remains a separate descriptor.

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct LaunchConfig {
    pub backend_optimization_level: Option<u8>,
    pub disable_donation: bool,
    /// Bit `i` marks kernel argument `i` as a constant: each launch reads its
    /// value from the argument buffer and specializes the kernel on it, so a
    /// new value compiles a new program.
    pub constants: u64,
}

impl LaunchConfig {
    /// Positions of the constant kernel arguments, in increasing order.
    pub fn constant_positions(self) -> impl Iterator<Item = u32> {
        (0..u64::BITS).filter(move |&i| self.constants & (1 << i) != 0)
    }

    /// Canonical textual metadata, shared by HIR, RRI, and MIR.
    pub fn encode(self) -> String {
        format!(
            "optimization={},donation={},constants={}",
            self.backend_optimization_level
                .map_or_else(|| "default".into(), |n| n.to_string()),
            if self.disable_donation {
                "none"
            } else {
                "auto"
            },
            self.constant_positions()
                .map(|i| i.to_string())
                .collect::<Vec<_>>()
                .join(";")
        )
    }

    pub fn parse(text: &str) -> Result<Self, &'static str> {
        let Some((level, rest)) = text
            .strip_prefix("optimization=")
            .and_then(|s| s.split_once(",donation="))
        else {
            return Err("invalid launch configuration metadata");
        };
        let Some((donation, constants)) = rest.split_once(",constants=") else {
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
        let constants = if constants.is_empty() {
            0
        } else {
            parse_positions(constants.split(';'))?
        };
        Ok(Self {
            backend_optimization_level,
            disable_donation,
            constants,
        })
    }
}

/// Parse distinct argument positions below 64 into a position set.
pub fn parse_positions<'a>(items: impl IntoIterator<Item = &'a str>) -> Result<u64, &'static str> {
    let mut set = 0u64;
    for item in items {
        let position = item
            .trim()
            .parse::<u32>()
            .ok()
            .filter(|&i| i < u64::BITS)
            .ok_or("constant launch arguments must be argument positions below 64")?;
        if set & (1 << position) != 0 {
            return Err("constant launch arguments must be distinct");
        }
        set |= 1 << position;
    }
    Ok(set)
}

#[cfg(test)]
mod tests {
    use super::LaunchConfig;
    #[test]
    fn metadata_is_checked_and_roundtrips() {
        for level in [None, Some(0), Some(1), Some(2), Some(3)] {
            for disable_donation in [false, true] {
                for constants in [0, 0b1, 0b1010, 1 << 63] {
                    let config = LaunchConfig {
                        backend_optimization_level: level,
                        disable_donation,
                        constants,
                    };
                    assert_eq!(LaunchConfig::parse(&config.encode()), Ok(config));
                }
            }
        }
        for text in [
            "",
            "optimization=default,donation=auto",
            "optimization=4,donation=auto,constants=",
            "optimization=-1,donation=none,constants=",
            "optimization=default,donation=force,constants=",
            "optimization=2,donation=none,constants=,extra=true",
            "optimization=2,donation=none,constants=1;1",
            "optimization=2,donation=none,constants=x",
            "optimization=2,donation=none,constants=64",
        ] {
            assert!(LaunchConfig::parse(text).is_err(), "{text}");
        }
    }
}
