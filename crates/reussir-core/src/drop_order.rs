//! Destruction order carried by resumable programs.

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub enum DropOrder {
    Forward,
    #[default]
    Backward,
}

impl DropOrder {
    pub const fn as_str(self) -> &'static str {
        match self {
            Self::Forward => "forward",
            Self::Backward => "backward",
        }
    }
}

impl std::fmt::Display for DropOrder {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.as_str())
    }
}

impl std::str::FromStr for DropOrder {
    type Err = String;

    fn from_str(value: &str) -> Result<Self, Self::Err> {
        match value {
            "forward" => Ok(Self::Forward),
            "backward" => Ok(Self::Backward),
            _ => Err(format!(
                "invalid drop order `{value}`; expected forward or backward"
            )),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::DropOrder;
    use crate::{full::mir, semi::hir};

    #[test]
    fn drop_order_roundtrips_in_both_ir_stages() {
        crate::in_arena(|tcx| {
            for order in [DropOrder::Forward, DropOrder::Backward] {
                let text = format!("drop_order {:?};\n", order.as_str());
                let hir = hir::build::parse_program(tcx, &text).unwrap();
                assert_eq!(hir.drop_order, Some(order));
                let printed = hir::print::Printer::new(&hir.defs, &hir.names)
                    .with_drop_order(hir.drop_order)
                    .program(&hir.funcs, &hir.strings, &hir.records, &hir.trampolines);
                assert_eq!(printed, text);
                let mir = mir::build::parse_program(tcx, &text).unwrap();
                assert_eq!(mir.program.drop_order, Some(order));
                assert_eq!(
                    mir::print::Printer::new(&mir.defs, &mir.names).program(&mir.program),
                    text
                );
            }
            for text in [
                "drop_order \"invalid\";",
                "drop_order \"forward\"; drop_order \"backward\";",
            ] {
                assert!(hir::build::parse_program(tcx, text).is_err());
                assert!(mir::build::parse_program(tcx, text).is_err());
            }
            assert_eq!(hir::build::parse_program(tcx, "").unwrap().drop_order, None);
            assert_eq!(
                mir::build::parse_program(tcx, "")
                    .unwrap()
                    .program
                    .drop_order,
                None
            );
        });
    }
}
