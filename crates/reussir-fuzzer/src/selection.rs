//! Case selection retaining the suite's original wire indices.

use crate::spec::SuiteSpec;

pub(crate) struct CaseSelection {
    selected: Vec<bool>,
    pub(crate) fuzz: Vec<usize>,
}

impl CaseSelection {
    pub(crate) fn new(suite: &SuiteSpec, filter: Option<&str>) -> Self {
        let selected: Vec<bool> = suite
            .cases
            .iter()
            .map(|case| filter.is_none_or(|pattern| case.joined_path().contains(pattern)))
            .collect();
        let fuzz = suite
            .cases
            .iter()
            .enumerate()
            .filter_map(|(index, case)| (selected[index] && !case.input.is_unit()).then_some(index))
            .collect();
        Self { selected, fuzz }
    }

    pub(crate) fn contains(&self, index: usize) -> bool {
        self.selected.get(index).copied().unwrap_or(false)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::spec::{CaseSpec, ValueSpec};

    #[test]
    fn preserves_indices_and_excludes_unit_cases_from_mutation() {
        let suite = SuiteSpec {
            cases: (0..100)
                .map(|index| CaseSpec {
                    path: vec![format!("case{index}")],
                    input: if index == 98 {
                        ValueSpec::Unit
                    } else {
                        ValueSpec::Bool
                    },
                })
                .collect(),
        };
        let selected = CaseSelection::new(&suite, Some("case9"));
        assert!(selected.contains(98));
        assert!(selected.contains(99));
        assert!(!selected.contains(0));
        assert!(!selected.fuzz.contains(&98));
        assert!(selected.fuzz.contains(&99));
        let mut bytes = [0; 64];
        for seed in 0..256 {
            let len = suite.mutate_buffer_selected(&mut bytes, 0, 64, seed, None, &selected.fuzz);
            let input = suite.decode_input(&bytes[..len]).unwrap();
            assert!(selected.contains(input.case_idx));
            assert_ne!(input.case_idx, 98);
        }
    }
}
