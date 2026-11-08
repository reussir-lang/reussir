//! Wire-size budgets for structured fuzz inputs.

use crate::mutator::{CharsetMutator, FuzzValue, SpecMutator};
use crate::spec::{Charset, SuiteSpec, ValueSpec};
use mutatis::{Session, mutators as m};

impl ValueSpec {
    /// Minimum encoded payload size, or None when it exceeds usize.
    pub fn minimum_wire_size(&self) -> Option<usize> {
        match self {
            Self::Unit => Some(0),
            Self::Bool => Some(1),
            Self::Int { width, .. } | Self::UInt { width, .. } => Some(width.byte_len()),
            Self::Float { width, .. } => Some(width.byte_len()),
            Self::Char(_) => Some(4),
            Self::String {
                min_len, max_len, ..
            } => 8usize.checked_add(usize::try_from((*min_len).min(*max_len)).ok()?),
            Self::Array {
                elem,
                min_len,
                max_len,
            } => {
                let count = usize::try_from((*min_len).min(*max_len)).ok()?;
                if count == 0 {
                    Some(8)
                } else {
                    8usize.checked_add(elem.minimum_wire_size()?.checked_mul(count)?)
                }
            }
            Self::Tuple(items) => items.iter().try_fold(0usize, |sum, item| {
                sum.checked_add(item.minimum_wire_size()?)
            }),
        }
    }

    /// A subset of this domain whose collection bounds fit the payload budget.
    pub(crate) fn within_budget(&self, budget: usize) -> Option<Self> {
        let minimum = self.minimum_wire_size()?;
        if minimum > budget {
            return None;
        }
        Some(match self {
            Self::String {
                charset,
                min_len,
                max_len,
            } => Self::String {
                charset: *charset,
                min_len: (*min_len).min(*max_len),
                max_len: (*min_len).max(*max_len).min((budget - 8) as u64),
            },
            Self::Array {
                elem,
                min_len,
                max_len,
            } => {
                let available = budget - 8;
                let low = (*min_len).min(*max_len);
                let high = (*min_len).max(*max_len);
                let Some(element_min) = elem.minimum_wire_size() else {
                    return Some(Self::Array {
                        elem: elem.clone(),
                        min_len: 0,
                        max_len: 0,
                    });
                };
                // Zero-byte elements need no wire space; choose a finite subset
                // above the declared minimum without changing that minimum.
                let limit = available.checked_div(element_min).map_or_else(
                    || low.saturating_add(available as u64),
                    |limit| limit as u64,
                );
                let count = high.min(limit);
                let bounded_elem = if count == 0 {
                    (**elem).clone()
                } else {
                    elem.within_budget(available / usize::try_from(count).ok()?)?
                };
                Self::Array {
                    elem: Box::new(bounded_elem),
                    min_len: low,
                    max_len: count,
                }
            }
            Self::Tuple(items) => {
                let extra = budget - minimum;
                let share = extra.checked_div(items.len()).unwrap_or(0);
                let bounded = items
                    .iter()
                    .map(|item| item.within_budget(item.minimum_wire_size()?.checked_add(share)?))
                    .collect::<Option<Vec<_>>>()?;
                Self::Tuple(bounded)
            }
            other => other.clone(),
        })
    }
}

/// Error returned when a selected fuzz case's declared input domain cannot fit
/// within the configured wire-size budget.
#[derive(Clone, Debug, PartialEq, Eq, thiserror::Error)]
pub enum InputBudgetError {
    /// Selected case index is out of bounds for the suite.
    #[error("invalid fuzz case index {index}")]
    InvalidCaseIndex { index: usize },
    /// Selected case requires more bytes than the configured `-max_len` budget.
    #[error(
        "fuzz case `{case}` requires at least {minimum} bytes, exceeding -max_len={budget}; increase -max_len or reduce the declared input domain"
    )]
    ExceedsBudget {
        case: String,
        minimum: usize,
        budget: usize,
    },
    /// Selected case's minimum wire size overflows `usize`.
    #[error(
        "fuzz case `{case}` requires at least more than usize::MAX bytes, exceeding -max_len={budget}; increase -max_len or reduce the declared input domain"
    )]
    MinimumWireSizeOverflow { case: String, budget: usize },
}

impl SuiteSpec {
    /// Validates the complete selected inputs, including their case selectors.
    pub fn validate_input_budget(
        &self,
        indices: &[usize],
        budget: usize,
    ) -> Result<(), InputBudgetError> {
        for &index in indices {
            let case = self
                .cases
                .get(index)
                .ok_or(InputBudgetError::InvalidCaseIndex { index })?;
            let Some(minimum) = case
                .input
                .minimum_wire_size()
                .and_then(|n| n.checked_add(8))
            else {
                return Err(InputBudgetError::MinimumWireSizeOverflow {
                    case: case.joined_path(),
                    budget,
                });
            };
            if minimum > budget {
                return Err(InputBudgetError::ExceedsBudget {
                    case: case.joined_path(),
                    minimum,
                    budget,
                });
            }
        }
        Ok(())
    }
}

fn wire_size(value: &FuzzValue) -> usize {
    match value {
        FuzzValue::Unit => 0,
        FuzzValue::Bool(_) => 1,
        FuzzValue::Int(width, _) | FuzzValue::UInt(width, _) => width.byte_len(),
        FuzzValue::Float(width, _) => width.byte_len(),
        FuzzValue::Char(_) => 4,
        FuzzValue::String(value) => 8 + value.len(),
        FuzzValue::Array(items) => 8 + items.iter().map(wire_size).sum::<usize>(),
        FuzzValue::Tuple(items) => items.iter().map(wire_size).sum(),
    }
}

pub(crate) fn generate(
    spec: &ValueSpec,
    budget: usize,
    session: &mut Session,
) -> mutatis::Result<FuzzValue> {
    let minimum = spec
        .minimum_wire_size()
        .filter(|&minimum| minimum <= budget)
        .ok_or_else(|| mutatis::Error::other("input domain does not fit the wire budget"))?;
    match spec {
        ValueSpec::String {
            charset,
            min_len,
            max_len,
        } => {
            let low = (*min_len).min(*max_len);
            let high = (*min_len).max(*max_len).min((budget - 8) as u64);
            let len = session.generate_with(&mut m::mrange(low..=high))? as usize;
            let mut out = String::with_capacity(len);
            for index in 0..len {
                let mut ch = session.generate_with(&mut CharsetMutator(*charset))?;
                let available = budget - 8 - out.len() - (len - index - 1);
                if ch.len_utf8() > available {
                    debug_assert_eq!(*charset, Charset::Utf8);
                    ch = 'a';
                }
                out.push(ch);
            }
            Ok(FuzzValue::String(out))
        }
        ValueSpec::Array {
            elem,
            min_len,
            max_len,
        } => {
            let low = (*min_len).min(*max_len);
            let available = budget - 8;
            let Some(element_min) = elem.minimum_wire_size() else {
                return Ok(FuzzValue::Array(Vec::new()));
            };
            let limit = available
                .checked_div(element_min)
                .map_or_else(|| low.saturating_add(available as u64), |n| n as u64);
            let high = (*min_len).max(*max_len).min(limit);
            let len = session.generate_with(&mut m::mrange(low..=high))? as usize;
            let mut remaining = available;
            let mut items = Vec::with_capacity(len);
            for index in 0..len {
                let reserved = element_min * (len - index - 1);
                let item = generate(elem, remaining - reserved, session)?;
                remaining -= wire_size(&item);
                items.push(item);
            }
            Ok(FuzzValue::Array(items))
        }
        ValueSpec::Tuple(items) => {
            let mut remaining = budget;
            let mut reserved = minimum;
            let mut values = Vec::with_capacity(items.len());
            for item in items {
                reserved -= item.minimum_wire_size().unwrap();
                let value = generate(item, remaining - reserved, session)?;
                remaining -= wire_size(&value);
                values.push(value);
            }
            Ok(FuzzValue::Tuple(values))
        }
        _ => session.generate_with(&mut SpecMutator(spec)),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::spec::{CaseSpec, IntWidth};

    #[test]
    fn rejects_an_impossible_declared_minimum() {
        let suite = SuiteSpec {
            cases: vec![CaseSpec {
                path: vec!["large".into()],
                input: ValueSpec::Array {
                    elem: Box::new(ValueSpec::Bool),
                    min_len: 5000,
                    max_len: 5000,
                },
            }],
        };
        let err = suite.validate_input_budget(&[0], 4096).unwrap_err();
        assert_eq!(
            err,
            InputBudgetError::ExceedsBudget {
                case: "large".into(),
                minimum: 5016,
                budget: 4096,
            }
        );
        let message = err.to_string();
        assert!(message.contains("5016 bytes"));
        assert!(message.contains("-max_len=4096"));
        assert!(suite.validate_input_budget(&[0], 5016).is_ok());
    }

    #[test]
    fn empty_arrays_do_not_require_an_encodable_element() {
        let spec = ValueSpec::Array {
            elem: Box::new(ValueSpec::String {
                charset: Charset::Ascii,
                min_len: u64::MAX,
                max_len: u64::MAX,
            }),
            min_len: 0,
            max_len: 10,
        };
        assert_eq!(spec.minimum_wire_size(), Some(8));
        let value = generate(&spec, 8, &mut Session::new().seed(1)).unwrap();
        assert_eq!(value.to_bytes(), 0u64.to_le_bytes());
    }

    #[test]
    fn variable_elements_can_use_unequal_shares_of_the_budget() {
        let spec = ValueSpec::Array {
            elem: Box::new(ValueSpec::String {
                charset: Charset::Ascii,
                min_len: 0,
                max_len: 1000,
            }),
            min_len: 2,
            max_len: 2,
        };
        let mut reached_long_first = false;
        for seed in 0..64 {
            let value = generate(&spec, 80, &mut Session::new().seed(seed)).unwrap();
            assert!(wire_size(&value) <= 80);
            let FuzzValue::Array(items) = value else {
                unreachable!()
            };
            let FuzzValue::String(first) = &items[0] else {
                unreachable!()
            };
            reached_long_first |= first.len() > 28;
        }
        assert!(
            reached_long_first,
            "feasible uneven collection layouts must remain reachable"
        );
    }

    #[test]
    fn bounded_mutations_remain_complete_nested_inputs() {
        let suite = SuiteSpec {
            cases: vec![CaseSpec {
                path: vec!["nested".into()],
                input: ValueSpec::Tuple(vec![
                    ValueSpec::Array {
                        elem: Box::new(ValueSpec::UInt {
                            width: IntWidth::I16,
                            min: 0,
                            max: 65535,
                        }),
                        min_len: 1,
                        max_len: 1_000_000,
                    },
                    ValueSpec::String {
                        charset: Charset::Utf8,
                        min_len: 2,
                        max_len: 1_000_000,
                    },
                ]),
            }],
        };
        suite.validate_input_budget(&[0], 64).unwrap();
        let original = suite.to_bytes();
        let mut bytes = [0; 64];
        let mut size = 0;
        for seed in 0..128 {
            size = suite.mutate_buffer(&mut bytes, size, 64, seed, None);
            assert!(size <= 64);
            let decoded = suite
                .decode_input(&bytes[..size])
                .expect("complete wire input");
            assert_eq!(decoded.to_bytes(), bytes[..size]);
        }
        assert_eq!(suite.to_bytes(), original);
    }
}
