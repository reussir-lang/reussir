//! Input domain specifications shared between `library/test` and `reussir-fuzzer`.
//!
//! All [`ValueSpec`] and [`SuiteSpec`] encodings match `test::spec` in
//! `library/test/src/spec.rr` byte-for-byte with a complete round-trip guarantee.

use winnow::binary::{le_i64, le_u8, le_u64};
use winnow::combinator::{eof, fail, repeat};
use winnow::error::ContextError;
use winnow::prelude::*;
use winnow::token::{literal, take};

/// Magic header for a standalone [`ValueSpec`] payload (`"RVSP"`).
pub const VALUE_SPEC_MAGIC: [u8; 4] = *b"RVSP";

/// Magic header for a [`SuiteSpec`] payload (`"RTSP"`).
pub const SUITE_SPEC_MAGIC: [u8; 4] = *b"RTSP";

/// Wire format version for [`ValueSpec`] and [`SuiteSpec`].
pub const SPEC_VERSION: u8 = 1;

/// Width tag for integer input domains.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum IntWidth {
    I8,
    I16,
    I32,
    I64,
}

impl IntWidth {
    pub const fn byte_len(self) -> usize {
        match self {
            Self::I8 => 1,
            Self::I16 => 2,
            Self::I32 => 4,
            Self::I64 => 8,
        }
    }

    pub const fn signed_min(self) -> i64 {
        match self {
            Self::I8 => i8::MIN as i64,
            Self::I16 => i16::MIN as i64,
            Self::I32 => i32::MIN as i64,
            Self::I64 => i64::MIN,
        }
    }

    pub const fn signed_max(self) -> i64 {
        match self {
            Self::I8 => i8::MAX as i64,
            Self::I16 => i16::MAX as i64,
            Self::I32 => i32::MAX as i64,
            Self::I64 => i64::MAX,
        }
    }

    pub const fn unsigned_max(self) -> u64 {
        match self {
            Self::I8 => u8::MAX as u64,
            Self::I16 => u16::MAX as u64,
            Self::I32 => u32::MAX as u64,
            Self::I64 => u64::MAX,
        }
    }

    pub const fn to_tag(self) -> u8 {
        match self {
            Self::I8 => 0,
            Self::I16 => 1,
            Self::I32 => 2,
            Self::I64 => 3,
        }
    }

    pub const fn from_tag(tag: u8) -> Option<Self> {
        match tag {
            0 => Some(Self::I8),
            1 => Some(Self::I16),
            2 => Some(Self::I32),
            3 => Some(Self::I64),
            _ => None,
        }
    }
}

/// Width tag for IEEE-754 floating-point input domains.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum FloatWidth {
    F32,
    F64,
}

impl FloatWidth {
    pub const fn byte_len(self) -> usize {
        match self {
            Self::F32 => 4,
            Self::F64 => 8,
        }
    }

    pub const fn to_tag(self) -> u8 {
        match self {
            Self::F32 => 0,
            Self::F64 => 1,
        }
    }

    pub const fn from_tag(tag: u8) -> Option<Self> {
        match tag {
            0 => Some(Self::F32),
            1 => Some(Self::F64),
            _ => None,
        }
    }
}

/// Character-set constraint for character and string domains.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Charset {
    Ascii,
    AsciiPrintable,
    Alphanumeric,
    Alpha,
    Digits,
    Identifier,
    Utf8,
}

impl Charset {
    pub const fn to_tag(self) -> u8 {
        match self {
            Self::Ascii => 0,
            Self::AsciiPrintable => 1,
            Self::Alphanumeric => 2,
            Self::Alpha => 3,
            Self::Digits => 4,
            Self::Identifier => 5,
            Self::Utf8 => 6,
        }
    }

    pub const fn from_tag(tag: u8) -> Option<Self> {
        match tag {
            0 => Some(Self::Ascii),
            1 => Some(Self::AsciiPrintable),
            2 => Some(Self::Alphanumeric),
            3 => Some(Self::Alpha),
            4 => Some(Self::Digits),
            5 => Some(Self::Identifier),
            6 => Some(Self::Utf8),
            _ => None,
        }
    }
}

/// Schema describing the input domain of a test case.
#[derive(Clone, Debug)]
pub enum ValueSpec {
    Unit,
    Bool,
    Int {
        width: IntWidth,
        min: i64,
        max: i64,
    },
    UInt {
        width: IntWidth,
        min: u64,
        max: u64,
    },
    Float {
        width: FloatWidth,
        min: f64,
        max: f64,
        allow_nan: bool,
        allow_inf: bool,
    },
    Char(Charset),
    String {
        charset: Charset,
        min_len: u64,
        max_len: u64,
    },
    Array {
        elem: Box<ValueSpec>,
        min_len: u64,
        max_len: u64,
    },
    Tuple(Vec<ValueSpec>),
}

impl PartialEq for ValueSpec {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Unit, Self::Unit) | (Self::Bool, Self::Bool) => true,
            (
                Self::Int {
                    width: w1,
                    min: min1,
                    max: max1,
                },
                Self::Int {
                    width: w2,
                    min: min2,
                    max: max2,
                },
            ) => w1 == w2 && min1 == min2 && max1 == max2,
            (
                Self::UInt {
                    width: w1,
                    min: min1,
                    max: max1,
                },
                Self::UInt {
                    width: w2,
                    min: min2,
                    max: max2,
                },
            ) => w1 == w2 && min1 == min2 && max1 == max2,
            (
                Self::Float {
                    width: w1,
                    min: min1,
                    max: max1,
                    allow_nan: nan1,
                    allow_inf: inf1,
                },
                Self::Float {
                    width: w2,
                    min: min2,
                    max: max2,
                    allow_nan: nan2,
                    allow_inf: inf2,
                },
            ) => {
                w1 == w2
                    && min1.to_bits() == min2.to_bits()
                    && max1.to_bits() == max2.to_bits()
                    && nan1 == nan2
                    && inf1 == inf2
            }
            (Self::Char(c1), Self::Char(c2)) => c1 == c2,
            (
                Self::String {
                    charset: c1,
                    min_len: min1,
                    max_len: max1,
                },
                Self::String {
                    charset: c2,
                    min_len: min2,
                    max_len: max2,
                },
            ) => c1 == c2 && min1 == min2 && max1 == max2,
            (
                Self::Array {
                    elem: e1,
                    min_len: min1,
                    max_len: max1,
                },
                Self::Array {
                    elem: e2,
                    min_len: min2,
                    max_len: max2,
                },
            ) => min1 == min2 && max1 == max2 && e1 == e2,
            (Self::Tuple(t1), Self::Tuple(t2)) => t1 == t2,
            _ => false,
        }
    }
}

impl Eq for ValueSpec {}

impl ValueSpec {
    /// Whether this spec represents a zero-argument unit test case.
    pub const fn is_unit(&self) -> bool {
        matches!(self, Self::Unit)
    }

    /// Serializes this [`ValueSpec`] to bytes with the `"RVSP"` v1 header.
    pub fn to_bytes(&self) -> Vec<u8> {
        let mut out = Vec::new();
        out.extend_from_slice(&VALUE_SPEC_MAGIC);
        out.push(SPEC_VERSION);
        self.encode_into(&mut out);
        out
    }

    /// Deserializes a [`ValueSpec`] from bytes produced by [`ValueSpec::to_bytes`].
    pub fn from_bytes(mut bytes: &[u8]) -> Option<Self> {
        (
            literal(VALUE_SPEC_MAGIC.as_slice()),
            literal([SPEC_VERSION].as_slice()),
            Self::parse,
            eof,
        )
            .map(|(_, _, spec, _)| spec)
            .parse_next(&mut bytes)
            .ok()
    }

    pub(crate) fn encode_into(&self, out: &mut Vec<u8>) {
        match self {
            Self::Unit => out.push(0),
            Self::Bool => out.push(1),
            Self::Int { width, min, max } => {
                out.push(2);
                out.push(width.to_tag());
                out.extend_from_slice(&(*min as u64).to_le_bytes());
                out.extend_from_slice(&(*max as u64).to_le_bytes());
            }
            Self::UInt { width, min, max } => {
                out.push(3);
                out.push(width.to_tag());
                out.extend_from_slice(&min.to_le_bytes());
                out.extend_from_slice(&max.to_le_bytes());
            }
            Self::Float {
                width,
                min,
                max,
                allow_nan,
                allow_inf,
            } => {
                out.push(4);
                out.push(width.to_tag());
                out.extend_from_slice(&min.to_bits().to_le_bytes());
                out.extend_from_slice(&max.to_bits().to_le_bytes());
                out.push(u8::from(*allow_nan));
                out.push(u8::from(*allow_inf));
            }
            Self::Char(charset) => {
                out.push(5);
                out.push(charset.to_tag());
            }
            Self::String {
                charset,
                min_len,
                max_len,
            } => {
                out.push(6);
                out.push(charset.to_tag());
                out.extend_from_slice(&min_len.to_le_bytes());
                out.extend_from_slice(&max_len.to_le_bytes());
            }
            Self::Array {
                elem,
                min_len,
                max_len,
            } => {
                out.push(7);
                out.extend_from_slice(&min_len.to_le_bytes());
                out.extend_from_slice(&max_len.to_le_bytes());
                elem.encode_into(out);
            }
            Self::Tuple(items) => {
                out.push(8);
                out.extend_from_slice(&(items.len() as u64).to_le_bytes());
                for item in items {
                    item.encode_into(out);
                }
            }
        }
    }

    pub(crate) fn parse(input: &mut &[u8]) -> ModalResult<Self, ContextError> {
        match le_u8.parse_next(input)? {
            0 => Ok(Self::Unit),
            1 => Ok(Self::Bool),
            2 => {
                let width = le_u8.verify_map(IntWidth::from_tag).parse_next(input)?;
                let min = le_i64.parse_next(input)?;
                let max = le_i64.parse_next(input)?;
                Ok(Self::Int { width, min, max })
            }
            3 => {
                let width = le_u8.verify_map(IntWidth::from_tag).parse_next(input)?;
                let min = le_u64.parse_next(input)?;
                let max = le_u64.parse_next(input)?;
                Ok(Self::UInt { width, min, max })
            }
            4 => {
                let width = le_u8.verify_map(FloatWidth::from_tag).parse_next(input)?;
                let min = le_u64.map(f64::from_bits).parse_next(input)?;
                let max = le_u64.map(f64::from_bits).parse_next(input)?;
                let allow_nan = parse_bool_flag(input)?;
                let allow_inf = parse_bool_flag(input)?;
                Ok(Self::Float {
                    width,
                    min,
                    max,
                    allow_nan,
                    allow_inf,
                })
            }
            5 => {
                let charset = le_u8.verify_map(Charset::from_tag).parse_next(input)?;
                Ok(Self::Char(charset))
            }
            6 => {
                let charset = le_u8.verify_map(Charset::from_tag).parse_next(input)?;
                let min_len = le_u64.parse_next(input)?;
                let max_len = le_u64.parse_next(input)?;
                Ok(Self::String {
                    charset,
                    min_len,
                    max_len,
                })
            }
            7 => {
                let min_len = le_u64.parse_next(input)?;
                let max_len = le_u64.parse_next(input)?;
                let elem = Box::new(Self::parse(input)?);
                Ok(Self::Array {
                    elem,
                    min_len,
                    max_len,
                })
            }
            8 => {
                let arity = parse_len(input)?;
                let items = repeat(arity, Self::parse).parse_next(input)?;
                Ok(Self::Tuple(items))
            }
            _ => fail.parse_next(input),
        }
    }
}

/// Specification of a single test case within a [`SuiteSpec`].
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CaseSpec {
    pub path: Vec<String>,
    pub input: ValueSpec,
}

impl CaseSpec {
    /// Returns the slash-joined path (`group/subgroup/case`).
    pub fn joined_path(&self) -> String {
        self.path.join("/")
    }
}

/// Complete specification derived from a `TestTree`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SuiteSpec {
    pub cases: Vec<CaseSpec>,
}

impl SuiteSpec {
    /// Serializes this [`SuiteSpec`] to bytes with the `"RTSP"` v1 header.
    pub fn to_bytes(&self) -> Vec<u8> {
        let mut out = Vec::new();
        out.extend_from_slice(&SUITE_SPEC_MAGIC);
        out.push(SPEC_VERSION);
        out.extend_from_slice(&(self.cases.len() as u64).to_le_bytes());
        for case in &self.cases {
            out.extend_from_slice(&(case.path.len() as u64).to_le_bytes());
            for seg in &case.path {
                let bytes = seg.as_bytes();
                out.extend_from_slice(&(bytes.len() as u64).to_le_bytes());
                out.extend_from_slice(bytes);
            }
            case.input.encode_into(&mut out);
        }
        out
    }

    /// Reconstructs a [`SuiteSpec`] from bytes produced by [`SuiteSpec::to_bytes`].
    pub fn from_bytes(mut bytes: &[u8]) -> Option<Self> {
        let (_, _, count) = (
            literal(SUITE_SPEC_MAGIC.as_slice()),
            literal([SPEC_VERSION].as_slice()),
            parse_len,
        )
            .parse_next(&mut bytes)
            .ok()?;
        (repeat(count, parse_case_spec), eof)
            .map(|(cases, _)| Self { cases })
            .parse_next(&mut bytes)
            .ok()
    }
}

fn parse_bool_flag(input: &mut &[u8]) -> ModalResult<bool, ContextError> {
    le_u8
        .verify_map(|b| match b {
            0 => Some(false),
            1 => Some(true),
            _ => None,
        })
        .parse_next(input)
}

fn parse_len(input: &mut &[u8]) -> ModalResult<usize, ContextError> {
    le_u64.try_map(usize::try_from).parse_next(input)
}

fn parse_utf8_string(input: &mut &[u8]) -> ModalResult<String, ContextError> {
    let byte_len = parse_len(input)?;
    take(byte_len)
        .try_map(|raw: &[u8]| std::str::from_utf8(raw).map(str::to_owned))
        .parse_next(input)
}

fn parse_case_spec(input: &mut &[u8]) -> ModalResult<CaseSpec, ContextError> {
    let seg_count = parse_len(input)?;
    let path = repeat(seg_count, parse_utf8_string).parse_next(input)?;
    let input_spec = ValueSpec::parse(input)?;
    Ok(CaseSpec {
        path,
        input: input_spec,
    })
}
