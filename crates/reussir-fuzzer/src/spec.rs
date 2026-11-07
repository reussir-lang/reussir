//! Input domain specifications shared between `library/test` and `reussir-fuzzer`.
//!
//! All [`ValueSpec`] and [`SuiteSpec`] encodings match `test::spec` in
//! `library/test/src/spec.rr` byte-for-byte with a complete round-trip guarantee.

use std::fmt;

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

impl fmt::Display for IntWidth {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::I8 => f.write_str("8"),
            Self::I16 => f.write_str("16"),
            Self::I32 => f.write_str("32"),
            Self::I64 => f.write_str("64"),
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

impl fmt::Display for FloatWidth {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::F32 => f.write_str("f32"),
            Self::F64 => f.write_str("f64"),
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

impl fmt::Display for Charset {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Ascii => f.write_str("ascii"),
            Self::AsciiPrintable => f.write_str("printable"),
            Self::Alphanumeric => f.write_str("alnum"),
            Self::Alpha => f.write_str("alpha"),
            Self::Digits => f.write_str("digits"),
            Self::Identifier => f.write_str("ident"),
            Self::Utf8 => f.write_str("utf8"),
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

impl fmt::Display for ValueSpec {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Unit => f.write_str("unit"),
            Self::Bool => f.write_str("bool"),
            Self::Int { width, min, max } => write!(f, "i{width}[{min}..={max}]"),
            Self::UInt { width, min, max } => write!(f, "u{width}[{min}..={max}]"),
            Self::Float {
                width,
                min,
                max,
                allow_nan,
                allow_inf,
            } => {
                write!(f, "{width}[{min}..={max}]")?;
                if *allow_nan {
                    f.write_str("+nan")?;
                }
                if *allow_inf {
                    f.write_str("+inf")?;
                }
                Ok(())
            }
            Self::Char(cs) => write!(f, "char({cs})"),
            Self::String {
                charset,
                min_len,
                max_len,
            } => write!(f, "string({charset})[{min_len}..={max_len}]"),
            Self::Array {
                elem,
                min_len,
                max_len,
            } => write!(f, "array({elem})[{min_len}..={max_len}]"),
            Self::Tuple(items) => {
                f.write_str("(")?;
                for (i, item) in items.iter().enumerate() {
                    if i > 0 {
                        f.write_str(", ")?;
                    }
                    write!(f, "{item}")?;
                }
                f.write_str(")")
            }
        }
    }
}

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
    pub fn from_bytes(bytes: &[u8]) -> Option<Self> {
        let mut cursor = Cursor::new(bytes);
        if cursor.read_exact(4)? != VALUE_SPEC_MAGIC {
            return None;
        }
        if cursor.read_u8()? != SPEC_VERSION {
            return None;
        }
        let spec = Self::decode_from(&mut cursor)?;
        cursor.is_eof().then_some(spec)
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

    pub(crate) fn decode_from(cursor: &mut Cursor<'_>) -> Option<Self> {
        let tag = cursor.read_u8()?;
        match tag {
            0 => Some(Self::Unit),
            1 => Some(Self::Bool),
            2 => {
                let width = IntWidth::from_tag(cursor.read_u8()?)?;
                let min = cursor.read_u64_le()? as i64;
                let max = cursor.read_u64_le()? as i64;
                Some(Self::Int { width, min, max })
            }
            3 => {
                let width = IntWidth::from_tag(cursor.read_u8()?)?;
                let min = cursor.read_u64_le()?;
                let max = cursor.read_u64_le()?;
                Some(Self::UInt { width, min, max })
            }
            4 => {
                let width = FloatWidth::from_tag(cursor.read_u8()?)?;
                let min = f64::from_bits(cursor.read_u64_le()?);
                let max = f64::from_bits(cursor.read_u64_le()?);
                let nan_flag = cursor.read_u8()?;
                let inf_flag = cursor.read_u8()?;
                if nan_flag > 1 || inf_flag > 1 {
                    return None;
                }
                Some(Self::Float {
                    width,
                    min,
                    max,
                    allow_nan: nan_flag == 1,
                    allow_inf: inf_flag == 1,
                })
            }
            5 => {
                let charset = Charset::from_tag(cursor.read_u8()?)?;
                Some(Self::Char(charset))
            }
            6 => {
                let charset = Charset::from_tag(cursor.read_u8()?)?;
                let min_len = cursor.read_u64_le()?;
                let max_len = cursor.read_u64_le()?;
                Some(Self::String {
                    charset,
                    min_len,
                    max_len,
                })
            }
            7 => {
                let min_len = cursor.read_u64_le()?;
                let max_len = cursor.read_u64_le()?;
                let elem = Box::new(Self::decode_from(cursor)?);
                Some(Self::Array {
                    elem,
                    min_len,
                    max_len,
                })
            }
            8 => {
                let arity = usize::try_from(cursor.read_u64_le()?).ok()?;
                let mut items = Vec::with_capacity(arity.min(1024));
                for _ in 0..arity {
                    items.push(Self::decode_from(cursor)?);
                }
                Some(Self::Tuple(items))
            }
            _ => None,
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

impl fmt::Display for CaseSpec {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.joined_path(), self.input)
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
    pub fn from_bytes(bytes: &[u8]) -> Option<Self> {
        let mut cursor = Cursor::new(bytes);
        if cursor.read_exact(4)? != SUITE_SPEC_MAGIC {
            return None;
        }
        if cursor.read_u8()? != SPEC_VERSION {
            return None;
        }
        let count = usize::try_from(cursor.read_u64_le()?).ok()?;
        let mut cases = Vec::with_capacity(count.min(1024));
        for _ in 0..count {
            let path_len = usize::try_from(cursor.read_u64_le()?).ok()?;
            let mut path = Vec::with_capacity(path_len.min(128));
            for _ in 0..path_len {
                let seg_len = usize::try_from(cursor.read_u64_le()?).ok()?;
                let raw = cursor.read_exact(seg_len)?;
                let seg = std::str::from_utf8(raw).ok()?.to_owned();
                path.push(seg);
            }
            let input = ValueSpec::decode_from(&mut cursor)?;
            cases.push(CaseSpec { path, input });
        }
        cursor.is_eof().then_some(Self { cases })
    }
}

impl fmt::Display for SuiteSpec {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (i, case) in self.cases.iter().enumerate() {
            if i > 0 {
                writeln!(f)?;
            }
            write!(f, "{case}")?;
        }
        Ok(())
    }
}

pub(crate) struct Cursor<'a> {
    bytes: &'a [u8],
    offset: usize,
}

impl<'a> Cursor<'a> {
    pub(crate) const fn new(bytes: &'a [u8]) -> Self {
        Self { bytes, offset: 0 }
    }

    pub(crate) const fn is_eof(&self) -> bool {
        self.offset >= self.bytes.len()
    }

    pub(crate) fn read_exact(&mut self, len: usize) -> Option<&'a [u8]> {
        let end = self.offset.checked_add(len)?;
        if end > self.bytes.len() {
            return None;
        }
        let slice = &self.bytes[self.offset..end];
        self.offset = end;
        Some(slice)
    }

    pub(crate) fn read_u8(&mut self) -> Option<u8> {
        let b = *self.bytes.get(self.offset)?;
        self.offset += 1;
        Some(b)
    }

    pub(crate) fn read_u64_le(&mut self) -> Option<u64> {
        let raw: [u8; 8] = self.read_exact(8)?.try_into().ok()?;
        Some(u64::from_le_bytes(raw))
    }
}
