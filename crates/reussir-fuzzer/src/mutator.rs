//! Structured fuzz input encoding and decoding.

use crate::spec::{Charset, FloatWidth, IntWidth, SuiteSpec, ValueSpec};

const PRINTABLE_ASCII: &[u8] =
    b" !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~";
const ALPHANUMERIC_ASCII: &[u8] = b"0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";
const ALPHA_ASCII: &[u8] = b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";
const DIGITS_ASCII: &[u8] = b"0123456789";
const IDENT_ASCII: &[u8] = b"_0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";

/// Structured runtime fuzz value corresponding to a [`ValueSpec`].
#[derive(Clone, Debug, PartialEq)]
pub enum FuzzValue {
    Unit,
    Bool(bool),
    Int(IntWidth, i64),
    UInt(IntWidth, u64),
    Float(FloatWidth, f64),
    Char(char),
    String(String),
    Array(Vec<FuzzValue>),
    Tuple(Vec<FuzzValue>),
}

/// Decoded suite-level fuzz input pairing a case selector with its structured payload.
#[derive(Clone, Debug, PartialEq)]
pub struct SuiteInput {
    pub case_idx: usize,
    pub payload: FuzzValue,
}

impl FuzzValue {
    /// Serializes this structured value into the binary wire format expected by
    /// Reussir's `Domain<T>` decoders.
    pub fn to_bytes(&self) -> Vec<u8> {
        let mut out = Vec::new();
        self.encode_into(&mut out);
        out
    }

    /// Appends the binary wire encoding of `self` to `out`.
    pub fn encode_into(&self, out: &mut Vec<u8>) {
        match self {
            Self::Unit => {}
            Self::Bool(b) => out.push(u8::from(*b)),
            Self::Int(width, value) => write_int_le(out, *width, *value),
            Self::UInt(width, value) => write_uint_le(out, *width, *value),
            Self::Float(FloatWidth::F32, value) => {
                out.extend_from_slice(&(*value as f32).to_bits().to_le_bytes());
            }
            Self::Float(FloatWidth::F64, value) => {
                out.extend_from_slice(&value.to_bits().to_le_bytes());
            }
            Self::Char(ch) => {
                out.extend_from_slice(&(*ch as u32).to_le_bytes());
            }
            Self::String(s) => {
                let bytes = s.as_bytes();
                out.extend_from_slice(&(bytes.len() as u64).to_le_bytes());
                out.extend_from_slice(bytes);
            }
            Self::Array(items) => {
                out.extend_from_slice(&(items.len() as u64).to_le_bytes());
                for item in items {
                    item.encode_into(out);
                }
            }
            Self::Tuple(items) => {
                for item in items {
                    item.encode_into(out);
                }
            }
        }
    }
}

impl SuiteInput {
    /// Encodes this suite input (8-byte little-endian case selector followed by
    /// the encoded payload) into a byte vector.
    pub fn to_bytes(&self) -> Vec<u8> {
        let mut out = Vec::with_capacity(16);
        self.encode_into(&mut out);
        out
    }

    /// Appends the binary wire encoding of this suite input to `out`.
    pub fn encode_into(&self, out: &mut Vec<u8>) {
        out.extend_from_slice(&(self.case_idx as u64).to_le_bytes());
        self.payload.encode_into(out);
    }
}

impl ValueSpec {
    /// Decodes a structured value within this specification’s domain bounds.
    pub fn decode_value(&self, cursor: &mut &[u8]) -> Option<FuzzValue> {
        match self {
            Self::Unit => Some(FuzzValue::Unit),
            Self::Bool => {
                let b = take_bytes(cursor, 1)?[0];
                Some(FuzzValue::Bool((b & 1) != 0))
            }
            Self::Int { width, min, max } => {
                let bytes = take_bytes(cursor, width.byte_len())?;
                let raw = read_int_le(bytes, *width);
                let (lo, hi) = clamped_signed_bounds(*width, *min, *max);
                Some(FuzzValue::Int(*width, project_i64(raw, lo, hi)))
            }
            Self::UInt { width, min, max } => {
                let bytes = take_bytes(cursor, width.byte_len())?;
                let raw = read_uint_le(bytes, *width);
                let (lo, hi) = clamped_unsigned_bounds(*width, *min, *max);
                Some(FuzzValue::UInt(*width, project_u64(raw, lo, hi)))
            }
            Self::Float {
                width,
                min,
                max,
                allow_nan,
                allow_inf,
            } => {
                let (lo, hi) = if *min <= *max {
                    (*min, *max)
                } else {
                    (*max, *min)
                };
                match width {
                    FloatWidth::F32 => {
                        let bytes = take_bytes(cursor, 4)?;
                        let raw =
                            f32::from_bits(u32::from_le_bytes(bytes.try_into().unwrap())) as f64;
                        Some(FuzzValue::Float(
                            FloatWidth::F32,
                            constrain_f64(raw, lo, hi, *allow_nan, *allow_inf),
                        ))
                    }
                    FloatWidth::F64 => {
                        let bytes = take_bytes(cursor, 8)?;
                        let raw = f64::from_bits(u64::from_le_bytes(bytes.try_into().unwrap()));
                        Some(FuzzValue::Float(
                            FloatWidth::F64,
                            constrain_f64(raw, lo, hi, *allow_nan, *allow_inf),
                        ))
                    }
                }
            }
            Self::Char(charset) => {
                let bytes = take_bytes(cursor, 4)?;
                let raw = u32::from_le_bytes(bytes.try_into().unwrap());
                let ch = char::from_u32(raw)
                    .filter(|c| charset_contains(*charset, *c))
                    .unwrap_or_else(|| project_char(raw as u64, *charset));
                Some(FuzzValue::Char(ch))
            }
            Self::String {
                charset,
                min_len,
                max_len,
            } => {
                let len_bytes = take_bytes(cursor, 8)?;
                let byte_len = u64::from_le_bytes(len_bytes.try_into().unwrap()) as usize;
                let raw_bytes = take_bytes(cursor, byte_len)?;
                let (lo, hi) = normalized_len_bounds(*min_len, *max_len);
                let s = decode_and_sanitize_string(raw_bytes, *charset, lo, hi);
                Some(FuzzValue::String(s))
            }
            Self::Array {
                elem,
                min_len,
                max_len,
            } => {
                let len_bytes = take_bytes(cursor, 8)?;
                let raw_len = u64::from_le_bytes(len_bytes.try_into().unwrap());
                let (lo, hi) = normalized_len_bounds(*min_len, *max_len);
                let count = project_u64(raw_len, lo as u64, hi as u64) as usize;
                let mut items = Vec::with_capacity(count);
                for _ in 0..count {
                    items.push(elem.decode_value(cursor)?);
                }
                Some(FuzzValue::Array(items))
            }
            Self::Tuple(specs) => {
                let mut items = Vec::with_capacity(specs.len());
                for spec in specs {
                    items.push(spec.decode_value(cursor)?);
                }
                Some(FuzzValue::Tuple(items))
            }
        }
    }
}

impl SuiteSpec {
    /// Decodes a case selector and its structured payload.
    pub fn decode_input(&self, bytes: &[u8]) -> Option<SuiteInput> {
        if self.cases.is_empty() || bytes.len() < 8 {
            return None;
        }
        let raw_idx = u64::from_le_bytes(bytes[..8].try_into().unwrap());
        let case_idx = (raw_idx as usize) % self.cases.len();
        let mut cursor = &bytes[8..];
        let payload = self.cases[case_idx].input.decode_value(&mut cursor)?;
        Some(SuiteInput { case_idx, payload })
    }
}

fn take_bytes<'a>(cursor: &mut &'a [u8], n: usize) -> Option<&'a [u8]> {
    if cursor.len() < n {
        None
    } else {
        let (head, tail) = cursor.split_at(n);
        *cursor = tail;
        Some(head)
    }
}

fn clamped_signed_bounds(width: IntWidth, min: i64, max: i64) -> (i64, i64) {
    let (lo, hi) = if min <= max { (min, max) } else { (max, min) };
    (
        lo.clamp(width.signed_min(), width.signed_max()),
        hi.clamp(width.signed_min(), width.signed_max()),
    )
}

fn clamped_unsigned_bounds(width: IntWidth, min: u64, max: u64) -> (u64, u64) {
    let (lo, hi) = if min <= max { (min, max) } else { (max, min) };
    (lo.min(width.unsigned_max()), hi.min(width.unsigned_max()))
}

fn normalized_len_bounds(min_len: u64, max_len: u64) -> (usize, usize) {
    let (lo, hi) = if min_len <= max_len {
        (min_len, max_len)
    } else {
        (max_len, min_len)
    };
    (
        usize::try_from(lo).unwrap_or(usize::MAX),
        usize::try_from(hi).unwrap_or(usize::MAX),
    )
}

fn constrain_f64(raw: f64, lo: f64, hi: f64, allow_nan: bool, allow_inf: bool) -> f64 {
    if raw.is_nan() {
        if allow_nan {
            return f64::NAN;
        }
        return lo;
    }
    if raw.is_infinite() {
        if allow_inf {
            return raw;
        }
        return if raw.is_sign_positive() { hi } else { lo };
    }
    if raw >= lo && raw <= hi {
        raw
    } else if lo >= hi {
        lo
    } else {
        let u = ((raw.to_bits() >> 11) as f64) * (1.0 / 9007199254740992.0);
        ((1.0 - u) * lo + u * hi).clamp(lo, hi)
    }
}

fn decode_and_sanitize_string(
    raw_bytes: &[u8],
    charset: Charset,
    min_len: usize,
    max_len: usize,
) -> String {
    let mut out = String::with_capacity(min_len);
    if let Ok(valid) = std::str::from_utf8(raw_bytes) {
        let mut count = 0usize;
        for ch in valid.chars() {
            if count >= max_len {
                break;
            }
            out.push(if charset_contains(charset, ch) {
                ch
            } else {
                project_char(ch as u64, charset)
            });
            count += 1;
        }
        while count < min_len {
            out.push(project_char(count as u64, charset));
            count += 1;
        }
    } else {
        let target = project_u64(raw_bytes.len() as u64, min_len as u64, max_len as u64) as usize;
        for i in 0..target {
            let word = raw_bytes.get(i).copied().unwrap_or(i as u8) as u64;
            out.push(project_char(word, charset));
        }
    }
    out
}

fn charset_contains(charset: Charset, ch: char) -> bool {
    match charset {
        Charset::Ascii => ch.is_ascii(),
        Charset::AsciiPrintable => (' '..='~').contains(&ch),
        Charset::Alphanumeric => ch.is_ascii_alphanumeric(),
        Charset::Alpha => ch.is_ascii_alphabetic(),
        Charset::Digits => ch.is_ascii_digit(),
        Charset::Identifier => ch.is_ascii_alphanumeric() || ch == '_',
        Charset::Utf8 => true,
    }
}

fn project_char(raw: u64, charset: Charset) -> char {
    let code = match charset {
        Charset::Ascii => (raw & 0x7f) as u32,
        Charset::AsciiPrintable => PRINTABLE_ASCII[(raw as usize) % PRINTABLE_ASCII.len()] as u32,
        Charset::Alphanumeric => {
            ALPHANUMERIC_ASCII[(raw as usize) % ALPHANUMERIC_ASCII.len()] as u32
        }
        Charset::Alpha => ALPHA_ASCII[(raw as usize) % ALPHA_ASCII.len()] as u32,
        Charset::Digits => DIGITS_ASCII[(raw as usize) % DIGITS_ASCII.len()] as u32,
        Charset::Identifier => IDENT_ASCII[(raw as usize) % IDENT_ASCII.len()] as u32,
        Charset::Utf8 => {
            let candidate = (raw as u32) % 0x110000;
            if (0xd800..=0xdfff).contains(&candidate) {
                0x20 + (candidate & 0x5f)
            } else {
                candidate
            }
        }
    };
    char::from_u32(code).unwrap_or(' ')
}

fn write_int_le(out: &mut Vec<u8>, width: IntWidth, value: i64) {
    match width {
        IntWidth::I8 => out.push((value as i8) as u8),
        IntWidth::I16 => out.extend_from_slice(&(value as i16).to_le_bytes()),
        IntWidth::I32 => out.extend_from_slice(&(value as i32).to_le_bytes()),
        IntWidth::I64 => out.extend_from_slice(&value.to_le_bytes()),
    }
}

fn read_int_le(bytes: &[u8], width: IntWidth) -> i64 {
    match width {
        IntWidth::I8 => (bytes[0] as i8) as i64,
        IntWidth::I16 => i16::from_le_bytes(bytes[..2].try_into().unwrap()) as i64,
        IntWidth::I32 => i32::from_le_bytes(bytes[..4].try_into().unwrap()) as i64,
        IntWidth::I64 => i64::from_le_bytes(bytes[..8].try_into().unwrap()),
    }
}

fn write_uint_le(out: &mut Vec<u8>, width: IntWidth, value: u64) {
    match width {
        IntWidth::I8 => out.push(value as u8),
        IntWidth::I16 => out.extend_from_slice(&(value as u16).to_le_bytes()),
        IntWidth::I32 => out.extend_from_slice(&(value as u32).to_le_bytes()),
        IntWidth::I64 => out.extend_from_slice(&value.to_le_bytes()),
    }
}

fn read_uint_le(bytes: &[u8], width: IntWidth) -> u64 {
    match width {
        IntWidth::I8 => bytes[0] as u64,
        IntWidth::I16 => u16::from_le_bytes(bytes[..2].try_into().unwrap()) as u64,
        IntWidth::I32 => u32::from_le_bytes(bytes[..4].try_into().unwrap()) as u64,
        IntWidth::I64 => u64::from_le_bytes(bytes[..8].try_into().unwrap()),
    }
}

pub fn project_u64(raw: u64, min: u64, max: u64) -> u64 {
    if min >= max {
        min
    } else if raw >= min && raw <= max {
        raw
    } else {
        let span = (max as u128) - (min as u128) + 1;
        min + (((raw as u128) % span) as u64)
    }
}

pub fn project_i64(raw: i64, min: i64, max: i64) -> i64 {
    if min >= max {
        min
    } else if raw >= min && raw <= max {
        raw
    } else {
        let sign_bit = 0x8000_0000_0000_0000u64;
        let u_min = (min as u64) ^ sign_bit;
        let u_max = (max as u64) ^ sign_bit;
        let u_raw = (raw as u64) ^ sign_bit;
        let projected = project_u64(u_raw, u_min, u_max);
        (projected ^ sign_bit) as i64
    }
}

#[cfg(test)]
mod length_tests {
    use super::*;

    #[test]
    fn large_array_keeps_following_tuple_field_aligned() {
        let spec = ValueSpec::Tuple(vec![
            ValueSpec::Array {
                elem: Box::new(ValueSpec::UInt {
                    width: IntWidth::I8,
                    min: 0,
                    max: 255,
                }),
                min_len: 5000,
                max_len: 5000,
            },
            ValueSpec::Bool,
        ]);
        let value = FuzzValue::Tuple(vec![
            FuzzValue::Array(vec![FuzzValue::UInt(IntWidth::I8, 0); 5000]),
            FuzzValue::Bool(true),
        ]);
        let bytes = value.to_bytes();
        let mut cursor = bytes.as_slice();
        assert_eq!(spec.decode_value(&mut cursor), Some(value));
        assert!(cursor.is_empty());
    }
}
