//! Spec-aware input generation and mutation for `reussir-fuzzer`.

use crate::spec::{Charset, FloatWidth, IntWidth, SuiteSpec, ValueSpec};

const PRINTABLE_ASCII: &[u8] =
    b" !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~";
const ALPHANUMERIC_ASCII: &[u8] = b"0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";
const ALPHA_ASCII: &[u8] = b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";
const DIGITS_ASCII: &[u8] = b"0123456789";
const IDENT_ASCII: &[u8] = b"_0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";

/// Deterministic Xoshiro256++ pseudo-random number generator.
#[derive(Clone, Debug)]
pub struct Rng {
    state: [u64; 4],
}

impl Rng {
    /// Seeds a generator from a 64-bit integer using SplitMix64.
    pub fn from_seed(seed: u64) -> Self {
        let mut sm = seed;
        let mut next_sm = || {
            sm = sm.wrapping_add(0x9e3779b97f4a7c15);
            let mut z = sm;
            z = (z ^ (z >> 30)).wrapping_mul(0xbf58476d1ce4e5b9);
            z = (z ^ (z >> 27)).wrapping_mul(0x94d049bb133111eb);
            z ^ (z >> 31)
        };
        Self {
            state: [next_sm(), next_sm(), next_sm(), next_sm()],
        }
    }

    /// Seeds a generator from OS entropy, returning the chosen 64-bit seed and generator.
    pub fn from_entropy() -> (u64, Self) {
        let mut buf = [0u8; 8];
        let seed = if getrandom::fill(&mut buf).is_ok() {
            u64::from_le_bytes(buf)
        } else {
            0x853c49e6748fea9b
        };
        (seed, Self::from_seed(seed))
    }

    /// Generates the next 64-bit random value.
    pub fn next_u64(&mut self) -> u64 {
        let result = self.state[0]
            .wrapping_add(self.state[3])
            .rotate_left(23)
            .wrapping_add(self.state[0]);
        let t = self.state[1] << 17;
        self.state[2] ^= self.state[0];
        self.state[3] ^= self.state[1];
        self.state[1] ^= self.state[2];
        self.state[0] ^= self.state[3];
        self.state[2] ^= t;
        self.state[3] = self.state[3].rotate_left(45);
        result
    }

    /// Generates a uniform value in `min..=max`.
    pub fn gen_range_u64(&mut self, min: u64, max: u64) -> u64 {
        project_u64(self.next_u64(), min, max)
    }

    /// Generates a signed integer in `min..=max`, with bias toward boundary and
    /// small values (`min`, `max`, `0`, `-1`, `1`).
    pub fn sample_i64(&mut self, min: i64, max: i64) -> i64 {
        if min >= max {
            return min;
        }
        match self.next_u64() & 15 {
            0 => min,
            1 => max,
            2 if min <= 0 && max >= 0 => 0,
            3 if min <= 1 && max >= 1 => 1,
            4 if min <= -1 && max >= -1 => -1,
            _ => project_i64(self.next_u64() as i64, min, max),
        }
    }

    /// Generates an unsigned integer in `min..=max`, with bias toward boundary
    /// values (`min`, `max`, `0`, `1`).
    pub fn sample_u64(&mut self, min: u64, max: u64) -> u64 {
        if min >= max {
            return min;
        }
        match self.next_u64() & 15 {
            0 => min,
            1 => max,
            2 if min <= 1 && max >= 1 => 1,
            _ => project_u64(self.next_u64(), min, max),
        }
    }

    /// Generates an `f64` in `min..=max`, honoring `allow_nan` and `allow_inf`.
    pub fn sample_f64(&mut self, min: f64, max: f64, allow_nan: bool, allow_inf: bool) -> f64 {
        let tag = self.next_u64() & 31;
        if allow_nan && tag == 0 {
            f64::NAN
        } else if allow_inf && tag == 1 {
            f64::INFINITY
        } else if allow_inf && tag == 2 {
            f64::NEG_INFINITY
        } else if tag == 3 {
            min
        } else if tag == 4 {
            max
        } else if tag == 5 && min <= 0.0 && max >= 0.0 {
            0.0
        } else if tag == 6 && min <= -0.0 && max >= -0.0 {
            -0.0
        } else if tag == 7 && min <= 1.0 && max >= 1.0 {
            1.0
        } else if tag == 8 && min <= -1.0 && max >= -1.0 {
            -1.0
        } else {
            interpolate_f64(self.next_u64(), min, max)
        }
    }

    /// Samples a Unicode scalar value in `charset`.
    pub fn sample_char_u32(&mut self, charset: Charset) -> u32 {
        let raw = self.next_u64();
        project_char_u32(raw, charset)
    }
}

impl ValueSpec {
    /// Generates a fresh random byte buffer conforming to this [`ValueSpec`].
    pub fn generate(&self, rng: &mut Rng) -> Vec<u8> {
        let mut out = Vec::new();
        self.write_random(rng, &mut out);
        out
    }

    /// Appends a random value conforming to this [`ValueSpec`] into `out`.
    pub fn write_random(&self, rng: &mut Rng, out: &mut Vec<u8>) {
        match self {
            Self::Unit => {}
            Self::Bool => out.push((rng.next_u64() & 1) as u8),
            Self::Int { width, min, max } => {
                let value = rng.sample_i64(*min, *max);
                write_int_le(out, *width, value);
            }
            Self::UInt { width, min, max } => {
                let value = rng.sample_u64(*min, *max);
                write_uint_le(out, *width, value);
            }
            Self::Float {
                width,
                min,
                max,
                allow_nan,
                allow_inf,
            } => {
                let value = rng.sample_f64(*min, *max, *allow_nan, *allow_inf);
                match width {
                    FloatWidth::F32 => {
                        out.extend_from_slice(&(value as f32).to_bits().to_le_bytes())
                    }
                    FloatWidth::F64 => out.extend_from_slice(&value.to_bits().to_le_bytes()),
                }
            }
            Self::Char(charset) => {
                let scalar = rng.sample_char_u32(*charset);
                out.extend_from_slice(&scalar.to_le_bytes());
            }
            Self::String {
                charset,
                min_len,
                max_len,
            } => {
                let len = rng.sample_u64(*min_len, *max_len);
                out.extend_from_slice(&len.to_le_bytes());
                for _ in 0..len {
                    let scalar = rng.sample_char_u32(*charset);
                    out.extend_from_slice(&scalar.to_le_bytes());
                }
            }
            Self::Array {
                elem,
                min_len,
                max_len,
            } => {
                let len = rng.sample_u64(*min_len, *max_len);
                out.extend_from_slice(&len.to_le_bytes());
                for _ in 0..len {
                    elem.write_random(rng, out);
                }
            }
            Self::Tuple(items) => {
                for item in items {
                    item.write_random(rng, out);
                }
            }
        }
    }

    /// Mutates a payload buffer in place according to this [`ValueSpec`].
    pub fn mutate(&self, buf: &mut Vec<u8>, rng: &mut Rng) {
        if matches!(self, Self::Unit) {
            buf.clear();
            return;
        }
        // Choose between structure-aware re-mutation and low-level byte mutation.
        let mut cursor = 0usize;
        if !self.mutate_at(buf, &mut cursor, rng) || (rng.next_u64() & 3) == 0 {
            mutate_raw_bytes(buf, 0, rng);
        }
    }

    fn mutate_at(&self, buf: &mut Vec<u8>, offset: &mut usize, rng: &mut Rng) -> bool {
        match self {
            Self::Unit => true,
            Self::Bool => {
                if *offset >= buf.len() {
                    return false;
                }
                if (rng.next_u64() & 1) == 0 {
                    buf[*offset] ^= 1;
                }
                *offset += 1;
                true
            }
            Self::Int { width, min, max } => {
                let n = width.byte_len();
                if *offset + n > buf.len() {
                    return false;
                }
                if (rng.next_u64() & 1) == 0 {
                    let cur = read_int_le(&buf[*offset..*offset + n], *width);
                    let next = if (rng.next_u64() & 3) == 0 {
                        rng.sample_i64(*min, *max)
                    } else {
                        let delta = ((rng.next_u64() % 33) as i64) - 16;
                        project_i64(cur.wrapping_add(delta), *min, *max)
                    };
                    let mut tmp = Vec::with_capacity(n);
                    write_int_le(&mut tmp, *width, next);
                    buf[*offset..*offset + n].copy_from_slice(&tmp);
                }
                *offset += n;
                true
            }
            Self::UInt { width, min, max } => {
                let n = width.byte_len();
                if *offset + n > buf.len() {
                    return false;
                }
                if (rng.next_u64() & 1) == 0 {
                    let cur = read_uint_le(&buf[*offset..*offset + n], *width);
                    let next = if (rng.next_u64() & 3) == 0 {
                        rng.sample_u64(*min, *max)
                    } else {
                        let delta = (rng.next_u64() % 33) as i64 - 16;
                        project_u64(cur.wrapping_add_signed(delta), *min, *max)
                    };
                    let mut tmp = Vec::with_capacity(n);
                    write_uint_le(&mut tmp, *width, next);
                    buf[*offset..*offset + n].copy_from_slice(&tmp);
                }
                *offset += n;
                true
            }
            Self::Float {
                width,
                min,
                max,
                allow_nan,
                allow_inf,
            } => {
                let n = width.byte_len();
                if *offset + n > buf.len() {
                    return false;
                }
                if (rng.next_u64() & 1) == 0 {
                    let val = rng.sample_f64(*min, *max, *allow_nan, *allow_inf);
                    match width {
                        FloatWidth::F32 => {
                            buf[*offset..*offset + 4]
                                .copy_from_slice(&(val as f32).to_bits().to_le_bytes());
                        }
                        FloatWidth::F64 => {
                            buf[*offset..*offset + 8].copy_from_slice(&val.to_bits().to_le_bytes());
                        }
                    }
                }
                *offset += n;
                true
            }
            Self::Char(charset) => {
                if *offset + 4 > buf.len() {
                    return false;
                }
                if (rng.next_u64() & 1) == 0 {
                    let scalar = rng.sample_char_u32(*charset);
                    buf[*offset..*offset + 4].copy_from_slice(&scalar.to_le_bytes());
                }
                *offset += 4;
                true
            }
            Self::String {
                charset,
                min_len,
                max_len,
            } => {
                if *offset + 8 > buf.len() {
                    return false;
                }
                let raw_len = u64::from_le_bytes(buf[*offset..*offset + 8].try_into().unwrap());
                let len = project_u64(raw_len, *min_len, *max_len) as usize;
                let byte_span = 8 + len.saturating_mul(4);
                if *offset + byte_span > buf.len() {
                    return false;
                }
                if (rng.next_u64() & 3) == 0 {
                    // Regenerate string with a new valid length.
                    let mut replacement = Vec::new();
                    self.write_random(rng, &mut replacement);
                    let new_span = replacement.len();
                    buf.splice(*offset..*offset + byte_span, replacement);
                    *offset += new_span;
                } else {
                    // Mutate one or more characters in place.
                    if len > 0 {
                        let idx = (rng.next_u64() as usize) % len;
                        let pos = *offset + 8 + idx * 4;
                        let scalar = rng.sample_char_u32(*charset);
                        buf[pos..pos + 4].copy_from_slice(&scalar.to_le_bytes());
                    }
                    *offset += byte_span;
                }
                true
            }
            Self::Array {
                elem,
                min_len,
                max_len,
            } => {
                if *offset + 8 > buf.len() {
                    return false;
                }
                let raw_len = u64::from_le_bytes(buf[*offset..*offset + 8].try_into().unwrap());
                let len = project_u64(raw_len, *min_len, *max_len) as usize;
                let start = *offset;
                *offset += 8;
                for _ in 0..len {
                    if !elem.mutate_at(buf, offset, rng) {
                        return false;
                    }
                }
                if (rng.next_u64() & 7) == 0 {
                    let end = *offset;
                    let mut replacement = Vec::new();
                    self.write_random(rng, &mut replacement);
                    let new_len = replacement.len();
                    buf.splice(start..end, replacement);
                    *offset = start + new_len;
                }
                true
            }
            Self::Tuple(items) => {
                for item in items {
                    if !item.mutate_at(buf, offset, rng) {
                        return false;
                    }
                }
                true
            }
        }
    }
}

impl SuiteSpec {
    /// Generates a suite-level input buffer for `case_idx` (8-byte little-endian
    /// case index followed by that case's random payload).
    pub fn generate_for_case(&self, case_idx: usize, rng: &mut Rng) -> Vec<u8> {
        let mut out = Vec::new();
        out.extend_from_slice(&(case_idx as u64).to_le_bytes());
        if let Some(case) = self.cases.get(case_idx) {
            case.input.write_random(rng, &mut out);
        }
        out
    }

    /// Mutates a suite-level input buffer for `case_idx` in place, keeping the
    /// 8-byte little-endian case selector prefix intact.
    pub fn mutate_for_case(&self, case_idx: usize, buf: &mut Vec<u8>, rng: &mut Rng) {
        let Some(case) = self.cases.get(case_idx) else {
            return;
        };
        if buf.len() < 8 {
            *buf = self.generate_for_case(case_idx, rng);
            return;
        }
        buf[..8].copy_from_slice(&(case_idx as u64).to_le_bytes());
        let mut payload = buf.split_off(8);
        case.input.mutate(&mut payload, rng);
        buf.extend_from_slice(&payload);
    }
}

fn mutate_raw_bytes(buf: &mut [u8], start: usize, rng: &mut Rng) {
    if buf.len() <= start {
        return;
    }
    let span = buf.len() - start;
    let idx = start + (rng.next_u64() as usize) % span;
    match rng.next_u64() & 3 {
        0 => {
            let bit = (rng.next_u64() & 7) as u8;
            buf[idx] ^= 1u8 << bit;
        }
        1 => {
            let delta = ((rng.next_u64() % 31) as i8).wrapping_sub(15) as u8;
            buf[idx] = buf[idx].wrapping_add(delta);
        }
        2 => {
            buf[idx] = match rng.next_u64() & 3 {
                0 => 0x00,
                1 => 0xff,
                2 => 0x7f,
                _ => 0x80,
            };
        }
        _ => {
            buf[idx] = rng.next_u64() as u8;
        }
    }
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

fn interpolate_f64(bits: u64, min: f64, max: f64) -> f64 {
    if min >= max {
        min
    } else {
        let u = ((bits >> 11) as f64) * (1.0 / 9007199254740992.0);
        let v = (1.0 - u) * min + u * max;
        v.clamp(min, max)
    }
}

fn project_char_u32(raw: u64, charset: Charset) -> u32 {
    match charset {
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
    }
}
