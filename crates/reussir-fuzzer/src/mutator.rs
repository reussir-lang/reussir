//! Spec-aware input generation and mutation powered by [`mutatis`].

use mutatis::{
    Candidates, Context, Generate, Mutate, Result as MutatisResult, Session, mutators as m,
};

use crate::spec::{Charset, FloatWidth, IntWidth, SuiteSpec, ValueSpec};
use winnow::binary::{
    le_f32, le_f64, le_i8, le_i16, le_i32, le_i64, le_u8, le_u16, le_u32, le_u64,
};
use winnow::combinator::repeat;
use winnow::error::ContextError;
use winnow::prelude::*;
use winnow::token::take;

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

/// [`mutatis`] mutator and generator for characters in a [`Charset`].
#[derive(Clone, Copy, Debug)]
pub struct CharsetMutator(pub Charset);

impl Mutate<char> for CharsetMutator {
    fn mutate(&mut self, c: &mut Candidates<'_>, value: &mut char) -> MutatisResult<()> {
        match self.0 {
            Charset::Ascii => m::ascii_char().mutate(c, value),
            Charset::AsciiPrintable => m::mrange(' '..='~').mutate(c, value),
            Charset::Digits => m::mrange('0'..='9').mutate(c, value),
            Charset::Alpha => {
                if c.shrink() {
                    if *value > 'a' {
                        m::mrange('A'..='Z')
                            .or(m::mrange('a'..='z'))
                            .mutate(c, value)
                    } else if *value > 'A' {
                        m::mrange('A'..='Z').mutate(c, value)
                    } else {
                        Ok(())
                    }
                } else {
                    m::mrange('A'..='Z')
                        .or(m::mrange('a'..='z'))
                        .mutate(c, value)
                }
            }
            Charset::Alphanumeric => {
                if c.shrink() {
                    if *value > 'a' {
                        m::mrange('0'..='9')
                            .or(m::mrange('A'..='Z'))
                            .or(m::mrange('a'..='z'))
                            .mutate(c, value)
                    } else if *value > 'A' {
                        m::mrange('0'..='9')
                            .or(m::mrange('A'..='Z'))
                            .mutate(c, value)
                    } else if *value > '0' {
                        m::mrange('0'..='9').mutate(c, value)
                    } else {
                        Ok(())
                    }
                } else {
                    m::mrange('0'..='9')
                        .or(m::mrange('A'..='Z'))
                        .or(m::mrange('a'..='z'))
                        .mutate(c, value)
                }
            }
            Charset::Identifier => {
                if c.shrink() {
                    if *value > 'a' {
                        m::mrange('0'..='9')
                            .or(m::mrange('A'..='Z'))
                            .or(m::just('_'))
                            .or(m::mrange('a'..='z'))
                            .mutate(c, value)
                    } else if *value > '_' {
                        m::mrange('0'..='9')
                            .or(m::mrange('A'..='Z'))
                            .or(m::just('_'))
                            .mutate(c, value)
                    } else if *value > 'A' {
                        m::mrange('0'..='9')
                            .or(m::mrange('A'..='Z'))
                            .mutate(c, value)
                    } else if *value > '0' {
                        m::mrange('0'..='9').mutate(c, value)
                    } else {
                        Ok(())
                    }
                } else {
                    m::mrange('0'..='9')
                        .or(m::mrange('A'..='Z'))
                        .or(m::mrange('a'..='z'))
                        .or(m::just('_'))
                        .mutate(c, value)
                }
            }
            Charset::Utf8 => m::char().mutate(c, value),
        }
    }
}

impl Generate<char> for CharsetMutator {
    fn generate(&mut self, ctx: &mut Context) -> MutatisResult<char> {
        match self.0 {
            Charset::Ascii => m::ascii_char().generate(ctx),
            Charset::AsciiPrintable => m::mrange(' '..='~').generate(ctx),
            Charset::Digits => m::mrange('0'..='9').generate(ctx),
            Charset::Alpha => m::mrange('A'..='Z').or(m::mrange('a'..='z')).generate(ctx),
            Charset::Alphanumeric => m::mrange('0'..='9')
                .or(m::mrange('A'..='Z'))
                .or(m::mrange('a'..='z'))
                .generate(ctx),
            Charset::Identifier => m::mrange('0'..='9')
                .or(m::mrange('A'..='Z'))
                .or(m::mrange('a'..='z'))
                .or(m::just('_'))
                .generate(ctx),
            Charset::Utf8 => m::char().generate(ctx),
        }
    }
}

/// [`mutatis`] mutator and generator for a [`ValueSpec`].
#[derive(Clone, Copy, Debug)]
pub struct SpecMutator<'a>(pub &'a ValueSpec);

impl Mutate<FuzzValue> for SpecMutator<'_> {
    fn mutate(&mut self, c: &mut Candidates<'_>, value: &mut FuzzValue) -> MutatisResult<()> {
        match (self.0, value) {
            (ValueSpec::Unit, FuzzValue::Unit) => Ok(()),
            (ValueSpec::Bool, FuzzValue::Bool(b)) => m::bool().mutate(c, b),
            (ValueSpec::Int { width, min, max }, FuzzValue::Int(w, v)) => {
                *w = *width;
                let (lo, hi) = clamped_signed_bounds(*width, *min, *max);
                m::mrange(lo..=hi).mutate(c, v)
            }
            (ValueSpec::UInt { width, min, max }, FuzzValue::UInt(w, v)) => {
                *w = *width;
                let (lo, hi) = clamped_unsigned_bounds(*width, *min, *max);
                m::mrange(lo..=hi).mutate(c, v)
            }
            (
                ValueSpec::Float {
                    width,
                    min,
                    max,
                    allow_nan,
                    allow_inf,
                },
                FuzzValue::Float(w, v),
            ) => {
                *w = *width;
                let (lo, hi) = if *min <= *max {
                    (*min, *max)
                } else {
                    (*max, *min)
                };
                let nan = *allow_nan;
                let inf = *allow_inf;
                match width {
                    FloatWidth::F32 => {
                        let mut cur = *v as f32;
                        let mut mutator = m::f32()
                            .or(m::just(lo as f32))
                            .or(m::just(hi as f32))
                            .map(|_ctx, x| {
                                *x = constrain_f64(*x as f64, lo, hi, nan, inf) as f32;
                                Ok(())
                            });
                        let res = mutator.mutate(c, &mut cur);
                        *v = cur as f64;
                        res
                    }
                    FloatWidth::F64 => {
                        let mut mutator =
                            m::f64().or(m::just(lo)).or(m::just(hi)).map(|_ctx, x| {
                                *x = constrain_f64(*x, lo, hi, nan, inf);
                                Ok(())
                            });
                        mutator.mutate(c, v)
                    }
                }
            }
            (ValueSpec::Char(charset), FuzzValue::Char(ch)) => {
                CharsetMutator(*charset).mutate(c, ch)
            }
            (
                ValueSpec::String {
                    charset,
                    min_len,
                    max_len,
                },
                FuzzValue::String(s),
            ) => {
                let (lo, hi) = normalized_len_bounds(*min_len, *max_len);
                let cs = *charset;
                if c.shrink() && s.chars().count() <= lo {
                    if !s.is_empty() {
                        c.mutation(|ctx| {
                            let mut chars: Vec<char> = s.chars().collect();
                            let idx = m::mrange(0..=chars.len() - 1).generate(ctx)?;
                            chars[idx] = CharsetMutator(cs).generate(ctx)?;
                            *s = chars.into_iter().collect();
                            Ok(())
                        })?;
                    }
                    Ok(())
                } else {
                    let mut mutator = m::string(CharsetMutator(cs))
                        .map(|ctx, val| constrain_string_len(ctx, val, cs, lo, hi));
                    mutator.mutate(c, s)
                }
            }
            (
                ValueSpec::Array {
                    elem,
                    min_len,
                    max_len,
                },
                FuzzValue::Array(items),
            ) => {
                let (lo, hi) = normalized_len_bounds(*min_len, *max_len);
                if c.shrink() && items.len() <= lo {
                    let mut elem_mut = SpecMutator(elem);
                    for item in items.iter_mut() {
                        elem_mut.mutate(c, item)?;
                    }
                    Ok(())
                } else {
                    let mut mutator = m::vec(SpecMutator(elem))
                        .map(|ctx, vec| constrain_vec_len(ctx, vec, elem, lo, hi));
                    mutator.mutate(c, items)
                }
            }
            (ValueSpec::Tuple(specs), FuzzValue::Tuple(values)) => {
                if specs.len() != values.len() {
                    let spec = self.0;
                    return c.mutation(|ctx| {
                        *values = match SpecMutator(spec).generate(ctx)? {
                            FuzzValue::Tuple(v) => v,
                            _ => Vec::new(),
                        };
                        Ok(())
                    });
                }
                for (spec, item) in specs.iter().zip(values.iter_mut()) {
                    SpecMutator(spec).mutate(c, item)?;
                }
                Ok(())
            }
            (spec, mismatched) => c.mutation(|ctx| {
                *mismatched = SpecMutator(spec).generate(ctx)?;
                Ok(())
            }),
        }
    }
}

impl Generate<FuzzValue> for SpecMutator<'_> {
    fn generate(&mut self, ctx: &mut Context) -> MutatisResult<FuzzValue> {
        match self.0 {
            ValueSpec::Unit => Ok(FuzzValue::Unit),
            ValueSpec::Bool => Ok(FuzzValue::Bool(m::bool().generate(ctx)?)),
            ValueSpec::Int { width, min, max } => {
                let (lo, hi) = clamped_signed_bounds(*width, *min, *max);
                Ok(FuzzValue::Int(*width, m::mrange(lo..=hi).generate(ctx)?))
            }
            ValueSpec::UInt { width, min, max } => {
                let (lo, hi) = clamped_unsigned_bounds(*width, *min, *max);
                Ok(FuzzValue::UInt(*width, m::mrange(lo..=hi).generate(ctx)?))
            }
            ValueSpec::Float {
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
                let raw = match width {
                    FloatWidth::F32 => m::f32().generate(ctx)? as f64,
                    FloatWidth::F64 => m::f64().generate(ctx)?,
                };
                let v = constrain_f64(raw, lo, hi, *allow_nan, *allow_inf);
                Ok(FuzzValue::Float(*width, v))
            }
            ValueSpec::Char(charset) => {
                Ok(FuzzValue::Char(CharsetMutator(*charset).generate(ctx)?))
            }
            ValueSpec::String {
                charset,
                min_len,
                max_len,
            } => {
                let (lo, hi) = normalized_len_bounds(*min_len, *max_len);
                let len = m::mrange(lo..=hi).generate(ctx)?;
                let mut s = String::with_capacity(len);
                let mut char_gen = CharsetMutator(*charset);
                for _ in 0..len {
                    s.push(char_gen.generate(ctx)?);
                }
                Ok(FuzzValue::String(s))
            }
            ValueSpec::Array {
                elem,
                min_len,
                max_len,
            } => {
                let (lo, hi) = normalized_len_bounds(*min_len, *max_len);
                let len = m::mrange(lo..=hi).generate(ctx)?;
                let mut items = Vec::with_capacity(len);
                let mut elem_gen = SpecMutator(elem);
                for _ in 0..len {
                    items.push(elem_gen.generate(ctx)?);
                }
                Ok(FuzzValue::Array(items))
            }
            ValueSpec::Tuple(specs) => {
                let mut items = Vec::with_capacity(specs.len());
                for spec in specs {
                    items.push(SpecMutator(spec).generate(ctx)?);
                }
                Ok(FuzzValue::Tuple(items))
            }
        }
    }
}

/// [`mutatis`] mutator and generator for a [`SuiteSpec`].
#[derive(Clone, Copy, Debug)]
pub struct SuiteMutator<'a>(pub &'a SuiteSpec);

impl Mutate<SuiteInput> for SuiteMutator<'_> {
    fn mutate(&mut self, c: &mut Candidates<'_>, value: &mut SuiteInput) -> MutatisResult<()> {
        let indices = fuzz_indices(self.0);
        SelectedSuiteMutator {
            suite: self.0,
            indices: &indices,
        }
        .mutate(c, value)
    }
}

impl Generate<SuiteInput> for SuiteMutator<'_> {
    fn generate(&mut self, ctx: &mut Context) -> MutatisResult<SuiteInput> {
        let indices = fuzz_indices(self.0);
        SelectedSuiteMutator {
            suite: self.0,
            indices: &indices,
        }
        .generate(ctx)
    }
}

fn fuzz_indices(suite: &SuiteSpec) -> Vec<usize> {
    suite
        .cases
        .iter()
        .enumerate()
        .filter_map(|(index, case)| (!case.input.is_unit()).then_some(index))
        .collect()
}

struct SelectedSuiteMutator<'a> {
    suite: &'a SuiteSpec,
    indices: &'a [usize],
}

impl Mutate<SuiteInput> for SelectedSuiteMutator<'_> {
    fn mutate(&mut self, c: &mut Candidates<'_>, value: &mut SuiteInput) -> MutatisResult<()> {
        if self.indices.is_empty() {
            return Ok(());
        }
        if !self.indices.contains(&value.case_idx) {
            return c.mutation(|ctx| {
                *value = self.generate(ctx)?;
                Ok(())
            });
        }
        let shrink = c.shrink();
        c.mutation(|ctx| {
            let seed = m::u64().generate(ctx)?;
            Session::new().seed(seed).shrink(shrink).mutate_with(
                &mut SpecMutator(&self.suite.cases[value.case_idx].input),
                &mut value.payload,
            )
        })?;
        if !shrink && self.indices.len() > 1 {
            c.mutation(|ctx| {
                *value = self.generate(ctx)?;
                Ok(())
            })?;
        }
        Ok(())
    }
}

impl Generate<SuiteInput> for SelectedSuiteMutator<'_> {
    fn generate(&mut self, ctx: &mut Context) -> MutatisResult<SuiteInput> {
        if self.indices.is_empty() {
            return Ok(SuiteInput {
                case_idx: 0,
                payload: FuzzValue::Unit,
            });
        }
        let pick = m::mrange(0..=self.indices.len() - 1).generate(ctx)?;
        let case_idx = self.indices[pick];
        let payload = SpecMutator(&self.suite.cases[case_idx].input).generate(ctx)?;
        Ok(SuiteInput { case_idx, payload })
    }
}

impl ValueSpec {
    /// Generates a fresh random byte buffer conforming to this [`ValueSpec`] using [`mutatis`].
    pub fn generate(&self, session: &mut Session) -> Vec<u8> {
        let mut mutator = SpecMutator(self);
        match session.generate_with(&mut mutator) {
            Ok(val) => val.to_bytes(),
            Err(_) => Vec::new(),
        }
    }

    /// Mutates a payload buffer in place according to this [`ValueSpec`] using [`mutatis`].
    pub fn mutate(&self, buf: &mut Vec<u8>, session: &mut Session) {
        let mut cursor = buf.as_slice();
        let mut mutator = SpecMutator(self);
        let mut val = match self.decode_value(&mut cursor) {
            Some(v) => v,
            None => match session.generate_with(&mut mutator) {
                Ok(v) => v,
                Err(_) => return,
            },
        };
        let _ = session.mutate_with(&mut mutator, &mut val);
        buf.clear();
        val.encode_into(buf);
    }

    /// Decodes a structured [`FuzzValue`] from `cursor`, projecting any raw bytes
    /// into this specification's domain bounds.
    pub fn decode_value(&self, cursor: &mut &[u8]) -> Option<FuzzValue> {
        self.parse_value(cursor).ok()
    }

    fn parse_value(&self, input: &mut &[u8]) -> ModalResult<FuzzValue, ContextError> {
        match self {
            Self::Unit => Ok(FuzzValue::Unit),
            Self::Bool => {
                let b = le_u8.parse_next(input)?;
                Ok(FuzzValue::Bool((b & 1) != 0))
            }
            Self::Int { width, min, max } => {
                let raw = match width {
                    IntWidth::I8 => le_i8.parse_next(input)? as i64,
                    IntWidth::I16 => le_i16.parse_next(input)? as i64,
                    IntWidth::I32 => le_i32.parse_next(input)? as i64,
                    IntWidth::I64 => le_i64.parse_next(input)?,
                };
                let (lo, hi) = clamped_signed_bounds(*width, *min, *max);
                Ok(FuzzValue::Int(*width, project_i64(raw, lo, hi)))
            }
            Self::UInt { width, min, max } => {
                let raw = match width {
                    IntWidth::I8 => le_u8.parse_next(input)? as u64,
                    IntWidth::I16 => le_u16.parse_next(input)? as u64,
                    IntWidth::I32 => le_u32.parse_next(input)? as u64,
                    IntWidth::I64 => le_u64.parse_next(input)?,
                };
                let (lo, hi) = clamped_unsigned_bounds(*width, *min, *max);
                Ok(FuzzValue::UInt(*width, project_u64(raw, lo, hi)))
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
                        let raw = le_f32.parse_next(input)? as f64;
                        Ok(FuzzValue::Float(
                            FloatWidth::F32,
                            constrain_f64(raw, lo, hi, *allow_nan, *allow_inf),
                        ))
                    }
                    FloatWidth::F64 => {
                        let raw = le_f64.parse_next(input)?;
                        Ok(FuzzValue::Float(
                            FloatWidth::F64,
                            constrain_f64(raw, lo, hi, *allow_nan, *allow_inf),
                        ))
                    }
                }
            }
            Self::Char(charset) => {
                let raw = le_u32.parse_next(input)?;
                let ch = char::from_u32(raw)
                    .filter(|c| charset_contains(*charset, *c))
                    .unwrap_or_else(|| project_char(raw as u64, *charset));
                Ok(FuzzValue::Char(ch))
            }
            Self::String {
                charset,
                min_len,
                max_len,
            } => {
                let byte_len = le_u64.try_map(usize::try_from).parse_next(input)?;
                let raw_bytes = take(byte_len).parse_next(input)?;
                let (lo, hi) = normalized_len_bounds(*min_len, *max_len);
                let s = decode_and_sanitize_string(raw_bytes, *charset, lo, hi);
                Ok(FuzzValue::String(s))
            }
            Self::Array {
                elem,
                min_len,
                max_len,
            } => {
                let raw_len = le_u64.parse_next(input)?;
                let (lo, hi) = normalized_len_bounds(*min_len, *max_len);
                let count = project_u64(raw_len, lo as u64, hi as u64) as usize;
                let items = repeat(count, |inp: &mut &[u8]| elem.parse_value(inp))
                    .parse_next(input)?;
                Ok(FuzzValue::Array(items))
            }
            Self::Tuple(specs) => {
                let mut items = Vec::with_capacity(specs.len());
                for spec in specs {
                    items.push(spec.parse_value(input)?);
                }
                Ok(FuzzValue::Tuple(items))
            }
        }
    }
}

/// Callback signature matching [`libfuzzer_sys::fuzzer_mutate`].
pub type RawMutateFn = fn(&mut [u8], usize, usize) -> usize;

impl SuiteSpec {
    /// Generates a suite-level input buffer for `case_idx` using [`mutatis`].
    pub fn generate_for_case(&self, case_idx: usize, session: &mut Session) -> Vec<u8> {
        let mut out = Vec::new();
        out.extend_from_slice(&(case_idx as u64).to_le_bytes());
        if let Some(case) = self.cases.get(case_idx) {
            let mut mutator = SpecMutator(&case.input);
            if let Ok(val) = session.generate_with(&mut mutator) {
                val.encode_into(&mut out);
            }
        }
        out
    }

    /// Mutates a suite-level input buffer for `case_idx` in place using [`mutatis`],
    /// keeping the 8-byte little-endian case selector prefix intact.
    pub fn mutate_for_case(&self, case_idx: usize, buf: &mut Vec<u8>, session: &mut Session) {
        let Some(case) = self.cases.get(case_idx) else {
            return;
        };
        if buf.len() < 8 {
            *buf = self.generate_for_case(case_idx, session);
            return;
        }
        buf[..8].copy_from_slice(&(case_idx as u64).to_le_bytes());
        let mut payload = buf.split_off(8);
        case.input.mutate(&mut payload, session);
        buf.extend_from_slice(&payload);
    }

    /// Decodes a suite-level input buffer (`case_idx` + `FuzzValue`).
    pub fn decode_input(&self, mut bytes: &[u8]) -> Option<SuiteInput> {
        if self.cases.is_empty() {
            return None;
        }
        let raw_idx = le_u64::<_, ContextError>.parse_next(&mut bytes).ok()?;
        let case_idx = (raw_idx as usize) % self.cases.len();
        let payload = self.cases[case_idx].input.decode_value(&mut bytes)?;
        Some(SuiteInput { case_idx, payload })
    }

    /// Mutates `data[..size]` in place within `max_size` using [`mutatis`] (and
    /// falling back to `raw_mutate` from libFuzzer when unstructured or
    /// exhausted), returning the new buffer size.
    pub fn mutate_buffer(
        &self,
        data: &mut [u8],
        size: usize,
        max_size: usize,
        seed: u32,
        raw_mutate: Option<RawMutateFn>,
    ) -> usize {
        self.mutate_buffer_selected(data, size, max_size, seed, raw_mutate, &fuzz_indices(self))
    }

    /// Mutates only eligible cases, preserving their original suite indices.
    #[allow(clippy::too_many_arguments)]
    pub fn mutate_buffer_selected(
        &self,
        data: &mut [u8],
        size: usize,
        max_size: usize,
        seed: u32,
        raw_mutate: Option<RawMutateFn>,
        indices: &[usize],
    ) -> usize {
        let limit = max_size.min(data.len());
        if self.cases.is_empty() || indices.is_empty() || limit < 8 {
            return 0;
        }
        let mut session = Session::new().seed(u64::from(seed));
        let previous = data.get(..size.min(data.len())).and_then(|bytes| {
            let raw = u64::from_le_bytes(bytes.get(..8)?.try_into().ok()?);
            let index = (raw as usize) % self.cases.len();
            indices.contains(&index).then_some(index)
        });
        let switch = session.generate::<bool>().unwrap_or(false);
        let case_idx = match previous {
            Some(index) if !switch => index,
            _ => {
                let pick = session
                    .generate_with(&mut m::mrange(0..=indices.len() - 1))
                    .unwrap_or(0);
                indices[pick]
            }
        };
        let Some(case) = self.cases.get(case_idx) else {
            return 0;
        };
        let Some(bounded) = case.input.within_budget(limit - 8) else {
            return 0;
        };
        let decoded = if !switch && previous == Some(case_idx) {
            let mut cursor = &data[8..size.min(data.len())];
            bounded.decode_value(&mut cursor)
        } else {
            None
        };
        let value = match decoded {
            Some(mut value) => {
                if session
                    .mutate_with(&mut SpecMutator(&bounded), &mut value)
                    .is_err()
                    && let Some(raw_mutate) = raw_mutate
                {
                    let next = raw_mutate(data, size.min(limit), limit).min(limit);
                    let mut cursor = data.get(8..next).unwrap_or(&[]);
                    if let Some(raw_value) = bounded.decode_value(&mut cursor) {
                        value = raw_value;
                    }
                }
                value
            }
            None => match crate::budget::generate(&case.input, limit - 8, &mut session) {
                Ok(value) => value,
                Err(_) => return 0,
            },
        };
        let mut payload = value.to_bytes();
        if payload.len() > limit - 8 {
            payload = match crate::budget::generate(&case.input, limit - 8, &mut session) {
                Ok(value) => value.to_bytes(),
                Err(_) => return 0,
            };
        }
        debug_assert!(payload.len() <= limit - 8);
        data[..8].copy_from_slice(&(case_idx as u64).to_le_bytes());
        data[8..8 + payload.len()].copy_from_slice(&payload);
        8 + payload.len()
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

fn constrain_string_len(
    ctx: &mut Context,
    s: &mut String,
    charset: Charset,
    min_len: usize,
    max_len: usize,
) -> MutatisResult<()> {
    let mut chars: Vec<char> = s
        .chars()
        .take(max_len)
        .map(|ch| {
            if charset_contains(charset, ch) {
                ch
            } else {
                project_char(ch as u64, charset)
            }
        })
        .collect();
    let mut char_gen = CharsetMutator(charset);
    while chars.len() < min_len {
        chars.push(char_gen.generate(ctx)?);
    }
    *s = chars.into_iter().collect();
    Ok(())
}

fn constrain_vec_len(
    ctx: &mut Context,
    items: &mut Vec<FuzzValue>,
    elem_spec: &ValueSpec,
    min_len: usize,
    max_len: usize,
) -> MutatisResult<()> {
    if items.len() > max_len {
        items.truncate(max_len);
    }
    let mut elem_gen = SpecMutator(elem_spec);
    while items.len() < min_len {
        items.push(elem_gen.generate(ctx)?);
    }
    Ok(())
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

fn write_uint_le(out: &mut Vec<u8>, width: IntWidth, value: u64) {
    match width {
        IntWidth::I8 => out.push(value as u8),
        IntWidth::I16 => out.extend_from_slice(&(value as u16).to_le_bytes()),
        IntWidth::I32 => out.extend_from_slice(&(value as u32).to_le_bytes()),
        IntWidth::I64 => out.extend_from_slice(&value.to_le_bytes()),
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

#[cfg(test)]
mod generation_length_tests {
    use super::*;

    #[test]
    fn generate_declared_lengths_above_4096() {
        let mut session = Session::new().seed(42);
        for spec in [
            ValueSpec::String {
                charset: Charset::Ascii,
                min_len: 5000,
                max_len: 5000,
            },
            ValueSpec::Array {
                elem: Box::new(ValueSpec::Bool),
                min_len: 5000,
                max_len: 5000,
            },
        ] {
            let bytes = spec.generate(&mut session);
            assert_eq!(u64::from_le_bytes(bytes[..8].try_into().unwrap()), 5000);
            let mut cursor = bytes.as_slice();
            assert!(spec.decode_value(&mut cursor).is_some());
            assert!(cursor.is_empty());
        }
    }
}

#[cfg(test)]
mod scheduling_tests {
    use super::*;
    use crate::spec::CaseSpec;

    #[test]
    fn case_switching_does_not_depend_on_payload_candidate_count() {
        let mut counts = Vec::new();
        for len in [1, 1000] {
            let suite = SuiteSpec {
                cases: vec![
                    CaseSpec {
                        path: vec!["array".into()],
                        input: ValueSpec::Array {
                            elem: Box::new(ValueSpec::Bool),
                            min_len: len,
                            max_len: len,
                        },
                    },
                    CaseSpec {
                        path: vec!["other".into()],
                        input: ValueSpec::Bool,
                    },
                ],
            };
            let mut initial = 0u64.to_le_bytes().to_vec();
            initial.extend_from_slice(&len.to_le_bytes());
            initial.resize(16 + len as usize, 0);
            let mut switched = 0;
            for seed in 0..512 {
                let mut bytes = vec![0; 2048];
                bytes[..initial.len()].copy_from_slice(&initial);
                let size = suite.mutate_buffer(&mut bytes, initial.len(), 2048, seed, None);
                switched += usize::from(suite.decode_input(&bytes[..size]).unwrap().case_idx == 1);
            }
            assert!(switched > 64);
            counts.push(switched);
        }
        assert_eq!(counts[0], counts[1]);
    }
}
