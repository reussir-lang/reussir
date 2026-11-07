//! Operating-system entropy backed by [`getrandom`].

use crate::rc::Rc;

// Status codes decoded by `library/std/src/random.rr`.
const OK: i64 = 0;
const UNSUPPORTED: i64 = 1;
const OTHER: i64 = 2;
const OS_ERROR_TAG: i64 = 3;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct Inner {
    status: i64,
    value: u64,
}

/// FFI handle carrying the outcome of one 64-bit OS entropy request.
#[derive(Clone)]
#[repr(transparent)]
pub struct GetRandomResult(Rc<Inner>);

/// Encodes a [`getrandom::Error`] into the status word decoded by `std::random`.
#[inline]
pub fn encode_error(err: getrandom::Error) -> i64 {
    if err == getrandom::Error::UNSUPPORTED {
        UNSUPPORTED
    } else if let Some(code) = err.raw_os_error() {
        (i64::from(code) << 2) | OS_ERROR_TAG
    } else {
        OTHER
    }
}

impl GetRandomResult {
    #[inline]
    pub fn entropy64() -> Self {
        match getrandom::u64() {
            Ok(value) => Self(Rc::new(Inner { status: OK, value })),
            Err(err) => Self(Rc::new(Inner {
                status: encode_error(err),
                value: 0,
            })),
        }
    }

    #[inline]
    pub fn status(&self) -> i64 {
        self.0.status
    }

    #[inline]
    pub fn value(&self) -> u64 {
        self.0.value
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn encodes_unsupported_and_custom_errors() {
        assert_eq!(encode_error(getrandom::Error::UNSUPPORTED), UNSUPPORTED);
        assert_eq!(encode_error(getrandom::Error::UNEXPECTED), OTHER);
        assert_eq!(encode_error(getrandom::Error::new_custom(42)), OTHER);
    }

    #[test]
    fn entropy64_returns_ok_result() {
        let a = GetRandomResult::entropy64();
        assert_eq!(a.status(), OK);
        let b = GetRandomResult::entropy64();
        assert_eq!(b.status(), OK);
        if a.value() == b.value() {
            let c = GetRandomResult::entropy64();
            assert_eq!(c.status(), OK);
            assert_ne!(a.value(), c.value());
        }
    }
}
