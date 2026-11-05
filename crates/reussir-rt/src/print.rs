//! Output sinks for already formatted UTF-8 text.

use std::io::{self, Write};

fn error_code(error: io::Error) -> i32 {
    match error.kind() {
        io::ErrorKind::BrokenPipe => 1,
        io::ErrorKind::PermissionDenied => 2,
        io::ErrorKind::WriteZero => 3,
        _ => 4,
    }
}

fn write_and_flush(writer: &mut impl Write, bytes: &[u8]) -> i32 {
    if let Err(error) = writer.write_all(bytes) {
        return error_code(error);
    }
    loop {
        match writer.flush() {
            Ok(()) => return 0,
            Err(error) if error.kind() == io::ErrorKind::Interrupted => continue,
            Err(error) => return 256 + error_code(error),
        }
    }
}

/// Zero means success; low bits identify the error, bit 8 a flush failure.
pub fn write_stdout(bytes: &[u8]) -> i32 {
    write_and_flush(&mut io::stdout().lock(), bytes)
}

/// Uses the same status encoding as [`write_stdout`].
pub fn write_stderr(bytes: &[u8]) -> i32 {
    write_and_flush(&mut io::stderr().lock(), bytes)
}

#[cfg(test)]
mod tests {
    use super::*;

    struct Sink {
        bytes: Vec<u8>,
        interrupt_write: bool,
        interrupt_flush: bool,
        write_error: Option<io::ErrorKind>,
        flush_error: Option<io::ErrorKind>,
        flushed: bool,
    }

    impl Sink {
        fn new() -> Self {
            Self {
                bytes: Vec::new(),
                interrupt_write: true,
                interrupt_flush: true,
                write_error: None,
                flush_error: None,
                flushed: false,
            }
        }
    }

    impl Write for Sink {
        fn write(&mut self, bytes: &[u8]) -> io::Result<usize> {
            if std::mem::take(&mut self.interrupt_write) {
                return Err(io::ErrorKind::Interrupted.into());
            }
            if let Some(error) = self.write_error {
                return Err(error.into());
            }
            let count = bytes.len().min(2);
            self.bytes.extend_from_slice(&bytes[..count]);
            Ok(count)
        }
        fn flush(&mut self) -> io::Result<()> {
            if std::mem::take(&mut self.interrupt_flush) {
                return Err(io::ErrorKind::Interrupted.into());
            }
            if let Some(error) = self.flush_error {
                return Err(error.into());
            }
            self.flushed = true;
            Ok(())
        }
    }

    #[test]
    fn retries_interruptions_and_short_writes() {
        let mut sink = Sink::new();
        assert_eq!(write_and_flush(&mut sink, "hello λ".as_bytes()), 0);
        assert_eq!(sink.bytes, "hello λ".as_bytes());
        assert!(sink.flushed);
    }

    #[test]
    fn distinguishes_write_and_flush_failures() {
        for (kind, code) in [
            (io::ErrorKind::BrokenPipe, 1),
            (io::ErrorKind::PermissionDenied, 2),
            (io::ErrorKind::WriteZero, 3),
            (io::ErrorKind::Other, 4),
        ] {
            let mut sink = Sink::new();
            sink.write_error = Some(kind);
            assert_eq!(write_and_flush(&mut sink, b"hello"), code);
            assert!(!sink.flushed);
            let mut sink = Sink::new();
            sink.flush_error = Some(kind);
            assert_eq!(write_and_flush(&mut sink, b"hello"), 256 + code);
            assert_eq!(sink.bytes, b"hello");
        }
    }

    #[test]
    fn reports_zero_progress() {
        struct Zero;
        impl Write for Zero {
            fn write(&mut self, _: &[u8]) -> io::Result<usize> {
                Ok(0)
            }
            fn flush(&mut self) -> io::Result<()> {
                panic!("must not flush after failure")
            }
        }
        assert_eq!(write_and_flush(&mut Zero, b"data"), 3);
    }

    #[test]
    fn does_not_replay_partial_output() {
        struct Partial(Vec<u8>);
        impl Write for Partial {
            fn write(&mut self, bytes: &[u8]) -> io::Result<usize> {
                if !self.0.is_empty() {
                    return Err(io::ErrorKind::BrokenPipe.into());
                }
                self.0.extend_from_slice(&bytes[..2]);
                Ok(2)
            }
            fn flush(&mut self) -> io::Result<()> {
                panic!("must not flush after failure")
            }
        }
        let mut sink = Partial(Vec::new());
        assert_eq!(write_and_flush(&mut sink, b"hello"), 1);
        assert_eq!(sink.0, b"he");
    }

    #[test]
    fn empty_output_still_flushes() {
        let mut sink = Sink::new();
        assert_eq!(write_and_flush(&mut sink, b""), 0);
        assert!(sink.bytes.is_empty());
        assert!(sink.flushed);
    }
}
