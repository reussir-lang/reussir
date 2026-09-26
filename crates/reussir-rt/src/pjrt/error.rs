use std::fmt;

use super::sys::PJRT_Error_Code;

#[derive(Clone, Debug)]
pub(super) struct Error {
    /// An upstream status code, or `None` for loading/ABI errors.
    pub code: Option<PJRT_Error_Code>,
    pub message: String,
}

impl Error {
    pub(super) fn local(message: impl Into<String>) -> Self {
        Self {
            code: None,
            message: message.into(),
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.message)?;
        if let Some(code) = self.code {
            write!(f, " (PjRt status {code})")?;
        }
        Ok(())
    }
}
impl std::error::Error for Error {}
pub(super) type Result<T> = std::result::Result<T, Error>;
