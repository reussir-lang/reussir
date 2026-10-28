use super::sys::PJRT_Error_Code;
use std::sync::Arc;

#[derive(Clone, Debug, thiserror::Error)]
pub(super) enum Error {
    #[error("{message}{}", code.map(|code| format!(" (PjRt status {code})")).unwrap_or_default())]
    Backend {
        code: Option<PJRT_Error_Code>,
        message: String,
    },
    #[error("{0}")]
    InvalidInput(String),
    #[error("PJRT compiler cache: {0}")]
    Cache(#[source] Arc<compcache::Error>),
    #[error(transparent)]
    Io(#[from] Arc<std::io::Error>),
    #[error(transparent)]
    Library(#[from] Arc<libloading::Error>),
    #[error(transparent)]
    Stablehlo(#[from] Arc<reussir_stablehlo::Error>),
    #[error(
        "kernel shapes depend on the value of a runtime argument; mark it in the launch configuration's `constants`: {0}"
    )]
    ResidualDynamism(#[source] Arc<reussir_stablehlo::Error>),
    #[error("plugin does not support persistent executable caching")]
    PersistenceUnsupported,
    #[error("cached PJRT executable checksum is invalid")]
    CorruptArtifact,
    #[error("no platform configuration directory available for reussir")]
    ConfigDirectoryUnavailable,
    #[error("PJRT cache returned an unexpected payload kind")]
    UnexpectedPayload,
}
impl Error {
    /// Validation diagnostics, rather than converted errors from another API.
    pub(super) fn local(message: impl Into<String>) -> Self {
        Self::InvalidInput(message.into())
    }
    #[cfg(test)]
    pub(super) fn message(&self) -> std::borrow::Cow<'_, str> {
        match self {
            Self::Backend { message, .. } | Self::InvalidInput(message) => message.into(),
            _ => self.to_string().into(),
        }
    }
    #[cfg(test)]
    pub(super) fn code(&self) -> Option<PJRT_Error_Code> {
        match self {
            Self::Backend { code, .. } => *code,
            _ => None,
        }
    }
}
impl From<compcache::Error> for Error {
    fn from(error: compcache::Error) -> Self {
        match error {
            compcache::Error::Producer(source) => match source.downcast::<Self>() {
                Ok(source) => *source,
                Err(source) => Self::Cache(Arc::new(compcache::Error::Producer(source))),
            },
            error => Self::Cache(Arc::new(error)),
        }
    }
}
pub(super) type Result<T> = std::result::Result<T, Error>;
