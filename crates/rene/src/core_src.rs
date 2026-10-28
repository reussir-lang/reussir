//! Compatibility helpers for the implicit core source bundle.

use crate::Error;
use std::path::{Path, PathBuf};

pub fn dir(build_dir: &Path) -> PathBuf {
    crate::bundled::CORE.dir(build_dir)
}

pub fn unpack(build_dir: &Path) -> Result<PathBuf, Error> {
    crate::bundled::CORE.unpack(build_dir)
}
