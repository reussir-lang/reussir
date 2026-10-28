//! Source bundles shipped inside rene. Core is implicit; std is opt-in through
//! `dependencies.std = { version = "^0.1" }`. Both compile as ordinary graph
//! dependencies and share their baked artifacts through the disk cache.

use std::path::{Path, PathBuf};

use crate::Error;

pub struct Bundle {
    pub name: &'static str,
    bytes: &'static [u8],
}

pub static CORE: Bundle = Bundle {
    name: "core",
    bytes: include_bytes!(concat!(env!("OUT_DIR"), "/core-src.tar.zst")),
};
pub static STD: Bundle = Bundle {
    name: "std",
    bytes: include_bytes!(concat!(env!("OUT_DIR"), "/std-src.tar.zst")),
};

pub fn find(name: &str) -> Option<&'static Bundle> {
    match name {
        "core" => Some(&CORE),
        "std" => Some(&STD),
        _ => None,
    }
}

impl Bundle {
    pub fn content_hash(&self) -> String {
        blake3::hash(self.bytes).to_hex().to_string()
    }

    pub fn dir(&self, build_dir: &Path) -> PathBuf {
        build_dir
            .join(format!("{}-src", self.name))
            .join(&self.content_hash()[..16])
    }

    /// The build-directory lock serializes unpacking. An existing manifest is
    /// the completion marker, written last so interrupted extraction retries.
    pub fn unpack(&self, build_dir: &Path) -> Result<PathBuf, Error> {
        let root = self.dir(build_dir);
        if root.join("rene.ncl").is_file() {
            return Ok(root);
        }
        std::fs::create_dir_all(&root).map_err(|source| Error::file("create bundled source directory", &root, source))?;
        let decoder = zstd::Decoder::new(self.bytes)?;
        let mut manifest = None;
        for entry in tar::Archive::new(decoder)
            .entries()
            ?
        {
            let mut entry = entry?;
            if entry.path()?.as_ref() == Path::new("rene.ncl") {
                let mut bytes = Vec::new();
                std::io::Read::read_to_end(&mut entry, &mut bytes)?;
                manifest = Some(bytes);
            } else {
                entry.unpack_in(&root).map_err(|source| Error::file("unpack bundled source into", &root, source))?;
            }
        }
        let staged_manifest = root.join("rene.ncl.tmp");
        std::fs::write(&staged_manifest, manifest.ok_or(Error::MissingBundleManifest { package: self.name })?)
            .map_err(|source| Error::file("write bundled manifest", &staged_manifest, source))?;
        std::fs::rename(&staged_manifest, root.join("rene.ncl"))
            .map_err(|source| Error::file("publish bundled manifest in", &root, source))?;
        Ok(root)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn bundles_contain_only_their_manifest_and_sources() {
        let tmp = tempfile::tempdir().unwrap();
        for bundle in [&CORE, &STD] {
            let decoder = zstd::Decoder::new(bundle.bytes).unwrap();
            for entry in tar::Archive::new(decoder).entries().unwrap() {
                let entry = entry.unwrap();
                let path = entry.path().unwrap();
                assert!(
                    path == Path::new("rene.ncl") || path.starts_with("src"),
                    "{path:?}"
                );
            }
            let root = bundle.unpack(tmp.path()).unwrap();
            assert_eq!(
                crate::manifest::load(&root.join("rene.ncl"))
                    .unwrap()
                    .manifest
                    .package
                    .name,
                bundle.name
            );
            assert!(root.join("src/lib.rr").is_file());
            assert_eq!(bundle.unpack(tmp.path()).unwrap(), root);
        }
    }
}
