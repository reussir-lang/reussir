//! Links the rcgmp integer runtime into JIT hosts and generates the table of
//! its entry points, so that JIT'd code calling the runtime resolves against
//! the linked-in copy (see `OrcJit::define_runtime_symbols`).
//!
//! CMake lists the runtime archive and the GMP library, one path per line, in
//! `reussir-jit-rcgmp-runtime.txt` beside the C API archives; the file is empty
//! when the `rcgmp` feature is disabled. The entry points are the `rcgmp_*`
//! symbols in the archive's own symbol index, so the table follows the runtime
//! without a hand-maintained list.

use std::collections::BTreeSet;
use std::env;
use std::fmt::Write;
use std::fs;
use std::path::{Path, PathBuf};

use object::read::archive::ArchiveFile;

fn main() {
    println!("cargo:rerun-if-env-changed=REUSSIR_CAPI_LIB_DIR");
    let lib_dir = env::var_os("REUSSIR_CAPI_LIB_DIR")
        .map(PathBuf::from)
        .unwrap_or_else(|| {
            PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap()).join("../../build/lib")
        });
    let manifest = lib_dir.join("reussir-jit-rcgmp-runtime.txt");
    println!("cargo:rerun-if-changed={}", manifest.display());
    let listing = fs::read_to_string(&manifest)
        .expect("failed to read the rcgmp runtime manifest; rerun CMake configuration");

    let mut lines = listing.lines();
    let entry_points = match (lines.next(), lines.next()) {
        (Some(archive), Some(gmp)) => {
            let (archive, gmp) = (Path::new(archive), Path::new(gmp));
            println!("cargo:rerun-if-changed={}", archive.display());
            println!(
                "cargo:rustc-link-search=native={}",
                parent(archive).display()
            );
            println!("cargo:rustc-link-lib=static=RCGMPRuntime");
            println!("cargo:rustc-link-search=native={}", parent(gmp).display());
            println!("cargo:rustc-link-lib=dylib=gmp");
            entry_points(archive)
        }
        _ => BTreeSet::new(),
    };

    let mut table = String::from("unsafe extern \"C\" {\n");
    for name in &entry_points {
        writeln!(table, "    fn {name}();").unwrap();
    }
    table.push_str("}\n\n/// The rcgmp runtime's entry points, by C symbol name.\n");
    table.push_str(
        "pub(crate) static RCGMP_RUNTIME_SYMBOLS: &[(&str, unsafe extern \"C\" fn())] = &[\n",
    );
    for name in &entry_points {
        writeln!(table, "    (\"{name}\", {name}),").unwrap();
    }
    table.push_str("];\n");
    let out = PathBuf::from(env::var("OUT_DIR").unwrap()).join("rcgmp_runtime_symbols.rs");
    fs::write(out, table).expect("failed to write the rcgmp runtime symbol table");
}

fn parent(path: &Path) -> &Path {
    path.parent()
        .unwrap_or_else(|| panic!("`{}` has no parent directory", path.display()))
}

/// The `rcgmp_*` symbols the archive defines. Mach-O spells C symbols with a
/// leading underscore.
fn entry_points(archive: &Path) -> BTreeSet<String> {
    let data =
        fs::read(archive).unwrap_or_else(|e| panic!("failed to read `{}`: {e}", archive.display()));
    let file = ArchiveFile::parse(&*data)
        .unwrap_or_else(|e| panic!("failed to parse `{}`: {e}", archive.display()));
    let symbols = file
        .symbols()
        .ok()
        .flatten()
        .unwrap_or_else(|| panic!("`{}` has no symbol index", archive.display()));
    symbols
        .filter_map(Result::ok)
        .filter_map(|symbol| {
            let name = std::str::from_utf8(symbol.name()).ok()?;
            let name = name
                .strip_prefix('_')
                .filter(|n| n.starts_with("rcgmp_"))
                .unwrap_or(name);
            name.starts_with("rcgmp_").then(|| name.to_owned())
        })
        .collect()
}
