<p align="center">
  <img src="misc/logos/reussir_banner_1600x800.svg" alt="Reussir Logo" width="600">
</p>

<h1 align="center">Reussir</h1>

<p align="center">
  <strong>A programming language with region-based memory management</strong>
</p>

<p align="center">
  <em>Built on LLVM/MLIR + Rust + Haskell</em>
</p>

<p align="center">
  <a href="#quickstart">Quickstart</a> •
  <a href="#building">Building</a> •
  <a href="#testing">Testing</a> •
  <a href="#project-structure">Project Structure</a> •
  <a href="#contributing">Contributing</a>
</p>

---

## Overview

Reussir is a programming language compiler featuring Rust-like syntax with advanced region-based memory management. The compiler is implemented as a multi-language project:

- **MLIR Backend** — C++ with LLVM 21/22 and MLIR for IR transformations and code generation
- **Runtime** — Rust for the runtime library with reference counting and region management
- **Frontend** — Haskell for parsing, type checking, and MLIR code generation

## Quickstart

### Ubuntu 24.04

```bash
# LLVM 21
wget https://apt.llvm.org/llvm.sh && chmod +x llvm.sh
sudo ./llvm.sh 21
sudo apt install libmlir-21-dev mlir-21-tools llvm-21-dev

# Build tools
sudo apt install cmake ninja-build libgtest-dev libspdlog-dev
pip install lit

# Rust
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
rustup default nightly

# Haskell (via GHCup)
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
ghcup install ghc 9.14.1
ghcup install cabal latest
```

> [!NOTE]
> For GHC 9.14.1, in case GHCup does not have the latest HLS support, you can manually install HLS 2.13.0.0:
> ```bash
> ghcup install hls 2.13.0.0 -u https://github.com/haskell/haskell-language-server/releases/download/2.13.0.0/haskell-language-server-2.13.0.0-aarch64-linux-ubuntu2204.tar.xz
> ```

**Sample VS Code settings for Ubuntu (`.vscode/settings.json`):**

```json
{
    "cmake.configureOnOpen": false,
    "cmake.generator": "Ninja",
    "cmake.configureSettings": {
        "TPDE_INCLUDE_TESTS": false,
        "MLIR_DIR": "/usr/lib/llvm-21/lib/cmake/mlir",
        "LLVM_DIR": "/usr/lib/llvm-21/lib/cmake/llvm",
        "CMAKE_CXX_COMPILER": "clang++-21",
        "CMAKE_C_COMPILER": "clang-21",
        "LLVM_USE_LINKER": "lld"
    },
    "editor.rulers": [
        80
    ],
    "files.autoSave": "afterDelay",
    "files.insertFinalNewline": true,
    "editor.formatOnSave": true,
    "editor.wordWrap": "off",
    "haskell.manageHLS": "GHCup",
    "clangd.path": "clangd-21",
    "haskell.toolchain": {
        "ghc": "9.14.1",
        "hls": "2.13.0.0",
        "cabal": "3.16.1.0"
    }
}
```

## Building

### Full Build (C++ Backend + Rust Runtime)

```bash
# From project root
mkdir -p build && cd build
cmake -GNinja -DCMAKE_BUILD_TYPE=Release ..
ninja
```

This builds:
- `reussir-opt` — MLIR optimization and transformation tool
- `reussir-translate` — MLIR to LLVM IR translation tool
- `reussir-rt` — Rust runtime library

### Haskell Frontend

The Haskell frontend consists of four packages managed by Cabal:

| Package | Description |
|---------|-------------|
| `reussir-parser` | Lexer and parser for Reussir syntax |
| `reussir-codegen` | MLIR code generation from AST |
| `reussir-bridge` | FFI bridge to the C++ MLIR infrastructure |
| `reussir-repl` | Interactive REPL for experimentation |

**Build all Haskell packages:**

```bash
cabal update
cabal build all -j
```

**Run the REPL:**

```bash
cabal run reussir-repl
```

### Development Build

For development with debug symbols and extra warnings:

```bash
cmake -GNinja \
  -DCMAKE_BUILD_TYPE=Debug \
  -DREUSSIR_ENABLE_PEDANTIC=ON \
  -DREUSSIR_ENABLE_TESTS=ON \
  ..
ninja
```

## Testing

### C++ / MLIR Tests

```bash
# Unit tests
cmake --build build --target reussir-ut
ctest --test-dir build --output-on-failure

# Integration tests (lit-based)
cmake --build build --target check
```

### Haskell Tests

```bash
cabal test all -j
```

### Rust Runtime Tests

```bash
cd runtime
cargo test

# Run with Miri for undefined behavior detection
cargo miri test
```

## Project Structure

```
reussir/
├── frontend/                 # Haskell frontend
│   ├── reussir-bridge/       # C++/MLIR FFI bridge
│   ├── reussir-codegen/      # MLIR code generation
│   ├── reussir-core/         # Core type checking and elaboration
│   ├── reussir-diagnostic/   # Error reporting and diagnostics
│   ├── reussir-parser/       # Parser and lexer
│   └── reussir-repl/         # Interactive REPL
├── include/                  # C++ headers
│   └── Reussir/
│       ├── Analysis/         # Analysis passes
│       ├── Conversion/       # Conversion passes
│       ├── IR/               # MLIR dialect definitions
│       └── Support/          # Support utilities
├── lib/                      # C++ implementation
│   ├── Analysis/             # Analysis pass implementations
│   ├── Bridge/               # JIT engine and bridges
│   ├── Conversion/           # Pass implementations
│   ├── IR/                   # Dialect implementation
│   └── RustCompiler/         # Rust compilation support
├── runtime/                  # Rust runtime library
│   └── src/
│       ├── alloc.rs          # Memory allocation
│       ├── collections/      # Runtime collections
│       ├── nullable.rs       # Nullable type support
│       ├── rc.rs             # Reference counting
│       └── region/           # Region-based allocation
├── tool/                     # Compiler tools
│   ├── reussir-opt/          # MLIR optimizer
│   └── reussir-translate/    # MLIR translator
├── tests/
│   ├── integration/          # lit-based integration tests
│   └── unittest/             # C++ unit tests
├── www/                      # Documentation source
└── cabal.project             # Cabal project file
```

## Dependencies

| Dependency | Version | Notes |
|------------|---------|-------|
| LLVM + MLIR | 21+ | Core compiler infrastructure |
| CMake | 3.31+ | Build system |
| Ninja | any | Build tool |
| Rust | nightly | Runtime library |
| GHC | 9.14.1 | Haskell frontend |
| Cabal | 3.16+ | Haskell build tool |
| Python | 3.11+ | Test infrastructure (lit) |

## CI/CD

The project runs automated tests on every push:

| Workflow | Platform | Description |
|----------|----------|-------------|
| MLIR Backend | Ubuntu 24.04 (x64/ARM) | LLVM 21 & 22 builds |
| MinGW Clang64 | Windows 2025 | Full pipeline with MSYS2 |
| Haskell Frontend | Ubuntu 24.04 ARM | Cabal build & test |
| Miri Tests | Ubuntu Latest | Runtime UB detection |
| MacOS | MacOS 14 | Full pipeline with Homebrew vendored dependencies |

## Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Ensure all tests pass:
   ```bash
   cmake --build build --target check  # Integration tests
   ctest --test-dir build              # Unit tests
   cabal test all -j                   # Haskell tests
   ```
5. Submit a pull request

## License

The Reussir Project is dual-licensed under:

- **Apache License, Version 2.0** (with optional LLVM exceptions)
- **MIT License**

Choose whichever license works best for your use case. See [LICENSE](LICENSE) for details.
