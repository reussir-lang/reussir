# Reussir

Reussir is a programming language compiler built on LLVM and MLIR, featuring a Rust-like syntax with advanced memory management capabilities.

## Table of Contents

- [Installation](#installation)
- [Dependencies](#dependencies)
- [Building](#building)
- [Testing](#testing)
- [Usage](#usage)
- [Project Structure](#project-structure)

## Installation

### System Dependencies

Reussir requires the following system dependencies:

- **LLVM 20+** and **MLIR** (part of LLVM)
- **CMake 3.31+**
- **Rust toolchain** (latest stable)
- **C++ compiler** with C++23 support
- **Python 3** (for test infrastructure)

### Installing Dependencies

#### Ubuntu/Debian
```bash
# Install LLVM and MLIR (version 20+)
sudo apt update
sudo apt install llvm-20-dev mlir-20-dev

# Install CMake and build tools
sudo apt install cmake build-essential

# Install Rust
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
source ~/.cargo/env

# Install Python and lit (for testing)
sudo apt install python3 python3-pip
pip3 install lit
```

#### macOS
```bash
# Install LLVM and MLIR via Homebrew
brew install llvm@20

# Install CMake
brew install cmake

# Install Rust
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
source ~/.cargo/env

# Install Python and lit
brew install python3
pip3 install lit
```

#### Arch Linux
```bash
# Install LLVM and MLIR
sudo pacman -S llvm mlir

# Install CMake and build tools
sudo pacman -S cmake base-devel

# Install Rust
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
source ~/.cargo/env

# Install Python and lit
sudo pacman -S python python-pip
pip install lit
```

## Building

### CMake Build (C++ Components)

1. **Configure the build:**
```bash
mkdir build && cd build
cmake .. -DCMAKE_BUILD_TYPE=Release
```

2. **Build the project:**
```bash
cmake --build . --parallel
```

**Alternative with Ninja (faster builds):**
```bash
# Configure with Ninja generator
cmake .. -G Ninja -DCMAKE_BUILD_TYPE=Release
# Build with Ninja
ninja
```

3. **Install (optional):**
```bash
cmake --install .
```

### Cargo Build (Rust Components)

The Rust components can be built independently:

```bash
# Build all Rust crates
cargo build --release
```

### Combined Build

The project uses both CMake and Cargo. The recommended approach is to use CMake which will handle both:

```bash
mkdir build && cd build
cmake .. -G Ninja -DCMAKE_BUILD_TYPE=Release
ninja
```

This will build:
- `reussir-opt`: MLIR optimization tool
- `reussir-rt`: Runtime library
- All Rust crates and dependencies

## Testing

### Running Tests

The project includes comprehensive test suites:

#### Integration Tests (MLIR/LLVM)
```bash
# From the build directory
ninja check

# Or directly with lit
./tests/integration/lit tests/integration/ -v
```

#### Unit Tests (C++)
```bash
# From the build directory
ctest --verbose
```

#### Rust Tests
```bash
# Run all Rust tests
cargo test

# Run tests for specific crate
cargo test -p reussir-core
cargo test -p reussir-bridge
```

### Test Structure

- **Integration Tests**: Located in `tests/integration/`
  - `basic/success/`: Tests that should pass
  - `basic/failure/`: Tests that should fail with specific errors
  - `conversion/`: MLIR conversion tests
  - `compilation/`: End-to-end compilation tests

- **Unit Tests**: Located in `tests/unittest/`
  - C++ unit tests for core functionality

- **Rust Tests**: Embedded in each crate's source files

## Usage

## Project Structure

```
reussir/
├── runtime/               # Runtime library
├── include/               # C++ headers
├── lib/                   # C++ implementation
├── tool/                  # Compiler tools
│   ├── reussir-opt/       # MLIR optimization tool
├── tests/                 # Test suites
│   ├── integration/       # Integration tests
│   └── unittest/          # Unit tests
└── docs/                  # Documentation
```

## Development

### Building for Development

```bash
# Debug build with all warnings
mkdir build-debug && cd build-debug
cmake .. -G Ninja -DCMAKE_BUILD_TYPE=Debug -DREUSSIR_ENABLE_PEDANTIC=ON
ninja

# Rust development
cargo build
```

### Running Specific Tests

```bash
# Run specific integration test
./tests/integration/lit tests/integration/basic/success/record.mlir -v

# Run specific Rust test
cargo test -p reussir-core test_name

# Run C++ unit tests
cd build && ctest -R test_name
```

## Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Run the test suite: `ninja check`
5. Submit a pull request

## License

This project is licensed under either of

- Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
- MIT License ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)

at your option.
