# Reussir Development Environment (Nix)

This project includes a Nix flake that provides a complete development environment with all necessary dependencies.

## Prerequisites

### Install Nix (if not already installed)

**On NixOS:**
- Nix is already included in your system

**On other Linux distributions or macOS:**
```bash
sh <(curl -L https://nixos.org/nix/install)
```

### Enable Flakes (if not already enabled)

Add to your `~/.config/nix/nix.conf`:
```
experimental-features = nix-command flakes
```

Or temporarily enable for one session:
```bash
export NIX_CONFIG="experimental-features = nix-command flakes"
```

## What's Included

The flake provides:
- **LLVM 21** with MLIR support
- **Haskell GHC 9.10.2** with cabal-install
- **Rust** stable toolchain with rustfmt, clippy, and rust-src
- **CMake** and Ninja for building
- All necessary C/C++ build tools

## Usage

### Enter Development Shell

To enter the development environment with all dependencies:

```bash
nix develop
```

This will:
1. Download and set up all dependencies
2. Configure environment variables (LLVM_DIR, MLIR_DIR, etc.)
3. Provide a shell with all tools available

### Build the Project

Inside the Nix shell:

```bash
mkdir -p build
cd build
cmake -GNinja -DCMAKE_BUILD_TYPE=Release ..
ninja
```

### Run Tests

After building:

```bash
ninja check
```

### Quick Commands

**Enter the development shell:**
```bash
nix develop
```

**Enter the shell and immediately build:**
```bash
nix develop --command bash -c 'cd build && ninja'
```

**Build with Nix (not recommended, slower than manual build):**
```bash
nix build
```

This will build the project entirely within Nix sandbox.

## Environment Variables

The following environment variables are automatically set in the Nix shell:

- `CC` and `CXX`: Point to clang from LLVM 21
- `CMAKE_PREFIX_PATH`: LLVM and MLIR installation paths
- `LLVM_DIR`: LLVM CMake configuration directory
- `MLIR_DIR`: MLIR CMake configuration directory
- `CARGO_HOME`: Rust cargo home directory
- `PATH`: All tool binaries are in PATH

## Troubleshooting

### "Experimental features not enabled"
Make sure you've enabled flakes in your Nix configuration (see Prerequisites).

### "llvmPackages_21 not found"
You may need to update nixpkgs. Try:
```bash
nix flake update
```

### CMake can't find LLVM/MLIR
If CMake has issues finding LLVM or MLIR, the environment variables should be set automatically in the shell. Make sure you're running CMake inside the `nix develop` shell.

### Building takes a long time the first time
The first time you run `nix develop`, it needs to download and build all dependencies. This can take 30-60 minutes depending on your system. Subsequent runs will be much faster as dependencies are cached.

## Alternative: Direnv

If you use direnv, you can automatically enter the Nix shell when you `cd` into the project directory:

1. Install direnv:
   ```bash
   nix profile install nixpkgs#direnv
   ```

2. Add hook to your shell (add to `~/.bashrc` or `~/.zshrc`):
   ```bash
   eval "$(direnv hook bash)"  # or zsh if using zsh
   ```

3. Allow direnv for this directory:
   ```bash
   echo "use flake" > .envrc
   direnv allow
   ```

Now the Nix environment will be loaded automatically when you enter the project directory.

## Version Information

To see versions of tools in the environment:

```bash
nix develop --command bash -c 'clang --version && ghc --version && rustc --version'
```

Or inside the `nix develop` shell, just run the commands directly.
