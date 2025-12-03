{
  description = "Reussir — A language with region-based memory management (LLVM/MLIR + Rust + Haskell)";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    rust-overlay.url = "github:oxalica/rust-overlay";
  };

  outputs = { self, nixpkgs, flake-utils, rust-overlay }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [ (import rust-overlay) ];
        pkgs = import nixpkgs {
          inherit system overlays;
        };

        # LLVM 21 + MLIR
        llvmPkgs = pkgs.llvmPackages_21;

        # Haskell GHC 9.10.3
        haskellPkgs = pkgs.haskell.packages.ghc9103;

        # Python 3.13
        pythonPkgs = pkgs.python313Packages;

        # Nightly Rust toolchain from Nix
        rustToolchain = pkgs.rust-bin.nightly."2025-11-07".default.override {
          extensions = [ "rust-src" "rustfmt" "clippy" "rustc-dev" "llvm-tools-preview" ];
        };

        buildInputs = with pkgs; [
          # General dependencies
          cmake ninja gnumake pkg-config git libffi spdlog gtest
          # LLVM dependencies
          llvmPkgs.llvm llvmPkgs.mlir llvmPkgs.clang llvmPkgs.libclang
          llvmPkgs.tblgen llvmPkgs.lldb
          # Python dependencies
          pythonPkgs.lit pythonPkgs.filecheck
          # Haskell dependencies
          haskellPkgs.ghc haskellPkgs.cabal-install haskellPkgs.haskell-language-server
          # Rust dependencies
          rustToolchain
          # Documentation dependencies
          typst
        ];

        # Wrapped clang-scan-deps with proper libc++ include paths
        # The issue is that clang-scan-deps doesn't inherit the wrapper's include paths
        # We inject the libc++ includes by appending to the compiler args after --
        wrappedClangScanDeps = pkgs.writeShellScriptBin "clang-scan-deps" ''
          # Append libc++ include paths to compiler args (after the -- separator)
          exec ${llvmPkgs.clang-unwrapped}/bin/clang-scan-deps \
            "$@" \
            -stdlib=libc++ \
            -isystem ${llvmPkgs.libcxx.dev}/include/c++/v1
        '';

        # Clean environment, explicit tool setup
        shellHook = ''
          export CC=${llvmPkgs.clang}/bin/clang
          export CXX=${llvmPkgs.clang}/bin/clang++

          # LLVM + MLIR CMake locations
          export LLVM_DIR=${llvmPkgs.llvm.dev}/lib/cmake/llvm
          export MLIR_DIR=${llvmPkgs.mlir.dev}/lib/cmake/mlir
          export CMAKE_PREFIX_PATH=${llvmPkgs.llvm.dev}/lib/cmake:${llvmPkgs.mlir.dev}/lib/cmake:${llvmPkgs.libclang.dev}/lib/cmake

          # TableGen override (ensures CMake finds the correct binary)
          export MLIR_TABLEGEN_EXE_OVERRIDE=${llvmPkgs.tblgen}/bin/mlir-tblgen

          # Point CMake to our wrapped clang-scan-deps
          export CMAKE_CXX_COMPILER_CLANG_SCAN_DEPS=${wrappedClangScanDeps}/bin/clang-scan-deps

          # Add toolchains to PATH (Rust and MLIR first, with wrapped clang-scan-deps)
          export PATH="${wrappedClangScanDeps}/bin:${rustToolchain}/bin:${llvmPkgs.llvm}/bin:${llvmPkgs.mlir}/bin:${llvmPkgs.tblgen}/bin:${pkgs.python313Packages.lit}/bin:${pkgs.python313Packages.filecheck}/bin:$PATH"
          echo "=== Reussir development environment loaded ==="
          echo "LLVM version: ${llvmPkgs.llvm.version}"
          echo "MLIR TableGen: $MLIR_TABLEGEN_EXE_OVERRIDE"
          echo "GHC version: $(${haskellPkgs.ghc}/bin/ghc --numeric-version)"
          echo "Rust: $(rustc --version)"
          echo "Cargo: $(cargo --version)"
          echo ""
          echo "Build instructions:"
          echo "  mkdir -p build && cd build"
          echo "  cmake -GNinja -DCMAKE_BUILD_TYPE=Release .."
          echo "  ninja"
          echo "=============================================="
        '';
      in
      {
        devShells.default = pkgs.mkShell {
          name = "reussir-dev";
          stdenv = llvmPkgs.libcxxStdenv; # ensures libc/include correctness
          packages = buildInputs;
          shellHook = shellHook;
        };
      }
    );
}
