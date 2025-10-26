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

        # Haskell GHC 9.10.2
        haskellPkgs = pkgs.haskell.packages.ghc9102;

        # Nightly Rust toolchain from Nix
        rustToolchain = pkgs.rust-bin.nightly."2025-10-01".default.override {
          extensions = [ "rust-src" "rustfmt" "clippy" ];
          targets = [ "x86_64-unknown-linux-gnu" ];
        };

        buildInputs = with pkgs; [
          cmake ninja gnumake pkg-config git
          llvmPkgs.llvm llvmPkgs.mlir llvmPkgs.clang llvmPkgs.libclang
          llvmPkgs.tblgen llvmPkgs.lldb
          python313Packages.lit python313Packages.filecheck
          haskellPkgs.ghc cabal-install haskell-language-server
          rustToolchain
        ];

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

          # Add toolchains to PATH (Rust and MLIR first)
          export PATH="${rustToolchain}/bin:${llvmPkgs.llvm}/bin:${llvmPkgs.mlir}/bin:${llvmPkgs.tblgen}/bin:${pkgs.python313Packages.lit}/bin:${pkgs.python313Packages.filecheck}/bin:$PATH"

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
          stdenv = llvmPkgs.stdenv; # ensures libc/include correctness
          packages = buildInputs;
          shellHook = shellHook;
        };
      }
    );
}
