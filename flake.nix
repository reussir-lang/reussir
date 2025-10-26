{
  description = "Reussir - A language with region-based memory management";

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
        
        # LLVM 21 with MLIR
        llvmPackages = pkgs.llvmPackages_21;
        
        # Rust toolchain
        rustToolchain = pkgs.rust-bin.stable.latest.default.override {
          extensions = [ "rust-src" "rustfmt" "clippy" ];
          targets = [ "x86_64-unknown-linux-gnu" ];
        };
        
        # Haskell GHC 9.10.2
        haskell = pkgs.haskell.packages.ghc9102;
        
        # Build environment with all dependencies
        buildInputs = with pkgs; [
          # C/C++ build tools
          cmake
          ninja
          gnumake
          
          # LLVM and MLIR
          llvmPackages.llvm
          llvmPackages.clang
          llvmPackages.libclang
          
          # Haskell tools
          haskell.ghc
          cabal-install
          haskell-language-server
          
          # Rust tools
          rustToolchain
          cargo
          
          # Utilities
          git
          pkg-config
          
          # Runtime dependencies
          glibc
        ];
        
        shellHook = ''
          export CC=${llvmPackages.clang}/bin/clang
          export CXX=${llvmPackages.clang}/bin/clang++
          export CMAKE_PREFIX_PATH=${llvmPackages.llvm.dev}/lib/cmake:${llvmPackages.libclang.dev}/lib/cmake
          export LLVM_DIR=${llvmPackages.llvm.dev}/lib/cmake/llvm
          export MLIR_DIR=${llvmPackages.llvm.dev}/lib/cmake/mlir
          
          # Rust setup
          export CARGO_HOME="$HOME/.cargo"
          export PATH="$CARGO_HOME/bin:$PATH"
          
          # Add LLVM to path
          export PATH="${llvmPackages.bintools.bintools_bin}/bin:$PATH"
          
          echo "Reussir development environment loaded!"
          echo "LLVM: ${llvmPackages.llvm.version}"
          echo "GHC: ${haskell.ghc.version}"
          echo "Rust: $(rustc --version 2>/dev/null || echo 'See rustup docs')"
          echo ""
          echo "Build commands:"
          echo "  mkdir -p build && cd build"
          echo "  cmake -GNinja -DCMAKE_BUILD_TYPE=Release .."
          echo "  ninja"
        '';
      in
      {
        devShells.default = pkgs.mkShell {
          inherit buildInputs shellHook;
          
          name = "reussir-dev";
          
          packages = buildInputs;
        };
        
        # Allow building with nix build
        packages.default = pkgs.stdenv.mkDerivation {
          name = "reussir";
          src = self;
          
          nativeBuildInputs = buildInputs;
          
          buildPhase = ''
            mkdir -p build
            cd build
            cmake -GNinja -DCMAKE_BUILD_TYPE=Release ..
            ninja
          '';
          
          installPhase = ''
            mkdir -p $out/bin
            cp bin/* $out/bin/
          '';
        };
      }
    );
}
