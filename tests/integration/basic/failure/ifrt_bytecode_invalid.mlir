// RUN: %reussir-opt %s --split-input-file --verify-diagnostics

// expected-error @+1 {{requires MLIR bytecode}}
reussir.ifrt.bytecode @text bytecode "module {}" checksum ""

// -----
// expected-error @+1 {{requires a 64-character hexadecimal BLAKE3 checksum}}
reussir.ifrt.bytecode @short bytecode "ML\EFR" checksum "01"

// -----
// expected-error @+1 {{requires a 64-character hexadecimal BLAKE3 checksum}}
reussir.ifrt.bytecode @nonhex bytecode "ML\EFR" checksum "g000000000000000000000000000000000000000000000000000000000000000"

// -----
// expected-error @+1 {{BLAKE3 checksum does not match bytecode}}
reussir.ifrt.bytecode @corrupt bytecode "ML\EFR" checksum "0000000000000000000000000000000000000000000000000000000000000000"
