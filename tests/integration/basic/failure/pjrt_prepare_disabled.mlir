// UNSUPPORTED: openxla
// RUN: %reussir-opt %s --reussir-prepare-pjrt --verify-diagnostics

// Even a call-free annotated host requires IFRT preparation support.
// expected-error @+1 {{PJRT preparation requires an OpenXLA-enabled build}}
module {
  func.func @host() attributes {ifrt.function} {
    return
  }
}
