// UNSUPPORTED: ifrt
// RUN: %reussir-opt %s --reussir-ifrt-just-in-time-transform --verify-diagnostics
// expected-error @+1 {{IFRT JIT transformation requires an OpenXLA-enabled build}}
module {}
