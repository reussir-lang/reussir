//! Raw, target-generated bindings to OpenXLA's PjRt plugin C API.
//!
//! No XLA library is linked. The caller loads `GetPjrtApi`, checks the table's
//! version/size, and manages handles according to `vendor/pjrt_c_api.h`.

#![allow(non_camel_case_types, non_snake_case, non_upper_case_globals)]

include!(concat!(env!("OUT_DIR"), "/pjrt.rs"));
