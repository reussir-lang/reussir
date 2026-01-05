// RUN: %reussir-opt %s --verify-roundtrip
module @"test" attributes {
    "reussir.test1" = #reussir.dbg_inttype< signed : true, i32, name : "i32" >,
    "reussir.test2" = #reussir.dbg_inttype< signed : false, i32, name : "u32" >,
    "reussir.test3" = #reussir.dbg_fptype<f64, name : "f64">,
    "reussir.test4" = #reussir.dbg_subprogram< raw_name : "fibonacci_logarithmic_impl", type_params : [#reussir.dbg_inttype< signed : false, i64, name : "u64" >] >,
    "reussir.test5" = #reussir.dbg_recordtype<members : [], is_variant : false,underlying_type : none, dbg_name : "EmptyRecord">
} {

}
