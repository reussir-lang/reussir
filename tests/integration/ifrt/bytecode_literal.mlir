// RUN: %reussir-opt %s --reussir-convert-to-llvm | %reussir-translate --mlir-to-llvmir | %FileCheck %s
// CHECK: internal constant { [64 x i8], [{{[0-9]+}} x i8] }
// CHECK-SAME: [64 x i8] c"
// CHECK-SAME: c"ML

reussir.ifrt.bytecode "private" @_RNvC21REUSSIR_IFRT_BYTECODE43EjvIcO7AriOMd92k7hudgRQXbeHtmsNUQvN4I45YfVB bytecode "ML\EFR\0DMLIR23.1.0\00\01\17\05\01\05\07\01\03\0B\03\05\07\0F\03\1B\0D\03\01\0D\0B\13\0B\0B\13\13\01\03\0F\02+\05\09\17\01%\07\0D\01\05\0B\17\01'\0B\17\01)\0F\05\01\01\041\05\01P\03\01\07\04!\03\01\05\03P\09\03\07\04\0D\03\01\05\05\00\0B\06\03\01\05\01\00\AB\0D\0B]\0F\0F\0B\11builtin\00func\00module\00return\00tests/integration/ifrt/bytecode_lowering.mlir\00main\00\08\17\05\05\01\01\0D\01\05\01\01\07\01" checksum "1bbc474471a902aaa5013de1cba4ead6df904b127dbb85fc1fa5fd72af4dd706"
