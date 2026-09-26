load("@rules_cc//cc:cc_library.bzl", "cc_library")
load("@rules_shell//shell:sh_binary.bzl", "sh_binary")
load(":tblgen.bzl", "td_library")

package(default_visibility = ["//visibility:public"])

# Reuse CMake's MLIR headers and TableGen; never compile a second MLIR.
cc_library(
    name = "headers",
    hdrs = glob(["include*/**/*.h", "include*/**/*.inc"]),
    includes = @INCLUDES@,
    deps = ["//llvm:headers"],
)

td_library(
    name = "all_td",
    srcs = glob(["include*/**/*.td"]),
    includes = @INCLUDES@,
)

sh_binary(name = "mlir-tblgen", srcs = ["mlir-tblgen.sh"])

[alias(name = name, actual = ":headers") for name in [
    "AffineDialect",
    "AllPassesAndDialects",
    "Analysis",
    "ArithDialect",
    "ArithUtils",
    "AsmParser",
    "BufferizationDialect",
    "BufferizationInterfaces",
    "BufferizationTransforms",
    "BytecodeOpInterface",
    "BytecodeWriter",
    "CallOpInterfaces",
    "ComplexDialect",
    "ControlFlowDialect",
    "ControlFlowInterfaces",
    "Dialect",
    "DialectUtils",
    "FuncDialect",
    "FuncExtensions",
    "FuncTransforms",
    "FunctionInterfaces",
    "IR",
    "InferTypeOpInterface",
    "InliningUtils",
    "LinalgDialect",
    "LinalgTransforms",
    "LinalgUtils",
    "MLProgramDialect",
    "MathDialect",
    "MemRefDialect",
    "Parser",
    "Pass",
    "QuantOps",
    "ReconcileUnrealizedCasts",
    "Rewrite",
    "SCFDialect",
    "ShapeDialect",
    "ShapeTransforms",
    "SideEffectInterfaces",
    "SparseTensorDialect",
    "SparseTensorEnums",
    "Support",
    "TableGen",
    "TensorDialect",
    "TensorUtils",
    "TransformUtils",
    "Transforms",
    "TranslateLib",
    "UBDialect",
]]

[alias(name = name, actual = ":all_td") for name in [
    "AttrTdFiles",
    "BuiltinDialectBytecodeTdFiles",
    "BuiltinDialectTdFiles",
    "BytecodeOpInterfaceTdFiles",
    "ControlFlowInterfacesTdFiles",
    "FuncTdFiles",
    "InferTypeOpInterfaceTdFiles",
    "LoopLikeInterfaceTdFiles",
    "MemRefOpsTdFiles",
    "OpBaseTdFiles",
    "PassBaseTdFiles",
    "QuantizationOpsTdFiles",
    "ShapeOpsTdFiles",
    "SideEffectInterfacesTdFiles",
    "TensorOpsTdFiles",
    "ViewLikeInterfaceTdFiles",
]]
