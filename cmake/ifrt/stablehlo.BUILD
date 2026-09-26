load("@rules_cc//cc:cc_library.bzl", "cc_library")
load("@llvm-project//mlir:tblgen.bzl", "td_library")

package(default_visibility = ["//visibility:public"])

# CMake supplies StableHLO implementations at the final link.
cc_library(
    name = "headers",
    hdrs = glob(["src/stablehlo/**/*.h", "gen/stablehlo/**/*.inc", "gen/stablehlo/**/*.h"]),
    includes = ["src", "gen"],
    deps = ["@llvm-project//mlir:headers"],
)

td_library(
    name = "all_td",
    srcs = glob(["src/stablehlo/**/*.td"]),
    includes = ["src"],
    deps = ["@llvm-project//mlir:all_td"],
)

[alias(name = name, actual = ":headers") for name in [
    "base",
    "chlo_ops",
    "register",
    "replica_group_utils",
    "stablehlo_assembly_format",
    "stablehlo_ops",
    "stablehlo_ops_inc_gen",
    "stablehlo_pass_utils",
    "stablehlo_passes",
    "stablehlo_passes_optimization",
    "stablehlo_portable_api",
    "stablehlo_serialization",
    "stablehlo_type_inference",
    "version",
]]

[alias(name = name, actual = ":all_td") for name in [
    "base_td_files",
    "chlo_ops_td_files",
    "stablehlo_ops_td_filegroup",
]]
