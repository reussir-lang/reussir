load("@rules_cc//cc:cc_library.bzl", "cc_library")

package(default_visibility = ["//visibility:public"])

# The final CMake link supplies LLVM. This repository provides headers only.
cc_library(
    name = "headers",
    hdrs = glob(["include*/**/*.h", "include*/**/*.inc", "include*/**/*.def"]),
    includes = @INCLUDES@,
)

[alias(name = name, actual = ":headers") for name in [
    "Core",
    "Support",
    "TableGen",
    "TargetParser",
    "TransformUtils",
]]
