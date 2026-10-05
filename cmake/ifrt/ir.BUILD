load("@rules_cc//cc:cc_static_library.bzl", "cc_static_library")

cc_library(
    name = "reussir_registration",
    srcs = ["reussir_registration.cc"],
    # Make workspace headers available to angle-bracket includes as well.
    copts = ["-I.", "-I$(BINDIR)"],
    deps = [
        ":ir",
        ":vifrt",
        "//xla/python/ifrt/ir/transforms:passes",
        "@llvm-project//mlir:IR",
        "@shardy//shardy/dialect/sdy/ir:dialect",
    ],
)

cc_static_library(
    name = "reussir_ifrt",
    deps = [":reussir_registration", ":reussir_translate", ":reussir_array_target"],
)

cc_library(
    name = "reussir_array_target",
    srcs = ["reussir_array_target.cc"],
    hdrs = ["reussir_target.h"],
    copts = ["-I.", "-I$(BINDIR)"],
    deps = [
        ":ir",
        "//xla:shape_util",
        "//xla/pjrt:layout_mode",
        "//xla/pjrt:pjrt_executable",
        "//xla/pjrt/proto:compile_options_proto_cc",
        "//xla/python/ifrt/ir/transforms:utils",
        "@llvm-project//mlir:IR",
    ],
)

# Only the registration archive is needed; a Bazel shared-library link would
# lack the LLVM/MLIR implementations that CMake supplies at the final link.
filegroup(
    name = "reussir_serdes_archive",
    srcs = [":ifrt_ir_program_serdes"],
    output_group = "archive",
)

# Upstream's translation driver, with an entry point for the CMake executable
# and Shardy registered for parsing outlined atom programs.
cc_library(
    name = "reussir_translate",
    srcs = ["reussir_translate.cc"],
    deps = [
        ":ir", ":vifrt", ":version", ":ifrt_ir_program",
        ":ifrt_ir_program_serdes",
        "//xla/python/ifrt:serdes",
        "//xla/python/ifrt:serdes_proto_cc",
        "@com_google_absl//absl/strings",
        "@llvm-project//mlir:headers",
        "@shardy//shardy/dialect/sdy/ir:dialect",
        "@stablehlo//:register",
        "@stablehlo//:version",
    ],
)
