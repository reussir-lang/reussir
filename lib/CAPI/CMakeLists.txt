# C API library consumed by the Rust backend crates (mlir-sys / melior).
#
# mlir-sys links MLIR and LLVM statically, so the Reussir dialect registration
# and the lowering-pipeline pass factories are shipped as static archives linked
# into the same Rust binary. That keeps a single copy of the MLIR runtime and
# dialect registry: the archives' MLIR references resolve against the same static
# MLIR/LLVM that mlir-sys pulls in. Requires the distro's static MLIR/LLVM
# development packages (including libpolly-<ver>-dev for the static Polly
# archives llvm-config references).
add_library(ReussirCAPI STATIC Dialects.cpp Passes.cpp ReuseRemarks.cpp Types.cpp
                               Attrs.cpp Jit.cpp Artifact.cpp Features.cpp)

# Jit.cpp runs the custom Reussir LLVM passes and (when available) TPDE. Linking
# these as usage requirements gives ReussirCAPI their headers/defines and orders
# the build; the archives themselves are pulled onto the final Rust link line by
# reussir-backend-sys (see its build.rs).
target_link_libraries(ReussirCAPI PRIVATE ReussirLLVMAllocationSimplicationPass
                                          ReussirLLVMLinearRecurrencePass
                                          ReussirLLVMRetconDropPass)
if(REUSSIR_HAS_TPDE)
  target_compile_definitions(ReussirCAPI PRIVATE REUSSIR_HAS_TPDE)
  target_link_libraries(ReussirCAPI PRIVATE tpde_llvm)
endif()

# Reussir component archives whose symbols ReussirCAPI references: per-pass
# factories plus the standalone helpers (compilePolymorphicFFI via the
# CompilePolymorphicFFI pass + ReussirRustCompiler, gatherCompiledModules via
# MLIRReussir). reussir-backend-sys links these into the final Rust binary; here
# we only ensure they are built first. Every archive in REUSSIR_ARCHIVES of
# crates/reussir-backend-sys/build.rs must be listed here unless it is a link
# dependency of ReussirCAPI (the LLVM passes above; the sync archives, through
# MLIRReussir): otherwise building only a cargo target in a fresh build
# directory (`--target rrc`) does not build it, and cargo cannot link.
set(REUSSIR_BACKEND_ARCHIVES
  MLIRReussir
  MLIRReussirAnalysis
  ReussirRustCompiler
  # conversions
  MLIRReussirTypeConverter
  MLIRReussirBasicOpsLowering
  MLIRReussirConvertToSTD
  MLIRReussirRegionPatterns
  MLIRReussirAcquireDropExpansion
  MLIRReussirRcDecrementExpansion
  MLIRReussirTokenInstantiation
  MLIRReussirClosureOutlining
  MLIRReussirCompilePolymorphicFFI
  MLIRReussirInstrumentNonlinearFFI
  MLIRReussirAttachNativeTarget
  MLIRReussirIFRTJustInTimeTransform
  # transformations
  MLIRReussirClosureBetaReduction
  MLIRReussirDefaultInliner
  MLIRReussirSCCP
  MLIRReussirRcCreateSink
  MLIRReussirRcCreateFusion
  MLIRReussirRcDispatchFusion
  MLIRReussirInferVariantTag
  MLIRReussirIncDecCancellation
  MLIRReussirTokenReuse
  MLIRReussirSpecialPointerTag
  MLIRReussirInvariantGroupAnalysis
  MLIRReussirUniqueCarryingRecursionAnalysis
  MLIRReussirTRMCRecursionAnalysis
  MLIRReussirPjrtDonation)
add_dependencies(ReussirCAPI ${REUSSIR_BACKEND_ARCHIVES})

# Cargo does not inherit CMake's transitive link dependencies. Export the
# optional archives explicitly, one `<mode> <archive>` line each, writing an
# empty manifest when no optional component is enabled so that stale archives
# cannot be linked from a reused build directory. `whole` archives are
# whole-archived next to the Reussir components; `plain` archives are extracted
# on demand after them.
set(REUSSIR_BACKEND_OPTIONAL_ARCHIVES "")
if(REUSSIR_HAS_TPDE)
  # TPDE and its vendored spdlog and instruction encoders, referenced by Jit.cpp.
  foreach(archive tpde_llvm tpde spdlog fadec disarm64)
    list(APPEND REUSSIR_BACKEND_OPTIONAL_ARCHIVES "whole ${archive}")
  endforeach()
endif()
if(REUSSIR_ENABLE_OPENXLA)
  target_compile_definitions(ReussirCAPI PRIVATE REUSSIR_ENABLE_OPENXLA)
  target_link_libraries(ReussirCAPI PRIVATE ReussirIfrt StablehloRegister)
  # XLA's layout parser also reaches its HLO importer and StableHLO support.
  # Keep the ReussirIfrt/StablehloPortableApi/ReussirStablehlo closure in static
  # link order; the remaining LLVM/MLIR dependencies come from mlir-sys.
  set(REUSSIR_OPENXLA_ARCHIVES
    ReussirIfrt StablehloPortableApi StablehloRegister StablehloSerialization
    StablehloPasses StablehloOptimizationPasses StablehloLinalgTransforms
    StablehloTypeConversion StablehloReplicaGroupUtils StablehloPassUtils
    ChloOps StablehloBroadcastUtils StablehloBroadcastLowering StablehloOps
    StablehloTypeInference VhloOps VhloTypes StablehloAssemblyFormat
    StablehloBase Version)
  add_dependencies(ReussirCAPI ${REUSSIR_OPENXLA_ARCHIVES})
  foreach(archive ${REUSSIR_OPENXLA_ARCHIVES})
    list(APPEND REUSSIR_BACKEND_OPTIONAL_ARCHIVES "plain ${archive}")
  endforeach()
endif()
if(REUSSIR_ENABLE_RCGMP)
  target_link_libraries(ReussirCAPI PRIVATE ReussirRCGMP)
  # Conversions before the dialect library they reference.
  set(REUSSIR_RCGMP_ARCHIVES MLIRRCGMPToSCF MLIRRCGMPToLLVM MLIRRCGMP)
  add_dependencies(ReussirCAPI ${REUSSIR_RCGMP_ARCHIVES})
  foreach(archive ${REUSSIR_RCGMP_ARCHIVES})
    list(APPEND REUSSIR_BACKEND_OPTIONAL_ARCHIVES "plain ${archive}")
  endforeach()
endif()
file(GENERATE
  OUTPUT ${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/reussir-backend-optional-archives.txt
  CONTENT "$<JOIN:${REUSSIR_BACKEND_OPTIONAL_ARCHIVES},\n>")

# The rcgmp integer runtime and the GMP library that in-process JIT hosts link
# (crates/reussir-jit/build.rs), one path per line. Like the manifest above, it
# is written empty when rcgmp is disabled.
set(REUSSIR_JIT_RCGMP_RUNTIME "")
if(REUSSIR_ENABLE_RCGMP)
  set(REUSSIR_JIT_RCGMP_RUNTIME "$<TARGET_FILE:RCGMPRuntime>\n${GMP_LIBRARY}\n")
  add_dependencies(ReussirCAPI RCGMPRuntime)
endif()
file(GENERATE
  OUTPUT ${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/reussir-jit-rcgmp-runtime.txt
  CONTENT "${REUSSIR_JIT_RCGMP_RUNTIME}")

# Aggregate target the Rust crates depend on.
add_custom_target(reussir-capi DEPENDS ReussirCAPI ${REUSSIR_BACKEND_ARCHIVES})
