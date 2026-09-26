get_property(dialect_libs GLOBAL PROPERTY MLIR_DIALECT_LIBS)
get_property(conversion_libs GLOBAL PROPERTY MLIR_CONVERSION_LIBS)
get_property(extension_libs GLOBAL PROPERTY MLIR_EXTENSION_LIBS)

set(LLVM_OPTIONAL_SOURCES Stablehlo.cpp)
if(REUSSIR_ENABLE_OPENXLA)
  set(stablehlo_sources Stablehlo.cpp)
endif()

add_mlir_tool(reussir-opt
  main.cpp
  ${stablehlo_sources}
  DEPENDS
    MLIROptLib
    MLIRReussir
    MLIRReussirConversion
    MLIRReussirTransformation
    MLIRSync
    MLIRSyncConversion
    ${dialect_libs}
    ${conversion_libs}
    ${extension_libs}
)

target_link_libraries(reussir-opt
  PRIVATE
    MLIRReussir
    MLIRReussirConversion
    MLIRReussirTransformation
    MLIRSync
    MLIRSyncConversion
    ${dialect_libs}
    ${conversion_libs}
    ${extension_libs}
    MLIROptLib
)

if(REUSSIR_ENABLE_OPENXLA)
  target_compile_definitions(reussir-opt PRIVATE REUSSIR_ENABLE_OPENXLA)
  target_link_libraries(reussir-opt PRIVATE ReussirStablehlo)
endif()

if (LLVM_VERSION VERSION_GREATER_EQUAL 22)
  target_link_libraries(reussir-opt
    PRIVATE
      MLIRRegisterAllDialects
      MLIRRegisterAllExtensions
      MLIRRegisterAllPasses
  )
else()
  message(STATUS "LLVM_VERSION is ${LLVM_VERSION}, skipping All(*) libs")
endif()
