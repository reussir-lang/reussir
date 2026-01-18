add_custom_target(reussir-semi-elab
    COMMAND ${CMAKE_COMMAND} -E env REUSSIR_PROJECT_ROOT=${CMAKE_SOURCE_DIR} cabal install reussir-semi-elab --installdir ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/bin --install-method=copy --overwrite-policy=always
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    USES_TERMINAL
)

add_dependencies(reussir-semi-elab MLIRReussirBridge)
