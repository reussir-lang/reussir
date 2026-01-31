function(add_frontend_target target_name)
    add_custom_target(${target_name}
        COMMAND ${CMAKE_COMMAND} -E env REUSSIR_PROJECT_ROOT=${CMAKE_SOURCE_DIR} cabal install -j ${target_name} --installdir ${CMAKE_RUNTIME_OUTPUT_DIRECTORY} --install-method=copy --overwrite-policy=always
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
        USES_TERMINAL
    )
    add_dependencies(${target_name} MLIRReussirBridge)
endfunction()

add_frontend_target(reussir-elab)
add_frontend_target(reussir-compiler)
add_frontend_target(reussir-repl)
add_frontend_target(reussir-parser)
