execute_process(
  COMMAND ${CARGO_COMMAND} rustc ${CARGO_TARGET_ARGS} --locked
    --profile ${PROFILE} --package reussir-polyffi-cache --lib
    --message-format=json-render-diagnostics
    -- --print=native-static-libs
  RESULT_VARIABLE status
  OUTPUT_VARIABLE output
  ERROR_VARIABLE diagnostics)
if(NOT status EQUAL 0)
  message(FATAL_ERROR "${output}${diagnostics}")
endif()
# Cargo also supplies search paths for native dependencies, notably the
# windows-targets import library that is not bundled into Rust's staticlib.
set(native_paths "")
string(REPLACE "\n" ";" messages "${output}")
foreach(message IN LISTS messages)
  if(NOT message MATCHES "\"reason\":\"build-script-executed\"")
    continue()
  endif()
  string(JSON count LENGTH "${message}" linked_paths)
  if(count EQUAL 0)
    continue()
  endif()
  math(EXPR last "${count} - 1")
  foreach(index RANGE ${last})
    string(JSON path GET "${message}" linked_paths ${index})
    set(prefix "-L")
    if(WIN32 OR CARGO_TARGET_ARGS MATCHES "windows-msvc")
      set(prefix "/libpath:")
    endif()
    if(path MATCHES "^framework=(.*)$")
      set(prefix "-F")
      set(path "${CMAKE_MATCH_1}")
    elseif(path MATCHES "^native=(.*)$")
      set(path "${CMAKE_MATCH_1}")
    endif()
    string(APPEND native_paths "\"${prefix}${path}\"\n")
  endforeach()
endforeach()
string(REGEX MATCH "note: native-static-libs: ([^\r\n]+)" match "${diagnostics}")
if(NOT match)
  message(FATAL_ERROR "rustc did not report native-static-libs: ${diagnostics}")
endif()
# Avoid relinking C++ consumers when Cargo's output has not changed.
file(CONFIGURE OUTPUT "${NATIVE_LIBS}" CONTENT "${native_paths}${CMAKE_MATCH_1}\n" @ONLY)
