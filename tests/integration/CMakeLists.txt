
message(STATUS "finding tools in ${LLVM_TOOLS_BINARY_DIR}")

function(find_llvm_tool VAR TOOL)
    find_program(${VAR}
        NAMES "${TOOL}${CMAKE_EXECUTABLE_SUFFIX}"
        HINTS ${LLVM_TOOLS_BINARY_DIR}
        REQUIRED)
    message(STATUS "${TOOL} path: ${${VAR}}")
endfunction()

find_llvm_tool(filecheck_path FileCheck)
find_llvm_tool(not_path not)
find_llvm_tool(opt_path opt)
find_llvm_tool(llc_path llc)
find_llvm_tool(lli_path lli)

if (WIN32)
  set(extra_sys_libs "-lucrt -lntdll -lws2_32 -ladvapi32 -lbcrypt -luserenv")
  set(REUSSIR_LIBRARY_PATH ${CMAKE_RUNTIME_OUTPUT_DIRECTORY})
else()
  set(REUSSIR_LIBRARY_PATH ${CMAKE_LIBRARY_OUTPUT_DIRECTORY})
endif()

set(rustc_path ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/rustc${CMAKE_EXECUTABLE_SUFFIX})
set(reussir_elab_path ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/reussir-elab${CMAKE_EXECUTABLE_SUFFIX})
set(reussir_compiler_path ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/reussir-compiler${CMAKE_EXECUTABLE_SUFFIX})
configure_file(lit.site.cfg.py.in lit.site.cfg.py @ONLY)

if(DEFINED ENV{MSYSTEM_PREFIX})
  # e.g. MSYSTEM_PREFIX = /mingw64 or /clang64
  message(STATUS "MSYS environment detected: $ENV{MSYSTEM_PREFIX}")

  # Convert /mingw64 -> C:/msys64/mingw64
  file(TO_CMAKE_PATH "$ENV{MSYSTEM_PREFIX}" _msys_prefix_normalized)

  # Build the full path to python
  set(_msys_python "${_msys_prefix_normalized}/bin/python3.exe")

  if(EXISTS "${_msys_python}")
    message(STATUS "Using MSYS Python at: ${_msys_python}")
    set(Python3_EXECUTABLE "${_msys_python}" CACHE FILEPATH "" FORCE)
  else()
    message(WARNING "MSYS Python was expected at ${_msys_python} but not found.")
  endif()
endif()

find_package(Python3 REQUIRED COMPONENTS Interpreter)

add_custom_target(check
  COMMAND ${Python3_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/lit "${CMAKE_CURRENT_BINARY_DIR}" -v
  DEPENDS reussir-opt reussir-rt reussir-translate reussir-elab)

