include(FetchContent)

set(CMAKE_DISABLE_FIND_PACKAGE_Boost ON)
set(CMAKE_DISABLE_FIND_PACKAGE_BoehmGC ON)
set(CMAKE_WARN_DEPRECATED OFF)

FetchContent_Declare(
  immer
  GIT_REPOSITORY https://github.com/arximboldi/immer.git
  GIT_TAG df6ef46d97e1fe81f397015b9aeb32505cef653b
)

# Set all Immer build options to OFF
set(immer_BUILD_TESTS OFF CACHE BOOL "Build tests" FORCE)
set(immer_BUILD_PERSIST_TESTS OFF CACHE BOOL "Build experimental persist tests" FORCE)
set(immer_BUILD_EXAMPLES OFF CACHE BOOL "Build examples" FORCE)
set(immer_BUILD_DOCS OFF CACHE BOOL "Build docs" FORCE)
set(immer_BUILD_EXTRAS OFF CACHE BOOL "Build extras" FORCE)

FetchContent_MakeAvailable(immer)
