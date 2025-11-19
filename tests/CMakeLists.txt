add_subdirectory(integration)

if (NOT WIN32)
    message(STATUS "unittest build is skipped on windows")
    add_subdirectory(unittest)
endif()