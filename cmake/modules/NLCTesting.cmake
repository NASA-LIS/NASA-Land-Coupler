# Helper functions for building and running tests in NLC

# Usage:
# nlc_add_test(
#   NAME my_test
#   EXECUTABLE my_exe
#   CONFIG_FILE my_config.toml
#   CACHE_DATA ON
# )
function(nlc_add_test)
  include(FetchContent)
  set(NLC_TEST_DIR ${CMAKE_CURRENT_BINARY_DIR}/NLC_TESTS)
  file(MAKE_DIRECTORY ${NLC_TEST_DIR})
  set(NLC_TEST_SCRIPT "${PROJECT_SOURCE_DIR}/testing/scripts/nlc_test_execute.py")
  if(NOT EXISTS ${NLC_TEST_SCRIPT})
    message(FATAL_ERROR "nlc_test_execute.py script not found: ${NLC_TEST_SCRIPT}")
  endif()
  # parse arguments
  set(one_value_keywords
    NAME # test name
    EXECUTABLE # test executable
    CONFIG_FILE # test configuration file
    CACHE_DATA # whether to cache remote input files for the test
  )
  cmake_parse_arguments(NLC_TEST
    ""
    "${one_value_keywords}"
    ""
    ${ARGN}
  )
  if(NLC_TEST_UNPARSED_ARGUMENTS)
    message(FATAL_ERROR "Detected invalid arguments: ${NLC_TEST_UNPARSED_ARGUMENTS}")
  endif()
  if(NOT NLC_TEST_NAME)
    message(FATAL_ERROR "Missing required argument: NAME")
  endif()
  if(NOT NLC_TEST_EXECUTABLE)
    message(FATAL_ERROR "Missing required argument: EXECUTABLE")
  endif()
  if(NOT NLC_TEST_CONFIG_FILE)
    message(FATAL_ERROR "Missing required argument: CONFIG_FILE")
  endif()

  # Run command for test
  set(NLC_TEST_RUN_CMD "")
  list(APPEND NLC_TEST_RUN_CMD "${NLC_TEST_DIR}")
  list(APPEND NLC_TEST_RUN_CMD "${NLC_TEST_EXECUTABLE}")
  list(APPEND NLC_TEST_RUN_CMD "${NLC_TEST_CONFIG_FILE}")
  if(MPI_FOUND)
    list(APPEND NLC_TEST_RUN_CMD
      "--mpi-exec=${MPIEXEC_EXECUTABLE}"
      "--mpi-numproc-flag=${MPIEXEC_NUMPROC_FLAG}"
    )
  endif()
  list(APPEND NLC_TEST_RUN_CMD
    "--tests=${NLC_TEST_NAME}"
  )
  list(APPEND NLC_TEST_RUN_CMD
    "--clean"
  )

  # cache remote input files for the test
  if(NLC_TEST_CACHE_DATA)
    execute_process(
      COMMAND ${Python3_EXECUTABLE} ${NLC_TEST_SCRIPT} ${NLC_TEST_RUN_CMD} --cache-only
      RESULT_VARIABLE NLC_TEST_CACHE_RESULT
    )
    if(NLC_TEST_CACHE_RESULT)
      message(FATAL_ERROR "Caching remote input files failed for test: ${NLC_TEST_NAME}")
    endif()
  endif()

  add_test(
    NAME ${NLC_TEST_NAME}
    COMMAND ${Python3_EXECUTABLE} ${NLC_TEST_SCRIPT} ${NLC_TEST_RUN_CMD}
    COMMAND_EXPAND_LISTS
  )
  set_property(TEST ${NLC_TEST_NAME} APPEND PROPERTY LABELS "nlc_test")

  message(STATUS "Configured test: ${NLC_TEST_NAME}")
endfunction()
