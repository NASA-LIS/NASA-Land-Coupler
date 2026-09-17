cmake_minimum_required(VERSION 3.15.5)

foreach(env_var IN ITEMS CMAKE_C_COMPILER CMAKE_CXX_COMPILER CMAKE_Fortran_COMPILER)
  if(NOT DEFINED ENV{${env_var}})
    message(FATAL_ERROR "${env_var} is not defined")
  endif()
endforeach()

set(CMAKE_C_COMPILER $ENV{CMAKE_C_COMPILER})
set(CMAKE_CXX_COMPILER $ENV{CMAKE_CXX_COMPILER})
set(CMAKE_Fortran_COMPILER $ENV{CMAKE_Fortran_COMPILER})
set(CMAKE_Platform $ENV{CMAKE_Platform})
set(CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE)

project(NASA-Land-Coupler
        VERSION 1.0
        LANGUAGES Fortran)

list(APPEND CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/cmake/modules)

# NLC executable
list(APPEND nlc_app_files
  src/driver/app.F90
  src/driver/driver.F90
  src/driver/fields.F90
  src/driver/flags.F90
  src/driver/mediator.F90
)
add_executable(NLC.exe ${nlc_app_files})

# discover MPI
find_package(MPI COMPONENTS Fortran)

# link ESMF
find_package(ESMF MODULE REQUIRED)
if (NOT ESMF_FOUND)
  message(FATAL_ERROR "ESMF library not found. Please set environment variable ESMFMKFILE.")
endif (NOT ESMF_FOUND)
target_link_libraries(NLC.exe PUBLIC ESMF)
list(APPEND nlc_defs_private ESMF_VERSION_MAJOR=${ESMF_VERSION_MAJOR})
list(APPEND nlc_defs_private ESMF_VERSION_MINOR=${ESMF_VERSION_MINOR})

if (COMPONENT_LIST)
  string(REPLACE "," ";" COMPONENT_LIST "${COMPONENT_LIST}")
  foreach (component IN ITEMS ${COMPONENT_LIST})
    string(TOUPPER ${component} component)
    set(ENABLE_${component} ON)
  endforeach()
else()
  set(ENABLE_LIS ON)
  set(ENABLE_WRFHYDRO ON)
  set(ENABLE_PARFLOW ON)
endif()

# link LIS
if (ENABLE_LIS)
  set(NLC_LIS_INST ${CMAKE_INSTALL_PREFIX}/LIS)
  if(NOT EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/src/LISF/lis/make/configure.lis")
    message(FATAL_ERROR "LIS configuration missing.")
  endif()
  message("Build LIS:")
  message("  run: ${CMAKE_BUILD_TOOL} nuopcinstall INSTPATH=${NLC_LIS_INST} BUILD_TYPE=${CMAKE_BUILD_TYPE} COMPILER=${CMAKE_Fortran_COMPILER_ID}")
  message("   in: ${CMAKE_CURRENT_SOURCE_DIR}/src/LISF/lis/runmodes/nuopc_cpl_mode")
  message("")
  add_custom_target(lis_nuopc
    WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/src/LISF/lis/runmodes/nuopc_cpl_mode"
    COMMAND ${CMAKE_BUILD_TOOL} nuopcinstall INSTPATH=${NLC_LIS_INST} BUILD_TYPE=${CMAKE_BUILD_TYPE} COMPILER=${CMAKE_Fortran_COMPILER_ID})
  add_dependencies(NLC.exe lis_nuopc)
  target_link_libraries(NLC.exe PUBLIC ${NLC_LIS_INST}/liblis_nuopc.a)
  target_include_directories(NLC.exe PUBLIC ${NLC_LIS_INST})
  list(APPEND nlc_defs_private NUOPCCAP_LIS=LIS_NUOPC)

  # link LIS dependencies
  include(lis_dependencies)
  target_link_lis_dependencies(NLC.exe)
endif (ENABLE_LIS)

# link WFHYDRO
if (ENABLE_WRFHYDRO)
  message("Build WRFHYDRO:")
  message("   add_subdirectory: ${CMAKE_CURRENT_SOURCE_DIR}/src/wrf_hydro_nwm/trunk/NDHMS")
  message("")
  add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/src/wrf_hydro_nwm/trunk/NDHMS)
  add_dependencies(NLC.exe wrfhydro_nuopc)
  target_link_libraries(NLC.exe PUBLIC wrfhydro_nuopc)
  target_include_directories(NLC.exe PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/src/wrf_hydro_nwm/trunk/NDHMS/mods>)
  list(APPEND nlc_defs_private NUOPCCAP_WRFHYDRO=WRFHydro_NUOPC)

  # link WRFHYDRO dependencies
  include(wrfhydro_dependencies)
  target_link_wrfhydro_dependencies(NLC.exe)
endif (ENABLE_WRFHYDRO)

# link PARFLOW
if (ENABLE_PARFLOW)
  message("Build PARFLOW:")
  message("   add_subdirectory: ${CMAKE_CURRENT_SOURCE_DIR}/src/parflow")
  message("")
  if (NOT DEFINED ENV{PARFLOW_AMPS_LAYER})
    message(FATAL_ERROR "Environment missing PARFLOW_AMPS_LAYER.")
  endif ()
  set(PARFLOW_AMPS_LAYER $ENV{PARFLOW_AMPS_LAYER} CACHE FILEPATH "Setting for PARFLOW_AMPS_LAYER")

  if (NOT DEFINED ENV{PARFLOW_HAVE_CLM})
    message(FATAL_ERROR "Environment missing PARFLOW_HAVE_CLM.")
  endif ()
  set(PARFLOW_HAVE_CLM $ENV{PARFLOW_HAVE_CLM} CACHE FILEPATH "Setting for PARFLOW_HAVE_CLM")
  string(TOUPPER "${PARFLOW_HAVE_CLM}" PARFLOW_HAVE_CLM)
  if (NOT DEFINED ENV{PARFLOW_ENABLE_NETCDF})
    message(FATAL_ERROR "Environment missing PARFLOW_ENABLE_NETCDF.")
  endif ()
  set(PARFLOW_ENABLE_NETCDF $ENV{PARFLOW_ENABLE_NETCDF} CACHE FILEPATH "Setting for PARFLOW_ENABLE_NETCDF")
  string(TOUPPER "${PARFLOW_ENABLE_NETCDF}" PARFLOW_ENABLE_NETCDF)
  if (NOT DEFINED ENV{PARFLOW_HYPRE_DIR})
    message(FATAL_ERROR "Environment missing PARFLOW_HYPRE_DIR.")
  endif ()
  set(HYPRE_ROOT $ENV{PARFLOW_HYPRE_DIR} CACHE FILEPATH "Path to HYPRE_ROOT")
  if (NOT DEFINED ENV{PARFLOW_HDF5_DIR})
    message(FATAL_ERROR "Environment missing PARFLOW_HDF5_DIR.")
  endif ()
  set(HDF5_ROOT $ENV{PARFLOW_HDF5_DIR} CACHE FILEPATH "Path to HDF5_ROOT")
  if (NOT DEFINED ENV{PARFLOW_SILO_DIR})
    message(FATAL_ERROR "Environment missing PARFLOW_SILO_DIR.")
  endif ()
  set(SILO_ROOT $ENV{PARFLOW_SILO_DIR} CACHE FILEPATH "Path to SILO_ROOT")
  set(PARFLOW_ENABLE_NUOPC "ON" CACHE FILEPATH "Set PARFLOW_ENABLE_NUOPC")

  add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/src/parflow)
  add_dependencies(NLC.exe parflow_nuopc)
  target_link_libraries(NLC.exe PUBLIC parflow_nuopc)
  target_include_directories(NLC.exe PUBLIC $<BUILD_INTERFACE:${CMAKE_BINARY_DIR}/include>)
  list(APPEND nlc_defs_private NUOPCCAP_PARFLOW=parflow_nuopc)

  # link PARFLOW dependencies
  include(parflow_dependencies)
  target_link_parflow_dependencies(NLC.exe)
endif (ENABLE_PARFLOW)

# add flags
target_compile_definitions(NLC.exe PRIVATE "${nlc_defs_private}")

# NLC install
install(TARGETS NLC.exe DESTINATION .)
install(FILES ${CMAKE_CURRENT_SOURCE_DIR}/src/driver/fd_nlc.yaml DESTINATION .)

# Configure build tests
include(CTest)
option(NLC_TEST_CACHE_DATA "Cache remote input files for tests" ON)
if (BUILD_TESTING)
  message(STATUS "Build tests: ${BUILD_TESTING}")
  include(NLCTesting)

  # Load test data configuration from TOML
  set(NLC_TEST_CONFIG_FILE "${PROJECT_SOURCE_DIR}/testing/config/nlc-test-config.toml")
  if (NOT EXISTS ${NLC_TEST_CONFIG_FILE})
    message(FATAL_ERROR "Test configuration file not found: ${NLC_TEST_CONFIG_FILE}")
  endif()
  set(NLC_TEST_CONFIG_PARSE "${PROJECT_SOURCE_DIR}/testing/scripts/nlc_test_config_parser.py")
  if (NOT EXISTS ${NLC_TEST_CONFIG_PARSE})
    message(FATAL_ERROR "Test configuration parser not found: ${NLC_TEST_CONFIG_PARSE}")
  endif()
  execute_process(
    COMMAND ${Python3_EXECUTABLE} ${NLC_TEST_CONFIG_PARSE} ${NLC_TEST_CONFIG_FILE} --cmake
    OUTPUT_VARIABLE NLC_TEST_CONFIG_OUTPUT
    RESULT_VARIABLE NLC_TEST_CONFIG_RESULT
  )
  if (NOT NLC_TEST_CONFIG_RESULT EQUAL 0)
    message(FATAL_ERROR "Failed to load test configuration")
  endif()
  cmake_language(EVAL CODE ${NLC_TEST_CONFIG_OUTPUT})

  foreach(NLC_TEST_NAME IN LISTS NLC_TESTS)
    # Process test-specific variables
    string(TOUPPER "${NLC_TEST_NAME}" NLC_TEST_NAME)
    set(NLC_TEST_MPI ${NLC_TEST_${NLC_TEST_NAME}_MPI})
    set(NLC_TEST_COMPS ${NLC_TEST_${NLC_TEST_NAME}_COMPS})

    # Check if MPI is required for the test and if MPI is found
    if (NLC_TEST_MPI)
      if (NOT MPI_Fortran_FOUND)
        message(FATAL_ERROR "MPI not found, cannot add MPI test: ${NLC_TEST_NAME}")
      endif()
    endif()

    # Check if the components required for the test are enabled
    foreach(NLC_TEST_COMP IN LISTS NLC_TEST_COMPS)
      string(TOUPPER "${NLC_TEST_COMP}" NLC_TEST_COMP)
      if(NOT ENABLE_${NLC_TEST_COMP})
        message(FATAL_ERROR "Component ${NLC_TEST_COMP} is not enabled, cannot add test: ${NLC_TEST_NAME}")
      endif()
    endforeach()

    # Add the test using the nlc_add_test function
    nlc_add_test(
      NAME ${NLC_TEST_NAME}
      EXECUTABLE $<TARGET_FILE:NLC.exe>
      CONFIG_FILE ${NLC_TEST_CONFIG_FILE}
      CACHE_DATA ${NLC_TEST_CACHE_DATA}
    )
  endforeach()

else()
  message(STATUS "Build tests: OFF")
endif()

add_custom_target(nlc_tests
  COMMAND ${CMAKE_CTEST_COMMAND} -L nlc_test
  COMMENT "Running NLC regression tests"
  DEPENDS NLC.exe
)
