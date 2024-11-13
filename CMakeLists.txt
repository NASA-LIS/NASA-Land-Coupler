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
endif()

# link LIS
if (ENABLE_LIS)
  if (DEFINED ENV{NLC_LIS_INST} AND NOT DEFINED NLC_LIS_INST)
    set(NLC_LIS_INST $ENV{NLC_LIS_INST} CACHE FILEPATH "Path to NLC_LIS_INST")
  elseif (NOT DEFINED NLC_LIS_INST)
    message(FATAL_ERROR "Environment missing NLC_LIS_INST.")
  endif ()
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

# add flags
target_compile_definitions(NLC.exe PRIVATE "${nlc_defs_private}")

# NLC install
install(TARGETS NLC.exe DESTINATION .)
