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

project(lishydro
        VERSION 1.0
        LANGUAGES Fortran)

list(APPEND CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/cmake/modules)

find_package(ESMF MODULE REQUIRED)
if (NOT ESMF_FOUND)
  message(FATAL_ERROR "ESMF library not found. Please set environment variable ESMFMKFILE.")
endif (NOT ESMF_FOUND)

list(APPEND lishydro_app_files
  src/driver/app.F90
  src/driver/driver.F90
  src/driver/fields.F90
  src/driver/mediator.F90
)

if (DEFINED ENV{LISHYDRO_LIS_INST} AND NOT DEFINED LISHYDRO_LIS_INST)
  set(LISHYDRO_LIS_INST $ENV{LISHYDRO_LIS_INST} CACHE FILEPATH "Path to LISHYDRO_LIS_INST")
endif ()
if (DEFINED ENV{LISHYDRO_WRFHYDRO_INST} AND NOT DEFINED LISHYDRO_WRFHYDRO_INST)
  set(LISHYDRO_WRFHYDRO_INST $ENV{LISHYDRO_WRFHYDRO_INST} CACHE FILEPATH "Path to LISHYDRO_WRFHYDRO_INST")
endif ()

add_executable(LISHydroApp ${lishydro_app_files})
target_link_libraries(LISHydroApp PUBLIC ESMF)
target_link_libraries(LISHydroApp PUBLIC ${LISHYDRO_LIS_INST}/liblis_nuopc.a)
target_link_libraries(LISHydroApp PUBLIC ${LISHYDRO_WRFHYDRO_INST}/libwrfhydro_nuopc.a)
target_include_directories(LISHydroApp PUBLIC ${LISHYDRO_LIS_INST})
target_include_directories(LISHydroApp PUBLIC ${LISHYDRO_WRFHYDRO_INST})
install(TARGETS LISHydroApp DESTINATION .)

# find LIS dependencies
find_package(LISDEPS MODULE REQUIRED)
# JASPER
target_link_libraries(LISHydroApp PUBLIC JASPER::JASPER)
# ECCODES
target_link_libraries(LISHydroApp PUBLIC ECCODES::ECCODES)
target_link_libraries(LISHydroApp PUBLIC ECCODES::ECCODES_F90)
# HDF4
target_link_libraries(LISHydroApp PUBLIC HDF4::MFHDF)
target_link_libraries(LISHydroApp PUBLIC HDF4::DF)
# HDF5
target_link_libraries(LISHydroApp PUBLIC HDF5::HDF5)
target_link_libraries(LISHydroApp PUBLIC HDF5::HDF5_FORTRAN)
target_link_libraries(LISHydroApp PUBLIC HDF5::HDF5_HL)
# HDFEOS
target_link_libraries(LISHydroApp PUBLIC HDFEOS::HDFEOS)
target_link_libraries(LISHydroApp PUBLIC HDFEOS::GCTP)
# NETCDF
target_link_libraries(LISHydroApp PUBLIC NETCDF::NETCDF)
target_link_libraries(LISHydroApp PUBLIC NETCDF::NETCDFF)

