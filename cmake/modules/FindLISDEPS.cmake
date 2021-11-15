# - Try to find LIS Dependencies
#
# Requires setting ESMFMKFILE to the filepath of esmf.mk. If this is NOT set,
# then ESMF_FOUND will always be FALSE. If ESMFMKFILE exists, then ESMF_FOUND=TRUE
# and all ESMF makefile variables will be set in the global scope. Optionally,
# set ESMF_MKGLOBALS to a string list to filter makefile variables. For example,
# to globally scope only ESMF_LIBSDIR and ESMF_APPSDIR variables, use this CMake
# command in CMakeLists.txt:
#
#   set(ESMF_MKGLOBALS "LIBSDIR" "APPSDIR")

if (DEFINED ENV{LIS_JASPER} AND NOT DEFINED LIS_JASPER)
  set(LIS_JASPER $ENV{LIS_JASPER} CACHE FILEPATH "Path to JASPER directory")
endif ()
if (DEFINED ENV{LIS_GRIBAPI} AND NOT DEFINED LIS_GRIBAPI)
  set(LIS_GRIBAPI $ENV{LIS_GRIBAPI} CACHE FILEPATH "Path to GRIBAPI directory")
endif()
if (DEFINED ENV{LIS_ECCODES} AND NOT DEFINED LIS_ECCODES)
  set(LIS_ECCODES $ENV{LIS_ECCODES} CACHE FILEPATH "Path to ECCODES directory")
endif ()
if (DEFINED ENV{LIS_NETCDF} AND NOT DEFINED LIS_NETCDF)
  set(LIS_NETCDF $ENV{LIS_NETCDF} CACHE FILEPATH "Path to NETCDF directory")
endif ()
if (DEFINED ENV{LIS_HDF4} AND NOT DEFINED LIS_HDF4)
  set(LIS_HDF4 $ENV{LIS_HDF4} CACHE FILEPATH "Path to HDF4 directory")
endif ()
if (DEFINED ENV{LIS_HDFEOS} AND NOT DEFINED LIS_HDFEOS)
  set(LIS_HDFEOS $ENV{LIS_HDFEOS} CACHE FILEPATH "Path to HDFEOS directory")
endif ()
if (DEFINED ENV{LIS_HDF5} AND NOT DEFINED LIS_HDF5)
  set(LIS_HDF5 $ENV{LIS_HDF5} CACHE FILEPATH "Path to HDF5 directory")
endif ()

# JASPER
if (EXISTS ${LIS_JASPER})
  set(JASPER_FOUND TRUE CACHE BOOL "JASPER directory file found" FORCE)
else()
  set(JASPER_FOUND FALSE CACHE BOOL "JASPER directory NOT found" FORCE)
  if (NOT DEFINED LIS_JASPER)
    message(WARNING "LIS_JASPER not defined")
  endif ()
endif()
add_library(JASPER::JASPER STATIC IMPORTED)
find_library(JASPER_LIB NAMES jasper PATHS ${LIS_JASPER} PATH_SUFFIXES lib lib64)
set_target_properties(JASPER::JASPER PROPERTIES
  IMPORTED_LOCATION "${JASPER_LIB}")
# GRIBAPI
if (EXISTS ${LIS_GRIBAPI})
  set(GRIBAPI_FOUND TRUE CACHE BOOL "GRIBAPI directory file found" FORCE)
else()
  set(GRIBAPI_FOUND FALSE CACHE BOOL "GRIBAPI directory NOT found" FORCE)
  if (NOT DEFINED LIS_GRIBAPI)
    message(WARNING "LIS_GRIBAPI not defined")
  endif ()
endif()
add_library(GRIBAPI::GRIBAPI STATIC IMPORTED)
find_library(GRIBAPI_LIB NAMES grib_api PATHS ${LIS_GRIBAPI} PATH_SUFFIXES lib lib64)
set_target_properties(GRIBAPI::GRIBAPI PROPERTIES
  IMPORTED_LOCATION "${GRIBAPI_LIB}")
add_library(GRIBAPI::GRIBAPI_F90 STATIC IMPORTED)
find_library(GRIBAPI_F90_LIB NAMES grib_api_f90 PATHS ${LIS_GRIBAPI} PATH_SUFFIXES lib lib64)
set_target_properties(GRIBAPI::GRIBAPI_F90 PROPERTIES
  IMPORTED_LOCATION "${GRIBAPI_F90_LIB}")
# ECCODES
if (EXISTS ${LIS_ECCODES})
  set(ECCODES_FOUND TRUE CACHE BOOL "ECCODES directory file found" FORCE)
else()
  set(ECCODES_FOUND FALSE CACHE BOOL "ECCODES directory NOT found" FORCE)
  if (NOT DEFINED LIS_ECCODES)
    message(WARNING "LIS_ECCODES not defined")
  endif ()
endif()
add_library(ECCODES::ECCODES STATIC IMPORTED)
find_library(ECCODES_LIB NAMES eccodes PATHS ${LIS_ECCODES} PATH_SUFFIXES lib lib64)
set_target_properties(ECCODES::ECCODES PROPERTIES
  IMPORTED_LOCATION "${ECCODES_LIB}")
add_library(ECCODES::ECCODES_F90 STATIC IMPORTED)
find_library(ECCODES_F90_LIB NAMES eccodes_f90 PATHS ${LIS_ECCODES} PATH_SUFFIXES lib lib64)
set_target_properties(ECCODES::ECCODES_F90 PROPERTIES
  IMPORTED_LOCATION "${ECCODES_F90_LIB}")
# HDF4
if (EXISTS ${LIS_HDF4})
  set(HDF4_FOUND TRUE CACHE BOOL "HDF4 directory file found" FORCE)
else()
  set(HDF4_FOUND FALSE CACHE BOOL "HDF4 directory NOT found" FORCE)
  if (NOT DEFINED LIS_HDF4)
    message(WARNING "LIS_HDF4 not defined")
  endif ()
endif()
add_library(HDF4::DF STATIC IMPORTED)
find_library(DF_LIB NAMES df PATHS ${LIS_HDF4} PATH_SUFFIXES lib lib64)
set_target_properties(HDF4::DF PROPERTIES
  IMPORTED_LOCATION "${DF_LIB}"
  INTERFACE_LINK_LIBRARIES "-ljpeg -lz")
add_library(HDF4::MFHDF STATIC IMPORTED)
find_library(MFHDF_LIB NAMES mfhdf PATHS ${LIS_HDF4} PATH_SUFFIXES lib lib64)
set_target_properties(HDF4::MFHDF PROPERTIES
  IMPORTED_LOCATION "${MFHDF_LIB}")
# HDF5
if (EXISTS ${LIS_HDF5})
  set(HDF5_FOUND TRUE CACHE BOOL "HDF5 directory file found" FORCE)
else()
  set(HDF5_FOUND FALSE CACHE BOOL "HDF5 directory NOT found" FORCE)
  if (NOT DEFINED LIS_HDF5)
    message(WARNING "LIS_HDF5 not defined")
  endif ()
endif()
add_library(HDF5::HDF5 STATIC IMPORTED)
find_library(HDF5_LIB NAMES hdf5 PATHS ${LIS_HDF5} PATH_SUFFIXES lib lib64)
set_target_properties(HDF5::HDF5 PROPERTIES
  IMPORTED_LOCATION "${HDF5_LIB}")
add_library(HDF5::HDF5_FORTRAN STATIC IMPORTED)
find_library(HDF5_FORTRAN_LIB NAMES hdf5_fortran PATHS ${LIS_HDF5} PATH_SUFFIXES lib lib64)
set_target_properties(HDF5::HDF5_FORTRAN PROPERTIES
  IMPORTED_LOCATION "${HDF5_FORTRAN_LIB}")
add_library(HDF5::HDF5_HL STATIC IMPORTED)
find_library(HDF5_HL_LIB NAMES hdf5_hl PATHS ${LIS_HDF5} PATH_SUFFIXES lib lib64)
set_target_properties(HDF5::HDF5_HL PROPERTIES
  IMPORTED_LOCATION "${HDF5_HL_LIB}")
# HDFEOS
if (EXISTS ${LIS_HDFEOS})
  set(HDFEOS_FOUND TRUE CACHE BOOL "HDFEOS directory file found" FORCE)
else()
  set(HDFEOS_FOUND FALSE CACHE BOOL "HDFEOS directory NOT found" FORCE)
  if (NOT DEFINED LIS_HDFEOS)
    message(WARNING "LIS_HDFEOS not defined")
  endif ()
endif()
add_library(HDFEOS::HDFEOS STATIC IMPORTED)
find_library(HDFEOS_LIB NAMES hdfeos PATHS ${LIS_HDFEOS} PATH_SUFFIXES lib lib64)
set_target_properties(HDFEOS::HDFEOS PROPERTIES
  IMPORTED_LOCATION "${HDFEOS_LIB}")
add_library(HDFEOS::GCTP STATIC IMPORTED)
find_library(GCTP_LIB NAMES Gctp PATHS ${LIS_HDFEOS} PATH_SUFFIXES lib lib64)
set_target_properties(HDFEOS::GCTP PROPERTIES
  IMPORTED_LOCATION "${GCTP_LIB}")
# NETCDF
if (EXISTS ${LIS_NETCDF})
  set(NETCDF_FOUND TRUE CACHE BOOL "NETCDF directory file found" FORCE)
else()
  set(NETCDF_FOUND FALSE CACHE BOOL "NETCDF directory NOT found" FORCE)
  if (NOT DEFINED LIS_NETCDF)
    message(WARNING "LIS_NETCDF not defined")
  endif ()
endif()
add_library(NETCDF::NETCDF STATIC IMPORTED)
find_library(NETCDF_LIB NAMES netcdf PATHS ${LIS_NETCDF} PATH_SUFFIXES lib lib64)
set_target_properties(NETCDF::NETCDF PROPERTIES
  IMPORTED_LOCATION "${NETCDF_LIB}")
add_library(NETCDF::NETCDFF STATIC IMPORTED)
find_library(NETCDFF_LIB NAMES netcdff PATHS ${LIS_NETCDF} PATH_SUFFIXES lib lib64)
set_target_properties(NETCDF::NETCDFF PROPERTIES
  IMPORTED_LOCATION "${NETCDFF_LIB}")

