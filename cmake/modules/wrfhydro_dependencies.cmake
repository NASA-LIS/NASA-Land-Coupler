#   Link WRFHYDRO Dependencies to target
#

function(target_link_wrfhydro_dependencies wrfhydro_target)

  # NETCDF
  if (DEFINED ENV{NETCDF} AND NOT DEFINED NETCDF)
    set(NETCDF $ENV{NETCDF} CACHE FILEPATH "Path to NETCDF directory")
  endif ()
  if (EXISTS ${NETCDF})
    set(NETCDF_FOUND TRUE CACHE BOOL "NETCDF directory file found" FORCE)
    add_library(WRFHYDRO::NETCDF UNKNOWN IMPORTED)
    find_library(NETCDF_LIB NAMES netcdf PATHS ${NETCDF} PATH_SUFFIXES lib lib64)
    set_target_properties(WRFHYDRO::NETCDF PROPERTIES
      IMPORTED_LOCATION "${NETCDF_LIB}")
    target_link_libraries(${wrfhydro_target} PUBLIC WRFHYDRO::NETCDF)
    add_library(WRFHYDRO::NETCDFF UNKNOWN IMPORTED)
    find_library(NETCDFF_LIB NAMES netcdff PATHS ${NETCDF} PATH_SUFFIXES lib lib64)
    set_target_properties(WRFHYDRO::NETCDFF PROPERTIES
      IMPORTED_LOCATION "${NETCDFF_LIB}")
    target_link_libraries(${wrfhydro_target} PUBLIC WRFHYDRO::NETCDFF)
  else()
    set(NETCDF_FOUND FALSE CACHE BOOL "NETCDF directory NOT found" FORCE)
    if (NOT DEFINED NETCDF)
      message(WARNING "NETCDF not defined")
    endif ()
  endif()

endfunction()
