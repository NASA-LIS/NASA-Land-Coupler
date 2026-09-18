# Link MPAS dependencies to target
function(target_link_mpas_dependencies mpas_target)

# Parallel NetCDF
  if (DEFINED ENV{PNETCDF} AND NOT DEFINED PNETCDF)
    set(PNETCDF $ENV{PNETCDF} CACHE FILEPATH "Path to PNETCDF directory")
  endif ()
  if (EXISTS ${PNETCDF})
    set(PNETCDF_FOUND TRUE CACHE BOOL "PNETCDF directory file found" FORCE)
    add_library(MPAS::PNETCDF UNKNOWN IMPORTED)
    find_library(PNETCDF_LIB NAMES pnetcdf PATHS ${PNETCDF} PATH_SUFFIXES lib lib64)
    set_target_properties(MPAS::PNETCDF PROPERTIES
      IMPORTED_LOCATION "${PNETCDF_LIB}")
    target_link_libraries(${mpas_target} PUBLIC MPAS::PNETCDF)
  else()
    set(PNETCDF_FOUND FALSE CACHE BOOL "PNETCDF directory NOT found" FORCE)
    if (NOT DEFINED PNETCDF)
      message(WARNING "PNETCDF not defined")
    endif ()
  endif()

endfunction()
