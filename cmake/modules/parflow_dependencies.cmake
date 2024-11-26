#   Link PARFLOW Dependencies to target
#

function(target_link_parflow_dependencies parflow_target)

  # HYPRE
  if (DEFINED ENV{PARFLOW_HYPRE_DIR} AND NOT DEFINED PARFLOW_HYPRE_DIR)
    set(PARFLOW_HYPRE_DIR $ENV{PARFLOW_HYPRE_DIR} CACHE FILEPATH "Path to HYPRE directory")
  endif ()
  if (EXISTS ${PARFLOW_HYPRE_DIR})
    set(HYPRE_FOUND TRUE CACHE BOOL "HYPRE directory file found" FORCE)
    add_library(PARFLOWDEPS::HYPRE UNKNOWN IMPORTED)
    find_library(HYPRE_LIB NAMES HYPRE PATHS ${PARFLOW_HYPRE_DIR} PATH_SUFFIXES lib lib64)
    set_target_properties(PARFLOWDEPS::HYPRE PROPERTIES
      IMPORTED_LOCATION "${HYPRE_LIB}")
    target_link_libraries(${parflow_target} PUBLIC PARFLOWDEPS::HYPRE)
  else()
    set(HYPRE_FOUND FALSE CACHE BOOL "HYPRE directory NOT found" FORCE)
    if (NOT DEFINED PARFLOW_HYPRE_DIR)
      message(WARNING "PARFLOW_HYPRE_DIR not defined")
    endif ()
  endif()

  # HDF5
  if (DEFINED ENV{PARFLOW_HDF5_DIR} AND NOT DEFINED PARFLOW_HDF5_DIR)
    set(PARFLOW_HDF5_DIR $ENV{PARFLOW_HDF5_DIR} CACHE FILEPATH "Path to HDF5 directory")
  endif ()
  if (EXISTS ${PARFLOW_HDF5_DIR})
    set(HDF5_FOUND TRUE CACHE BOOL "HDF5 directory file found" FORCE)
    add_library(PARFLOWDEPS::HDF5 UNKNOWN IMPORTED)
    find_library(HDF5_LIB NAMES hdf5 PATHS ${PARFLOW_HDF5_DIR} PATH_SUFFIXES lib lib64)
    set_target_properties(PARFLOWDEPS::HDF5 PROPERTIES
      IMPORTED_LOCATION "${HDF5_LIB}")
    add_library(PARFLOWDEPS::HDF5_FORTRAN UNKNOWN IMPORTED)
    find_library(HDF5_FORTRAN_LIB NAMES hdf5_fortran PATHS ${PARFLOW_HDF5_DIR} PATH_SUFFIXES lib lib64)
    set_target_properties(PARFLOWDEPS::HDF5_FORTRAN PROPERTIES
      IMPORTED_LOCATION "${HDF5_FORTRAN_LIB}")
    add_library(PARFLOWDEPS::HDF5_HL UNKNOWN IMPORTED)
    find_library(HDF5_HL_LIB NAMES hdf5_hl PATHS ${PARFLOW_HDF5_DIR} PATH_SUFFIXES lib lib64)
    set_target_properties(PARFLOWDEPS::HDF5_HL PROPERTIES
      IMPORTED_LOCATION "${HDF5_HL_LIB}")
    target_link_libraries(${parflow_target} PUBLIC PARFLOWDEPS::HDF5)
  else()
    set(HDF5_FOUND FALSE CACHE BOOL "HDF5 directory NOT found" FORCE)
    if (NOT DEFINED PARFLOW_HDF5_DIR)
      message(WARNING "PARFLOW_HDF5_DIR not defined")
    endif ()
  endif()

  # SILO
  if (DEFINED ENV{PARFLOW_SILO_DIR} AND NOT DEFINED PARFLOW_SILO_DIR)
    set(PARFLOW_SILO_DIR $ENV{PARFLOW_SILO_DIR} CACHE FILEPATH "Path to SILO directory")
  endif()
  if (EXISTS ${PARFLOW_SILO_DIR})
    set(SILO_FOUND TRUE CACHE BOOL "SILO directory file found" FORCE)
    add_library(PARFLOWDEPS::SILO UNKNOWN IMPORTED)
    find_library(SILO_LIB NAMES silo PATHS ${PARFLOW_SILO_DIR} PATH_SUFFIXES lib lib64)
    set_target_properties(PARFLOWDEPS::SILO PROPERTIES
      IMPORTED_LOCATION "${SILO_LIB}")
    target_link_libraries(${parflow_target} PUBLIC PARFLOWDEPS::SILO)
  else()
    set(SILO_FOUND FALSE CACHE BOOL "SILO directory NOT found" FORCE)
    if (NOT DEFINED PARFLOW_SILO_DIR)
      message(WARNING "PARFLOW_SILO_DIR not defined")
    endif ()
  endif()

endfunction()
