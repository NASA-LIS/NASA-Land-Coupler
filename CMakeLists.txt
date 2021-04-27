cmake_minimum_required(VERSION 3.5.2)

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

find_package(ZLIB)

list(APPEND lishydro_app_files
  src/driver/app.F90
  src/driver/driver.F90
  src/driver/fields.F90
  src/driver/mediator.F90
)

set(LIS_MK ${CMAKE_CURRENT_SOURCE_DIR}/LIS-INSTALL/lis.mk)
file(STRINGS ${LIS_MK} LIS_LIB REGEX "ESMF_DEP_LINK_OBJS" )
file(STRINGS ${LIS_MK} LIS_INCLUDE REGEX "ESMF_DEP_INCPATH" )
file(STRINGS ${LIS_MK} LIS_SHRD_LIBS REGEX "ESMF_DEP_SHRD_PATH" )
string(REGEX REPLACE ".*=" "" LIS_LIB ${LIS_LIB})
string(REGEX REPLACE ".*=" "" LIS_INCLUDE ${LIS_INCLUDE})
string(REGEX REPLACE ".*=" "" LIS_SHRD_LIBS ${LIS_SHRD_LIBS})
string(STRIP ${LIS_LIB} LIS_LIB)
string(STRIP ${LIS_INCLUDE} LIS_INCLUDE)
string(STRIP ${LIS_SHRD_LIBS} LIS_SHRD_LIBS)
 
set(WRFHYDRO_MK ${CMAKE_CURRENT_SOURCE_DIR}/WRFHydro-INSTALL/wrfhydro.mk)
file(STRINGS ${WRFHYDRO_MK} WRFHYDRO_LIB REGEX "ESMF_DEP_LINK_OBJS" )
file(STRINGS ${WRFHYDRO_MK} WRFHYDRO_INCLUDE REGEX "ESMF_DEP_INCPATH" )
file(STRINGS ${WRFHYDRO_MK} WRFHYDRO_SHRD_LIBS REGEX "ESMF_DEP_SHRD_PATH" )
string(REGEX REPLACE ".*=" "" WRFHYDRO_LIB ${WRFHYDRO_LIB})
string(REGEX REPLACE ".*=" "" WRFHYDRO_INCLUDE ${WRFHYDRO_INCLUDE})
string(REGEX REPLACE ".*=" "" WRFHYDRO_SHRD_LIBS ${WRFHYDRO_SHRD_LIBS})
string(STRIP ${WRFHYDRO_LIB} WRFHYDRO_LIB)
string(STRIP ${WRFHYDRO_INCLUDE} WRFHYDRO_INCLUDE)
string(STRIP ${WRFHYDRO_SHRD_LIBS} WRFHYDRO_SHRD_LIBS)

add_executable(LISHydroApp ${lishydro_app_files})
target_link_libraries(LISHydroApp PUBLIC ESMF)
target_link_libraries(LISHydroApp PUBLIC ${LIS_LIB})
target_link_libraries(LISHydroApp PUBLIC ${WRFHYDRO_LIB})
target_link_libraries(LISHydroApp PUBLIC z)
target_include_directories(LISHydroApp PUBLIC ${LIS_INCLUDE})
target_include_directories(LISHydroApp PUBLIC ${WRFHYDRO_INCLUDE})
install(TARGETS LISHydroApp DESTINATION bin)
