#!/bin/bash

# usage instructions
usage () {
  printf "Usage: $0 [OPTIONS]...\n"
  printf "\n"
  printf "OPTIONS\n"
  printf "  --system=SYSTEM\n"
  printf "      name of machine (e.g. 'discover', 'cheyenne'\n"
  printf "  --compiler=COMPILER\n"
  printf "      compiler to use; valid options are 'intel.X.Y.Z', \n"
  printf "      'gnu.X.Y.Z'; default is system dependent.\n"
  printf "  --components=\"COMPONENT1,COMPONENT2...\"\n"
  printf "      components to include in build; delimited with ','\n"
  printf "  --continue\n"
  printf "      continue with existing build\n"
  printf "  --clean\n"
  printf "      removes existing build; will override --continue\n"
  printf "  --build-dir=BUILD_DIR\n"
  printf "      build directory\n"
  printf "  --build-type=BUILD_TYPE\n"
  printf "      build type; valid options are 'debug', 'release',\n"
  printf "      'relWithDebInfo'\n"
  printf "  --install-dir=INSTALL_DIR\n"
  printf "      installation prefix\n"
  printf "  --verbose, -v\n"
  printf "      build with verbose output\n"
  printf "\n"
}

# print settings
settings () {
  printf "Settings:\n"
  printf "\n"
  printf "  LISHYDRO_DIR=${LISHYDRO_DIR}\n"
  printf "  BUILD_DIR=${BUILD_DIR}\n"
  printf "  INSTALL_DIR=${INSTALL_DIR}\n"
  printf "  SYSTEM=${SYSTEM}\n"
  printf "  COMPILER=${COMPILER}\n"
  if [ ! -z "${COMPONENTS}" ]; then printf "  COMPONENTS=${COMPONENTS}\n"; fi
  printf "  CLEAN=${CLEAN}\n"
  printf "  CONTINUE=${CONTINUE}\n"
  printf "  BUILD_TYPE=${BUILD_TYPE}\n"
  printf "  VERBOSE=${VERBOSE}\n"
  printf "\n"
}

# find system name
find_system () {
    local sysname=`hostname`
    sysname="${sysname//[[:digit:]]/}"
    echo "$sysname"
}

# default settings
LISHYDRO_DIR=$(cd "$(dirname "$(readlink -f -n "${BASH_SOURCE[0]}" )" )" && pwd -P)
BUILD_DIR=${LISHYDRO_DIR}/build
INSTALL_DIR=${LISHYDRO_DIR}/src/driver
SYSTEM=""
COMPILER=""
COMPONENTS=""
BUILD_TYPE="release"
CLEAN=false
CONTINUE=false
VERBOSE=false

# required arguments
if [ "$1" = "--help" ] || [ "$1" = "-h" ]; then
  usage
  exit 0
fi

# process arguments
while :; do
  case $1 in
    --help|-h) usage; exit 0 ;;
    --system=?*) SYSTEM=${1#*=} ;;
    --system) printf "ERROR: $1 requires an argument.\n"; usage; exit 1 ;;
    --system=) printf "ERROR: $1 requires an argument.\n"; usage; exit 1 ;;
    --compiler=?*) COMPILER=${1#*=} ;;
    --compiler) printf "ERROR: $1 requires an argument.\n"; usage; exit 1 ;;
    --compiler=) printf "ERROR: $1 requires an argument.\n"; usage; exit 1 ;;
    --components=?*) COMPONENTS=${1#*=} ;;
    --components) printf "ERROR: $1 requires an argument.\n"; usage; exit 1 ;;
    --components=) printf "ERROR: $1 requires an argument.\n"; usage; exit 1 ;;
    --build-dir=?*) BUILD_DIR=${1#*=} ;;
    --build-dir) printf "ERROR: $1 requires an argument.\n"; usage; exit 1 ;;
    --build-dir=) printf "ERROR: $1 requires an argument.\n"; usage; exit 1 ;;
    --install-dir=?*) INSTALL_DIR=${1#*=} ;;
    --install-dir) printf "ERROR: $1 requires an argument.\n"; usage; exit 1 ;;
    --install-dir=) printf "ERROR: $1 requires an argument.\n"; usage; exit 1 ;;
    --clean) CLEAN=true ;;
    --clean=?*) printf "ERROR: $1 argument ignored.\n"; usage; exit 1 ;;
    --clean=) printf "ERROR: $1 argument ignored.\n"; usage; exit 1 ;;
    --continue) CONTINUE=true ;;
    --continue=?*) printf "ERROR: $1 argument ignored.\n"; usage; exit 1 ;;
    --continue=) printf "ERROR: $1 argument ignored.\n"; usage; exit 1 ;;
    --verbose|-v) VERBOSE=true ;;
    --verbose=?*) printf "ERROR: $1 argument ignored.\n"; usage; exit 1 ;;
    --verbose=) printf "ERROR: $1 argument ignored.\n"; usage; exit 1 ;;
    -?*) printf "ERROR: Unknown option $1\n"; usage; exit 1 ;;
    *) break
  esac
  shift
done

set -eu

# automatically determine system
if [ -z "${SYSTEM}" ] ; then
  SYSTEM=$(find_system)
fi

# automatically determine compiler
if [ -z "${COMPILER}" ] ; then
  if [ "${SYSTEM}" = "discover" ]; then
    COMPILER="intel.19.1.0"
  elif [ "${SYSTEM}" = "cheyenne" ]; then
    COMPILER="env.cheyenne.intel.17.0.1"
  else
    printf "ERROR: no default compiler for ${SYSTEM}\n"
    printf "\n"
    exit 1
  fi
fi

# print settings
if [ "${VERBOSE}" = true ] ; then
  settings
fi

# load environment for this system/compiler combination
ENVFILE="${LISHYDRO_DIR}/env/${SYSTEM}.${COMPILER}"
if [ ! -f "${ENVFILE}" ]; then
  printf "ERROR: environment file does not exist for ${SYSTEM}.${COMPILER}\n"
  printf "Please select one of the following configurations\n"
  for f in "${LISHYDRO_DIR}/env"/*
  do
    printf "  $(basename $f)\n"
  done
  printf "\n"
  exit 1
fi
source ${ENVFILE}

# if build directory already exists then exist
if [ "${CLEAN}" = true ]; then
  printf "Remove build directory\n"
  printf "  BUILD_DIR=${BUILD_DIR}\n"
  printf "\n"
  rm -rf ${BUILD_DIR}
elif [ "${CONTINUE}" = true ]; then
  printf "Continue build in directory\n"
  printf "  BUILD_DIR=${BUILD_DIR}\n"
  printf "\n"
else
  if [ -d "${BUILD_DIR}" ]; then
    printf "ERROR: Build directory exists\n"
    printf "  use option --continue to continue existing build\n"
    printf "  use option --clean to remove existing build\n"
    exit 1
  fi
fi
mkdir -p ${BUILD_DIR}

# cmake settings
CMAKE_SETTINGS="-DCMAKE_BUILD_TYPE=${BUILD_TYPE}"
if [ ! -z "${COMPONENTS}" ]; then
  CMAKE_SETTINGS="${CMAKE_SETTINGS} -DINCLUDE_COMPONENTS=${COMPONENTS}"
fi

# make settings
MAKE_SETTINGS=""
if [ "${VERBOSE}" = true ]; then
  MAKE_SETTINGS="VERBOSE=1"
fi

# build the code
cd ${BUILD_DIR}
cmake ${LISHYDRO_DIR} -DCMAKE_INSTALL_PREFIX=${INSTALL_DIR} ${CMAKE_SETTINGS}
make -j ${BUILD_JOBS:-4} ${MAKE_SETTINGS}
make install

exit 0
