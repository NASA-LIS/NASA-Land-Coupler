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
  printf "  --interactive\n"
  printf "      run interactive configuration\n"
  printf "  --verbose, -v\n"
  printf "      build with verbose output\n"
  printf "\n"
}

# print settings
settings () {
  printf "Settings:\n"
  printf "\n"
  printf "  LISHYDRO_DIR=${LISHYDRO_DIR}\n"
  printf "  SYSTEM=${SYSTEM}\n"
  printf "  COMPILER=${MYCOMPILER}\n"
  printf "  INTERACTIVE=${INTERACTIVE}\n"
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
SYSTEM=""
MYCOMPILER=""
INTERACTIVE=false
VERBOSE=false

# process arguments
while :; do
  case $1 in
    --help|-h) usage; exit 0 ;;
    --system=?*) SYSTEM=${1#*=} ;;
    --system) printf "ERROR: $1 requires an argument.\n"; usage; exit 1 ;;
    --system=) printf "ERROR: $1 requires an argument.\n"; usage; exit 1 ;;
    --compiler=?*) MYCOMPILER=${1#*=} ;;
    --compiler) printf "ERROR: $1 requires an argument.\n"; usage; exit 1 ;;
    --compiler=) printf "ERROR: $1 requires an argument.\n"; usage; exit 1 ;;
    --interactive) INTERACTIVE=true ;;
    --interactive=?*) printf "ERROR: $1 argument ignored.\n"; usage; exit 1 ;;
    --interactive=) printf "ERROR: $1 argument ignored.\n"; usage; exit 1 ;;
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
if [ -z "${MYCOMPILER}" ] ; then
  if [ "${SYSTEM}" = "discover" ]; then
    MYCOMPILER="intel.19.1.0"
  elif [ "${SYSTEM}" = "cheyenne" ]; then
    MYCOMPILER="intel.17.0.1"
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
ENVFILE="${LISHYDRO_DIR}/env/${SYSTEM}.${MYCOMPILER}"
if [ ! -f "${ENVFILE}" ]; then
  printf "ERROR: environment file does not exist for ${SYSTEM}.${MYCOMPILER}\n"
  printf "Please select one of the following configurations\n"
  for f in "${LISHYDRO_DIR}/env"/*
  do
    printf "  $(basename $f)\n"
  done
  printf "\n"
  exit 1
fi
source ${ENVFILE}
printf "*************************************************\n"
printf "***          LIS BUILD CONFIGURATION          ***\n"
printf "*************************************************\n"
cd $LISHYDRO_DIR/src/LISF/lis
if [ "${INTERACTIVE}" = true ]; then
  ./configure
else
  echo "" | ./configure
fi
if [ ! -f "make/configure.lis" ]; then
  printf "ERROR: LIS configuration failed\n"
  exit 1
fi
printf "\n"

printf "*************************************************\n"
printf "***       WRFHYDRO BUILD CONFIGURATION        ***\n"
printf "*************************************************\n"
cd $LISHYDRO_DIR/src/wrf_hydro_nwm/trunk/NDHMS
if [ "${INTERACTIVE}" = true ]; then
  ./configure
elif [[ "${MYCOMPILER}" == *"cray"* ]]; then
  echo "6" | ./configure
elif [[ "${MYCOMPILER}" == *"intel"* ]]; then
  echo "3" | ./configure
elif [[ "${MYCOMPILER}" == *"gnu"* ]]; then
  echo "2" | ./configure
elif [[ "${MYCOMPILER}" == *"pgi"* ]]; then
  echo "1" | ./configure
else
  printf "ERROR: compiler unknown ${MYCOMPILER}\n" 
  exit 1
fi
if [ ! -f "macros" ]; then
  printf "ERROR: WRFHYDRO configuration failed\n"
  exit 1
fi
printf "\n"

exit 0
