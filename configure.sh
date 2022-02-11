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
  printf "  --auto\n"
  printf "      run non-interactive configuration\n"
  printf "  --verbose, -v\n"
  printf "      build with verbose output\n"
  printf "\n"
}

# print settings
settings () {
  printf "Settings:\n"
  printf "\n"
  printf "  NLC_DIR=${NLC_DIR}\n"
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
NLC_DIR=$(cd "$(dirname "$(readlink -f -n "${BASH_SOURCE[0]}" )" )" && pwd -P)
SYSTEM=""
MYCOMPILER=""
INTERACTIVE=true
VERBOSE=false
RC=0

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
    --auto) INTERACTIVE=false ;;
    --auto=?*) printf "ERROR: $1 argument ignored.\n"; usage; exit 1 ;;
    --auto=) printf "ERROR: $1 argument ignored.\n"; usage; exit 1 ;;
    --verbose|-v) VERBOSE=true ;;
    --verbose=?*) printf "ERROR: $1 argument ignored.\n"; usage; exit 1 ;;
    --verbose=) printf "ERROR: $1 argument ignored.\n"; usage; exit 1 ;;
    -?*) printf "ERROR: Unknown option $1\n"; usage; exit 1 ;;
    *) break
  esac
  shift
done

set -u

# automatically determine system
if [ -z "${SYSTEM}" ] ; then
  SYSTEM=$(find_system)
fi

# automatically determine compiler
if [ -z "${MYCOMPILER}" ] ; then
  if [ "${SYSTEM}" = "discover" ]; then
    MYCOMPILER="intel.19.1.3"
  elif [ "${SYSTEM}" = "cheyenne" ]; then
    MYCOMPILER="intel.19.1.1"
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
ENVFILE="${NLC_DIR}/env/${SYSTEM}.${MYCOMPILER}"
if [ ! -f "${ENVFILE}" ]; then
  printf "ERROR: environment file does not exist for ${SYSTEM}.${MYCOMPILER}\n"
  printf "Please select one of the following configurations\n"
  for f in "${NLC_DIR}/env"/*
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
if [ ! -f "$NLC_DIR/src/LISF/lis/configure" ]; then
  printf "ERROR: LIS configure file is missing\n"
  printf "       \tgit submodule init\n" 
  printf "       \tgit submodule update\n"
  exit 1
else
  cd $NLC_DIR/src/LISF/lis
fi
if [ "${INTERACTIVE}" = true ]; then
  ./configure; RC=$?
else
  echo "" | ./configure; RC=$?
fi
if [ ! -f "make/configure.lis" ]; then
  RC=1
fi
printf "\n"

printf "*************************************************\n"
printf "***       WRFHYDRO BUILD CONFIGURATION        ***\n"
printf "*************************************************\n"
if [ ! -f "$NLC_DIR/src/wrf_hydro_nwm/trunk/NDHMS/configure" ]; then
  printf "ERROR: WRFHYDRO configure file is missing\n"
  printf "       \tgit submodule init\n"
  printf "       \tgit submodule update\n"
  exit 1
else
  cd $NLC_DIR/src/wrf_hydro_nwm/trunk/NDHMS
fi
if [ "${INTERACTIVE}" = true ]; then
  ./configure; RC=$?
elif [[ "${MYCOMPILER}" == *"cray"* ]]; then
  echo "6" | ./configure; RC=$?
elif [[ "${MYCOMPILER}" == *"intel"* ]]; then
  echo "3" | ./configure; RC=$?; echo $RC
elif [[ "${MYCOMPILER}" == *"gnu"* ]]; then
  echo "2" | ./configure; RC=$?
elif [[ "${MYCOMPILER}" == *"pgi"* ]]; then
  echo "1" | ./configure; RC=$?
else
  printf "ERROR: compiler unknown ${MYCOMPILER}\n" 
  exit 1
fi
if [ ! -f "macros" ]; then
  RC=1
fi
printf "\n"

if [ $RC -ne 0 ]; then
  printf "ERROR: configuration failed.\n"
  exit 1
fi

exit 0
