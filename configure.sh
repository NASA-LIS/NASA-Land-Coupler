#!/bin/bash
#-----------------------BEGIN NOTICE -- DO NOT EDIT-----------------------------
# NASA Goddard Space Flight Center
# NASA Land Coupler (NLC)
# Version 0.5
#
# Copyright (c) 2022 United States Government as represented by the
# Administrator of the National Aeronautics and Space Administration.
# All Rights Reserved.
# Licensed under Apache License 2.0.
#-------------------------END NOTICE -- DO NOT EDIT-----------------------------
# usage instructions
usage () {
  printf "Usage: $0 [OPTIONS]...\n"
  printf "\n"
  printf "OPTIONS\n"
  printf "  --env-auto-off\n"
  printf "      do not load preconfigured environment based on system\n"
  printf "  --build-type=BUILD_TYPE\n"
  printf "      build type; valid options are 'debug', 'release'.\n"
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
  printf "  ENV_AUTO=${ENV_AUTO}\n"
  printf "  BUILD_TYPE=${BUILD_TYPE}\n"
  printf "  INTERACTIVE=${INTERACTIVE}\n"
  printf "  VERBOSE=${VERBOSE}\n"
  printf "\n"
}

# default settings
NLC_DIR=$(cd "$(dirname "$(readlink -f -n "${BASH_SOURCE[0]}" )" )" && pwd -P)
SYSTEM=""
ENV_DIR="${NLC_DIR}/env"
ENV_AUTO=true
BUILD_TYPE="Release"
INTERACTIVE=true
VERBOSE=false
RC=0

# process arguments
while :; do
  case $1 in
    --help|-h) usage; exit 0 ;;
    --env-auto-off) ENV_AUTO=false ;;
    --env-auto-off=?*) printf "ERROR: $1 argument ignored.\n"; usage; exit 1 ;;
    --env-auto-off=) printf "ERROR: $1 argument ignored.\n"; usage; exit 1 ;;
    --build-type=?*) BUILD_TYPE=${1#*=} ;;
    --build-type) printf "ERROR: $1 requires an argument.\n"; usage; exit 1 ;;
    --build-type=) printf "ERROR: $1 requires an argument.\n"; usage; exit 1 ;;
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

source scripts/setupenv.sh

# automatically determine system
if [ -z "${SYSTEM}" ] ; then
  SYSTEM=$(find_system)
fi

# auto modulefile
export NLC_DIR="${NLC_DIR}"
if [ "${ENV_AUTO}" = true ] ; then
  auto_environment ${SYSTEM} ${ENV_DIR}
fi

# print settings
if [ "${VERBOSE}" = true ] ; then
  settings
fi

printf "*************************************************\n"
printf "***          LIS BUILD CONFIGURATION          ***\n"
printf "*************************************************\n"
if [ ! -f "$NLC_DIR/src/LISF/lis/configure" ]; then
  printf "\e[31mERROR: LIS configure file is missing\e[0m\n"
  printf "       \tgit submodule init\n"
  printf "       \tgit submodule update\n"
  exit 1
else
  cd $NLC_DIR/src/LISF/lis
fi
if [ "${INTERACTIVE}" = true ]; then
  ./configure; RC=$?
else
  case ${BUILD_TYPE} in
    debug | DEBUG | Debug)
      echo -ne '\n-2' | ./configure; RC=$?
      ;;
    *)
      echo '' | ./configure; RC=$?
      ;;
  esac
fi
if [ ! -f "make/configure.lis" ]; then
  RC=1
fi
printf "\n"
printf "\e[33mWARNING: VIC will be disabled in user.cfg\e[0m\n"
printf "VIC.4.1.1: Off\n" >  "$NLC_DIR/src/LISF/lis/make/user.cfg"
printf "VIC.4.1.2: Off\n" >> "$NLC_DIR/src/LISF/lis/make/user.cfg"
printf "\n"

if [ $RC -ne 0 ]; then
  printf "\e[31mERROR: configuration failed\e[0m\n"
  exit 1
fi

printf "\e[32mSUCCESS: configuration complete\e[0m\n"
exit 0
