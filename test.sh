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
  printf "  --build-dir=BUILD_DIR\n"
  printf "      build directory\n"
  printf "  --tests-regex=REGEX, -R=REGEX\n"
  printf "      run tests matching the specified regular expression\n"
  printf "  --label-regex=REGEX, -L=REGEX\n"
  printf "      run tests with labels matching the specified regular expression\n"
  printf "  --verbose, -v\n"
  printf "      build with verbose output\n"
  printf "  --help, -h\n"
  printf "      print this help message\n"
  printf "\n"
}

# print settings
settings () {
  printf "Settings:\n"
  printf "\n"
  printf "  NLC_DIR=${NLC_DIR}\n"
  printf "  BUILD_DIR=${BUILD_DIR}\n"
  printf "  TESTS_REGEX=${TESTS_REGEX}\n"
  printf "  LABEL_REGEX=${LABEL_REGEX}\n"
  printf "  SYSTEM=${SYSTEM}\n"
  printf "  ENV_AUTO=${ENV_AUTO}\n"
  printf "  VERBOSE=${VERBOSE}\n"
  printf "\n"
}

# default settings
NLC_DIR=$(cd "$(dirname "$(readlink -f -n "${BASH_SOURCE[0]}" )" )" && pwd -P)
BUILD_DIR=${NLC_DIR}/build
SYSTEM=""
ENV_DIR="${NLC_DIR}/env"
ENV_AUTO=false
VERBOSE=false
TESTS_REGEX=""
LABEL_REGEX=""

# required arguments
if [ "$1" = "--help" ] || [ "$1" = "-h" ]; then
  usage
  exit 0
fi

# process arguments
while :; do
  case $1 in
    --help|-h) usage; exit 0 ;;
    --build-dir=?*) BUILD_DIR=${1#*=} ;;
    --build-dir) printf "ERROR: $1 requires an argument.\n"; usage; exit 1 ;;
    --build-dir=) printf "ERROR: $1 requires an argument.\n"; usage; exit 1 ;;
    --tests-regex=?*|-R=?*) TESTS_REGEX="${1#*=}" ;;
    --tests-regex|-R) printf "ERROR: $1 requires an argument.\n"; usage; exit 1 ;;
    --tests-regex=|-R=) printf "ERROR: $1 requires an argument.\n"; usage; exit 1 ;;
    --label-regex=?*|-L=?*) LABEL_REGEX="${1#*=}" ;;
    --label-regex|-L) printf "ERROR: $1 requires an argument.\n"; usage; exit 1 ;;
    --label-regex=|-L=) printf "ERROR: $1 requires an argument.\n"; usage; exit 1 ;;
    --verbose|-v) VERBOSE=true ;;
    --verbose=?*) printf "ERROR: $1 argument ignored.\n"; usage; exit 1 ;;
    --verbose=) printf "ERROR: $1 argument ignored.\n"; usage; exit 1 ;;
    -?*) printf "ERROR: Unknown option $1\n"; usage; exit 1 ;;
    *) break
  esac
  shift
done

set -eu

source scripts/setupenv.sh

# load saved configuration if it exists
if [ -f "${NLC_DIR}/.nlc_config.sh" ]; then
  source "${NLC_DIR}/.nlc_config.sh"
else
  printf "ERROR: NLC has not been configured.\n"
  printf "  Please run './configure.sh' before building.\n"
  exit 1
fi

# auto modulefile
if [ "${ENV_AUTO}" = true ] ; then
  auto_environment ${SYSTEM} ${ENV_DIR}
fi

# print settings
if [ "${VERBOSE}" = true ] ; then
  settings
fi

# check for BUILD_DIR
if [ ! -d "${BUILD_DIR}" ]; then
  printf "ERROR: Build directory missing\n"
  printf "  Please run './build.sh' before testing.\n"
  exit 1
fi

# ctest settings
CTEST_SETTINGS=""
if [ "${VERBOSE}" = true ]; then
  CTEST_SETTINGS="VERBOSE=1"
fi
if [ -n "${TESTS_REGEX}" ]; then
  CTEST_SETTINGS="${CTEST_SETTINGS} -R ${TESTS_REGEX}"
fi
if [ -n "${LABEL_REGEX}" ]; then
  CTEST_SETTINGS="${CTEST_SETTINGS} -L ${LABEL_REGEX}"
fi

# run tests
cd ${BUILD_DIR}
ctest --test-dir ${BUILD_DIR} -L nlc_test ${CTEST_SETTINGS}
exit 0

