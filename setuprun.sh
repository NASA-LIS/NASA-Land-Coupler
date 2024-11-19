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
  printf "Usage: $0 USECASE [OPTIONS]...\n"
  printf "\n"
  printf "USECASE\n"
  printf "  preconfigured use case\n"
  printf "\n"
  printf "OPTIONS\n"
  printf "  --env-auto-off\n"
  printf "      do not load preconfigured environment based on system\n"
  printf "  --dir-app=PATH\n"
  printf "      application directory\n"
  printf "  --data-root=PATH\n"
  printf "      root directory for input data\n"
  printf "  --batch-sys=BATCH_SYS\n"
  printf "      batch system (e.g. 'sbatch', 'qsub')\n"
  printf "  --cpu-per-node=CPPERNODE\n"
  printf "      CPUs per node\n"
  printf "  --list-usecases, -l\n"
  printf "      list usecases and exit\n"
  printf "  --help, -h\n"
  printf "      print usage and exit\n"
  printf "  --verbose, -v\n"
  printf "      build with verbose output\n"
  printf "\n"
}

# print settings
settings () {
  printf "Settings:\n"
  printf "\n"
  printf "  NLC_DIR=${NLC_DIR}\n"
  printf "  USECASE=${USECASE}\n"
  printf "  DIR_APP=${DIR_APP}\n"
  printf "  DIR_USECASES=${DIR_USECASES}\n"
  printf "  DIR_RUNCFG=${DIR_RUNCFG}\n"
  printf "  DIR_RUNSCP=${DIR_RUNSCP}\n"
  printf "  SYSTEM=${SYSTEM}\n"
  printf "  ENV_AUTO=${ENV_AUTO}\n"
  printf "  DATA_ROOT=${DATA_ROOT}\n"
  printf "  BATCH_SYS=${BATCH_SYS}\n"
  printf "  CPPERNODE=${CPPERNODE}\n"
  printf "  VERBOSE=${VERBOSE}\n"
  printf "\n"
}

list_usecases () {
  local file_list=`find $DIR_USECASES -maxdepth 1 -type f -name '*' | sort`
  printf "Available use cases:\n"
  for file in $file_list; do
    file=`basename $file`
    printf "\t$file\n"
  done
  printf "\n"
}

# default settings
NLC_DIR=$(cd "$(dirname "$(readlink -f -n "${BASH_SOURCE[0]}" )" )" && pwd -P)
DIR_APP=${NLC_DIR}/install
DIR_USECASES="${NLC_DIR}/usecases"
DIR_RUNCFG="${NLC_DIR}/templates/runconfig"
DIR_RUNSCP="${NLC_DIR}/templates/runscripts"
USECASE=""
SYSTEM=""
ENV_DIR="${NLC_DIR}/env"
ENV_AUTO=true
DATA_ROOT=""
BATCH_SYS=""
CPPERNODE=""
VERBOSE=false
ERROR=0

# required arguments
if [ "$#" -lt 1 ]; then
  printf "ERROR: Missing USECASE.\n"; usage; list_usecases; exit 1
elif [ "$1" = "--help" ] || [ "$1" = "-h" ]; then
  usage; exit 0
fi

# process arguments
#while :; do
while [ ! -z "$1" ]; do
  case $1 in
    --help|-h) usage; exit 0 ;;
    --list-usecases|-l) list_usecases; exit 0 ;;
    --env-auto-off) ENV_AUTO=true ;;
    --env-auto-off=?*) printf "ERROR: $1 argument ignored.\n"
                   usage; exit 1 ;;
    --env-auto-off=) printf "ERROR: $1 argument ignored.\n"
                   usage; exit 1 ;;
    --dir-app=?*) DIR_APP=${1#*=} ;;
    --dir-app|--dir-app=) printf "ERROR: $1 requires an argument.\n"
                          usage; exit 1 ;;
    --data-root=?*) DATA_ROOT=${1#*=} ;;
    --data-root|--data-root=) printf "ERROR: $1 requires an argument.\n"
                              usage; exit 1 ;;
    --batch-sys=?*) BATCH_SYS=${1#*=} ;;
    --batch-sys|--batch-sys=) printf "ERROR: $1 requires an argument.\n"
                              usage; exit 1 ;;
    --cpu-per-node=?*) CPPERNODE=${1#*=} ;;
    --cpu-per-node|--cpu-per-node=) printf "ERROR: $1 requires an argument.\n"
                                    usage; exit 1 ;;
    --verbose|-v) VERBOSE=true ;;
    -?*) printf "ERROR: Unknown option $1\n"; usage; exit 1 ;;
    *) if [ -z "${USECASE}" ] ; then
         USECASE=$1
       else
         printf "ERROR: USECASE defined multiple times.\n"; usage; exit 1
       fi
  esac
  shift
done

set -eu

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

# Include usecase settings
USECASE_SETTINGS="$DIR_USECASES/$USECASE"
if [ ! -f $USECASE_SETTINGS ]; then
  echo "ERROR: Use case settings file is missing [$USECASE_SETTINGS]"
  list_usecases; exit 1
fi
source $USECASE_SETTINGS >& /dev/null
if [ $? -ne 0 ]; then
  echo "ERROR: Use case settings file must be Bash [$USECASE_SETTINGS]"
  exit 1
fi

# Calculate Nodes Needed
if [ -z ${TASKS+x} ]; then
  echo "ERROR: TASKS must be set in $USECASE_SETTINGS"; exit 1
else
  REMAINDER=$(( $TASKS % $CPPERNODE ))
  if [ $REMAINDER -gt 0 ]; then
    NODES=$(( $TASKS / $CPPERNODE + 1 ))
  else
    NODES=$(( $TASKS / $CPPERNODE ))
  fi
fi

# Check RUNCONFIG variable then set NLC_RUNCONFIG template file
if [ -z $RUNCONFIG ]; then
  echo "ERROR: RUNCONFIG variable is undefined. Check [$USECASE_SETTINGS]"
  exit 1
fi
NLC_RUNCONFIG="$DIR_RUNCFG/nlc.runconfig.$RUNCONFIG"
if [ ! -f $NLC_RUNCONFIG ]; then
  echo "ERROR: NLC_RUNCONFIG file is missing [$NLC_RUNCONFIG]"
  exit 1
fi

# Set NLC_RUNSCRIPT template file
NLC_RUNSCRIPT="$DIR_RUNSCP/${SYSTEM}/run.${COMPILER_VERS}"
if [ ! -f $NLC_RUNSCRIPT ]; then
  echo "ERROR: NLC_RUNSCRIPT file is missing [$NLC_RUNSCRIPT]"
  exit 1
fi

# Check NLC_EXE
NLC_EXE=${DIR_APP}/NLC.exe
if [ ! -f $NLC_EXE ]; then
  echo "ERROR: NLC.exe executable does not exist."
  exit 1
fi

# Check NLC_FD
NLC_FD=${DIR_APP}/fd_nlc.yaml
if [ ! -f $NLC_FD ]; then
  echo "ERROR: fd_nlc.yaml field dictionary does not exist."
  exit 1
fi

# Set RUNDIR and create RUNDIR
RUNDIR=${NLC_DIR}/run/$USECASE
if [ -d $RUNDIR ]; then
    echo "ERROR: Run directory already exists [$RUNDIR]"
    echo "Please delete or move and try again"
    exit 1
fi
mkdir -p $RUNDIR

# WRF-Hydro setup
if [[ $RUNCONFIG == *"hyd"* ]]; then
  DATA_HYD=$DATA_ROOT/WRFHydro/$USECASE
  if [ -d $DATA_HYD ]; then
    ensemble=`find $DATA_HYD -name 'HYD-*'`
    for data_hyd_member in ${ensemble}; do
      member=`basename $data_hyd_member`
      rundir_member=$RUNDIR/$member
      mkdir -p $rundir_member
      cp $data_hyd_member/namelist.hrldas $rundir_member
      cp $data_hyd_member/hydro.namelist  $rundir_member
      cp $data_hyd_member/WRFHYDRO_PARMS/*.TBL $rundir_member
      ln -sf $data_hyd_member/WRFHYDRO_DOMAIN  $rundir_member/WRFHYDRO_DOMAIN
      ln -sf $data_hyd_member/WRFHYDRO_FORCING $rundir_member/WRFHYDRO_FORCING
      ln -sf $data_hyd_member/WRFHYDRO_PARMS   $rundir_member/WRFHYDRO_PARMS
      ln -sf $data_hyd_member/WRFHYDRO_RESTART $rundir_member/WRFHYDRO_RESTART
    done
    if [ -z "$ensemble" ]; then
      cp $DATA_HYD/namelist.hrldas $RUNDIR
      cp $DATA_HYD/hydro.namelist  $RUNDIR
      cp $DATA_HYD/WRFHYDRO_PARMS/*.TBL $RUNDIR
      ln -sf $DATA_HYD/WRFHYDRO_DOMAIN  $RUNDIR/WRFHYDRO_DOMAIN
      ln -sf $DATA_HYD/WRFHYDRO_FORCING $RUNDIR/WRFHYDRO_FORCING
      ln -sf $DATA_HYD/WRFHYDRO_PARMS   $RUNDIR/WRFHYDRO_PARMS
      ln -sf $DATA_HYD/WRFHYDRO_RESTART $RUNDIR/WRFHYDRO_RESTART
    fi
  else
    echo "ERROR: DATA_HYD directory not found [$DATA_HYD]"
    exit 1
  fi
fi

# LIS Setup
if [[ $RUNCONFIG == *"lnd"* ]]; then
  DATA_LND=$DATA_ROOT/LIS/$USECASE
  if [ -d $DATA_LND ]; then
    cp $DATA_LND/lis.config $RUNDIR
    ln -s $DATA_LND/LIS_DOMAIN $RUNDIR/LIS_DOMAIN
    ln -s $DATA_LND/LIS_INPUTS $RUNDIR/LIS_INPUTS
    ln -s $DATA_LND/LIS_RSTRT $RUNDIR/LIS_RSTRT
    ln -s $DATA_LND/LIS_FORCING $RUNDIR/LIS_FORCING
    if [ -d $DATA_LND/NOAH33_PARMS ]; then
      ln -s $DATA_LND/NOAH33_PARMS $RUNDIR/NOAH33_PARMS
    fi
    if [ -d $DATA_LND/NOAHMP36_PARMS ]; then
      ln -s $DATA_LND/NOAHMP36_PARMS $RUNDIR/NOAHMP36_PARMS
    fi
    if [ -d $DATA_LND/NOAHMP401_PARMS ]; then
      ln -s $DATA_LND/NOAHMP401_PARMS $RUNDIR/NOAHMP401_PARMS
    fi
  else
    echo "ERROR: DATA_LND directory not found [$DATA_LND]"
    exit 1
  fi
  # check lis.config
  metfsrc=$(grep "Met forcing sources:" $RUNDIR/lis.config || echo "")
  if [[ "${metfsrc}" == *"NLDAS2"* ]]; then
    if [[ "${metfsrc}" != *"NLDAS2 grib"* ]] && \
       [[ "${metfsrc}" != *"NLDAS2 netcdf"* ]]; then
      printf "\e[31mERROR: lis.config error in [Met forcing sources]\n"
      printf "  [NLDAS2] must be replaced with [NLDAS2 grib] or [NLDAS2 netcdf]"
      printf "\e[0m\n\n"
    fi
  fi
fi

# Copy executable to RUNDIR
cp $NLC_EXE $RUNDIR/.
cp $NLC_FD  $RUNDIR/.

# Copy runconfig and process use case settings
cp $NLC_RUNCONFIG $RUNDIR/nlc.runconfig
settings=`grep -ow -e "__.*__" $RUNDIR/nlc.runconfig`
ERROR=0
for setting in ${settings}; do
  envvar=${setting//__/""}
  if [ -z ${!envvar+x} ]; then
    echo "ERROR: "$envvar" must be set in $USECASE_SETTINGS"
    ERROR=1
    continue
  else
    value=${!envvar}
    value=`eval echo ${value}`
    value="${value//\//\\\/}"
    sed -i "s/$setting/$value/g" $RUNDIR/nlc.runconfig
  fi
done
if [ $ERROR -ne 0 ]; then
  echo "ERROR: Processing failed [$NLC_RUNCONFIG]"
  exit $ERROR
fi

# Copy batch script and process use case settings
cp $NLC_RUNSCRIPT $RUNDIR/run.sh
settings=`grep -ow -e "__.*__" $RUNDIR/run.sh`
ERROR=0
for setting in ${settings}; do
  envvar=${setting//__/""}
  if [ -z ${!envvar+x} ]; then
    echo "ERROR: "$envvar" must be set in $USECASE_SETTINGS"
    ERROR=1
    continue
  else
    value=${!envvar}
    value=`eval echo ${value}`
    sed -i "s/$setting/$value/g" $RUNDIR/run.sh
  fi
done
if [ $ERROR != 0 ]; then
  echo "ERROR: Processing failed [$NLC_RUNSCRIPT]"
  exit $ERROR
fi

echo "Run directory set up in: $RUNDIR"
echo "To execute:"
echo "$ cd $RUNDIR"
echo "$ $BATCH_SYS run.sh"

