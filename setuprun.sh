#!/bin/bash

# usage instructions
usage () {
  printf "Usage: $0 USECASE [OPTIONS]...\n"
  printf "\n"
  printf "USECASE\n"
  printf "  preconfigured use case\n"
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
  printf "  USECASE=${USECASE}\n"
  printf "  DIR_APP=${DIR_APP}\n"
  printf "  DIR_SETNGS=${DIR_SETNGS}\n"
  printf "  DIR_RUNCFG=${DIR_RUNCFG}\n"
  printf "  DIR_RUNSCP=${DIR_RUNSCP}\n"
  printf "  SYSTEM=${SYSTEM}\n"
  printf "  MYCOMPILER=${MYCOMPILER}\n"
  printf "  DATA_ROOT=${DATA_ROOT}\n"
  printf "  BATCH_SYS=${BATCH_SYS}\n"
  printf "  CPPERNODE=${CPPERNODE}\n"
  printf "  VERBOSE=${VERBOSE}\n"
  printf "\n"
}

# find system name
find_system () {
    local sysname=`hostname`
    sysname="${sysname//[[:digit:]]/}"
    echo "$sysname"
}

list_usecases () {
  local file_list=`find $DIR_SETNGS -maxdepth 1 -type f -name '*' | sort`
  printf "Available use cases:\n"
  for file in $file_list; do
    file=`basename $file`
    printf "\t$file\n"
  done
  printf "\n"
}

# default settings
LISHYDRO_DIR=$(cd "$(dirname "$(readlink -f -n "${BASH_SOURCE[0]}" )" )" && pwd -P)
DIR_APP=${LISHYDRO_DIR}/src/driver
DIR_SETNGS="${LISHYDRO_DIR}/usecases/runsettings"
DIR_RUNCFG="${LISHYDRO_DIR}/usecases/runconfig"
DIR_RUNSCP="${LISHYDRO_DIR}/usecases/runscripts"
USECASE=""
SYSTEM=""
MYCOMPILER=""
DATA_ROOT=""
BATCH_SYS=""
CPPERNODE=""
VERBOSE=false
ERROR=0

# required arguments
if [ "$#" -lt 1 ]; then
  printf "ERROR: Missing USECASE.\n"; list_usecases; usage; exit 1
elif [ "$1" = "--help" ] || [ "$1" = "-h" ]; then
  usage; exit 0
fi

# process arguments
#while :; do
while [ ! -z "$1" ]; do
  case $1 in
    --help|-h) usage; exit 0 ;;
    --system=?*) SYSTEM=${1#*=} ;;
    --system) printf "ERROR: $1 requires an argument.\n"; usage; exit 1 ;;
    --system=) printf "ERROR: $1 requires an argument.\n"; usage; exit 1 ;;
    --compiler=?*) MYCOMPILER=${1#*=} ;;
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
    *) if [ -z "${USECASE}" ] ; then
         USECASE=$1
       else
         printf "ERROR: USECASE defined multiple times.\n"; usage; exit 1
       fi
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

# automatically determine data root
if [ -z "${DATA_ROOT}" ] ; then
  if [ "${SYSTEM}" = "discover" ]; then
    DATA_ROOT="/discover/nobackup/projects/nu-wrf/lishydro/data"
  elif [ "${SYSTEM}" = "cheyenne" ]; then
    DATA_ROOT="/glade/p/ral/hap/drosen/projects/LISHydro/data"
  else
    printf "ERROR: no default compiler for ${SYSTEM}\n"
    printf "\n"
    exit 1
  fi
fi

# automatically determine batch system
if [ -z "${BATCH_SYS}" ] ; then
  if [ "${SYSTEM}" = "discover" ]; then
    BATCH_SYS="sbatch"
  elif [ "${SYSTEM}" = "cheyenne" ]; then
    BATCH_SYS="qsub"
  else
    printf "ERROR: no default compiler for ${SYSTEM}\n"
    printf "\n"
    exit 1
  fi
fi

# automatically determine cores per node
if [ -z "${CPPERNODE}" ] ; then
  if [ "${SYSTEM}" = "discover" ]; then
    CPPERNODE=28
  elif [ "${SYSTEM}" = "cheyenne" ]; then
    CPPERNODE=36
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

# Include usecase settings
USECASE_SETTINGS="$DIR_SETNGS/$USECASE"
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
  REMAINDER=`expr $TASKS % $CPPERNODE`
  if [ $REMAINDER -gt 0 ]; then
    NODES=`expr $TASKS / $CPPERNODE + 1`
  else
    NODES=`expr $TASKS / $CPPERNODE`
  fi
fi

# Check RUNCONFIG variable then set LISHYDRO_RUNCONFIG template file
if [ -z $RUNCONFIG ]; then
  echo "ERROR: RUNCONFIG variable is undefined. Check [$USECASE_SETTINGS]"
  exit 1
fi
LISHYDRO_RUNCONFIG="$DIR_RUNCFG/lishydro.runconfig.$RUNCONFIG"
if [ ! -f $LISHYDRO_RUNCONFIG ]; then
  echo "ERROR: LISHYDRO_RUNCONFIG file is missing [$LISHYDRO_RUNCONFIG]"
  exit 1
fi

# Set LISHYDRO_RUNSCRIPT template file
LISHYDRO_RUNSCRIPT="$DIR_RUNSCP/run.${SYSTEM}.${MYCOMPILER}"
if [ ! -f $LISHYDRO_RUNSCRIPT ]; then
  echo "ERROR: LISHYDRO_RUNSCRIPT file is missing [$LISHYDRO_RUNSCRIPT]"
  exit 1
fi

# Check LISHYDRO_EXE
LISHYDRO_EXE=${DIR_APP}/LISHydroApp
if [ ! -f $LISHYDRO_EXE ]; then
  echo "ERROR: LISHydroApp executable does not exist."
  exit 1
fi

# Set RUNDIR and create RUNDIR
RUNDIR=${LISHYDRO_DIR}/run/$USECASE
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
      cp $data_hyd_member/WRFHYDRO_PARMS/CHANPARM.TBL $rundir_member
      ln -sf $data_hyd_member/WRFHYDRO_DOMAIN  $rundir_member/WRFHYDRO_DOMAIN
      ln -sf $data_hyd_member/WRFHYDRO_FORCING $rundir_member/WRFHYDRO_FORCING
      ln -sf $data_hyd_member/WRFHYDRO_PARMS   $rundir_member/WRFHYDRO_PARMS
      ln -sf $data_hyd_member/WRFHYDRO_RESTART $rundir_member/WRFHYDRO_RESTART
    done
    if [ -z "$ensemble" ]; then
      cp $DATA_HYD/namelist.hrldas $RUNDIR
      cp $DATA_HYD/hydro.namelist  $RUNDIR
      cp $DATA_HYD/WRFHYDRO_PARMS/CHANPARM.TBL $RUNDIR
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
fi

# Copy executable to RUNDIR
cp $LISHYDRO_EXE $RUNDIR/.

# Copy runconfig and process use case settings
cp $LISHYDRO_RUNCONFIG $RUNDIR/lishydro.runconfig
settings=`grep -ow -e "__.*__" $RUNDIR/lishydro.runconfig`
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
    sed -i "s/$setting/$value/g" $RUNDIR/lishydro.runconfig
  fi
done
if [ $ERROR -ne 0 ]; then
  echo "ERROR: Processing failed [$LISHYDRO_RUNCONFIG]"
  exit $ERROR
fi

# Copy batch script and process use case settings
cp $LISHYDRO_RUNSCRIPT $RUNDIR/run.sh
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
  echo "ERROR: Processing failed [$LISHYDRO_RUNSCRIPT]"
  exit $ERROR
fi

echo "Run directory set up in: $RUNDIR"
echo "To execute:"
echo "$ cd $RUNDIR"
echo "$ $BATCH_SYS run.sh"

