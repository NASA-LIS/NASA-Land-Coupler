#!/bin/bash

INF_OPT=":hap:d:"
INF_USG="Usage: $0 [-h] [-a] [-p PREFIX] [-d DIRECTORY] RUN_DIRECTORY"
INF_HLP="Help:"

OPT_RUNDIR="missing"
OPT_OUTDIR="plots"
OPT_OUTPFX="PLOT"
OPT_ALLFIL=false

PTN_LISHST="LIS_HIST*"
PTN_HYDCRT="*.CHRTOUT_DOMAIN1"

function check_command { 
  local COMMAND=$1
  which ${COMMAND} >/dev/null 2>&1 
  if [ $? -ne 0 ]; then
    echo "${COMMAND}: command not found" 1>&2
    exit 1
  fi
}

function list_directories {
  local LST_DIRTRY=(`find ${LISHYDRO_DIR}/run -maxdepth 1 -mindepth 1 -type d`)
  echo "Available directories in ${LISHYDRO_DIR}/run:"
  for DIRECTORY in ${LST_DIRTRY[@]}; do
    echo "     $(basename ${DIRECTORY})"
  done
}

function plot_lis_history {
  local FIL_LISHST="$1"
  local FIL_BSENME=$(basename ${FIL_LISHST})
  local FIL_DRYNME=$(dirname ${FIL_LISHST})
  ferret -gif -script ${SCP_LISHST} ${FIL_LISHST} ${OPT_OUTPTH}
}

function plot_hyd_chrtout {
  local FIL_HYDCRT="$1"
  local FIL_BSENME=$(basename ${FIL_HYDCRT})
  local FIL_DRYNME=$(dirname ${FIL_HYDCRT})
  ferret -gif -script ${SCP_HYDCRT} ${FIL_HYDCRT} ${OPT_OUTPTH}
}

while getopts ${INF_OPT} opt; do
  case ${opt} in
    h )
      echo ${INF_HLP}
      exit 0
      ;;
    a )
      OPT_ALLFIL=true
      ;;
    p )
      OPT_OUTPFX=$OPTARG
      ;;
    d )
      OPT_OUTDIR=$OPTARG
      ;;
    \? )
      echo "${INF_USG}" 1>&2
      exit 1
      ;;
  esac
done
shift $((OPTIND -1))

if [ -z ${LISHYDRO_DIR+x} ]; then
   echo "ERROR: LISHYDRO_DIR is unset"  1>&2
   exit 1
fi

if [[ $# -ne 1 ]]; then
  echo "${INF_USG}" 1>&2
  list_directories 1>&2
  exit 1
else
  OPT_RUNDIR=$1
fi

SCP_LISHST="${LISHYDRO_DIR}/analysis/FERRET_LIS_HIST.jnl"
if [ ! -f ${SCP_LISHST} ]; then
   echo "ERROR: LIS history script missing [${SCP_LISHST}]"  1>&2
   exit 1
fi
SCP_HYDCRT="${LISHYDRO_DIR}/analysis/FERRET_HYDRO_CHRTOUT.jnl"
if [ ! -f ${SCP_HYDCRT} ]; then
   echo "ERROR: Hydro channel routing script missing [${SCP_HYDCRT}]"  1>&2
   exit 1
fi

DIR_RUNDIR="${LISHYDRO_DIR}/run/${OPT_RUNDIR}"
if [ ! -d ${DIR_RUNDIR} ]; then
   echo "ERROR: Run directory missing [${DIR_RUNDIR}]"  1>&2
   list_directories
   exit 1
fi
DIR_LISOUT="${LISHYDRO_DIR}/run/${OPT_RUNDIR}/LIS_OUTPUT/SURFACEMODEL"
if [ ! -d ${DIR_LISOUT} ]; then
   echo "ERROR: LIS output directory missing [${DIR_LISOUT}]"  1>&2
   exit 1
fi
DIR_HYDOUT="${LISHYDRO_DIR}/run/${OPT_RUNDIR}"
if [ ! -d ${DIR_HYDOUT} ]; then
   echo "ERROR: Hydro output directory missing [${DIR_HYDOUT}]"  1>&2
   exit 1
fi

OPT_OUTPTH="${OPT_OUTDIR}/${OPT_OUTPFX}"

LST_LISHST=(`find ${DIR_LISOUT} -maxdepth 2 -type f -name ${PTN_LISHST} | sort`)
LST_HYDCRT=(`find ${DIR_HYDOUT} -maxdepth 1 -type f -name ${PTN_HYDCRT} | sort`)

check_command ferret

mkdir -p "${OPT_OUTDIR}"
if [ "${OPT_ALLFIL}" = true ]; then
  for FILENAME in ${LST_LISHST[@]}; do
    plot_lis_history ${FILENAME}
  done
  for FILENAME in ${LST_HYDCRT[@]}; do
    plot_hyd_chrtout ${FILENAME}
  done
else
  plot_lis_history ${LST_LISHST[${#LST_LISHST[@]}-1]}
  plot_hyd_chrtout ${LST_HYDCRT[${#LST_HYDCRT[@]}-1]}
fi

