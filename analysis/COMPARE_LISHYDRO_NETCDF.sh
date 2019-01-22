#!/bin/bash

INF_OPT=":ha"
INF_USG="Usage: $0 [-h] [-a] BASE_DIRECTORY TEST_DIRECTORY"
INF_HLP="Help:"

OPT_BSEDIR="missing"
OPT_TSTDIR="missing"
OPT_DIFDIR="diffs"
OPT_DIFPFX="DIFF"
OPT_MBSDIR="mabs"
OPT_MBSPFX="MABS"
OPT_ALLFIL=false
OPT_LISVAR="-v CRainf_f_inst,CRainf_f_tavg,EWind_f_inst,EWind_f_tavg,LWdown_f_inst,LWdown_f_tavg,NWind_f_inst,NWind_f_tavg,Psurf_f_inst,Psurf_f_tavg,Qair_f_inst,Qair_f_tavg,Qg_inst,Qg_tavg,Qh_inst,Qh_tavg,Qle_inst,Qle_tavg,Qs_inst,Qs_tavg,Qsb_inst,Qsb_tavg,Rainf_f_inst,Rainf_f_tavg,SWdown_f_inst,SWdown_f_tavg,SmLiqFrac_inst,SmLiqFrac_tavg,SoilMoist_inst,SoilMoist_tavg,SoilTemp_inst,SoilTemp_tavg,Tair_f_inst,Tair_f_tavg"
OPT_HYDVAR="-v streamflow,head"
OPT_HYDVRN="-d .feature_id,station -v .Head,head"

PTN_LISHST="LIS_HIST*"
PTN_HYDCRT="*.CHRTOUT_DOMAIN1"

CNT_ERRORS=0

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

function compare_netcdf_lis {
  local BOL_ERRORS=false
  local FIL_BSEPTH="$1"
  local FIL_BSEFIL=$(basename ${FIL_BSEPTH})
  local FIL_TSTPTH=`find ${DIR_TSTLIS} -maxdepth 2 -type f -name ${FIL_BSEFIL}`
  local FIL_DIFFIL=${OPT_DIFDIR}/${OPT_DIFPFX}_${FIL_BSEFIL}
  local FIL_MBSFIL=${OPT_MBSDIR}/${OPT_MBSPFX}_${FIL_BSEFIL}
  echo "Checking: ${FIL_BSEFIL}"
  ncdiff -O -C ${OPT_LISVAR} ${FIL_BSEPTH} ${FIL_TSTPTH} ${FIL_DIFFIL}
  ncwa -O -ymabs ${FIL_DIFFIL} ${FIL_MBSFIL}
  LST_CMPVAL=`ncdump ${FIL_MBSFIL} | sed -e '1,/data:/d' | grep " = "`
  ARY_CMPVAL=$(echo $LST_CMPVAL | tr -d '[:space:]' | tr ";" "\n")
  for CMPVAL in ${ARY_CMPVAL}; do
    VAR="$(echo ${CMPVAL} | cut -d'=' -f1)"
    VAL="$(echo ${CMPVAL} | cut -d'=' -f2)"
    if [[ $VAL != 0 ]]; then
      echo "ERROR: ${VAR} = ${VAL}"
      BOL_ERRORS=true
    fi
  done
  if [ "${BOL_ERRORS}" = true ]; then
    CNT_ERRORS=$((CNT_ERRORS + 1))
  fi
}

function compare_netcdf_hyd {
  local BOL_ERRORS=false
  local FIL_BSEPTH="$1"
  local FIL_BSEFIL=$(basename ${FIL_BSEPTH})
  local FIL_TSTPTH=`find ${DIR_TSTHYD} -maxdepth 1 -type f -name ${FIL_BSEFIL}`
  local FIL_DIFFIL=${OPT_DIFDIR}/${OPT_DIFPFX}_${FIL_BSEFIL}
  local FIL_MBSFIL=${OPT_MBSDIR}/${OPT_MBSPFX}_${FIL_BSEFIL}
  echo "Checking: ${FIL_BSEFIL}"
  ncrename ${OPT_HYDVRN} ${FIL_BSEPTH} TMP_BSEPTH.nc > /dev/null
  ncrename ${OPT_HYDVRN} ${FIL_TSTPTH} TMP_TSTPTH.nc > /dev/null
  ncdiff -O -C ${OPT_HYDVAR} TMP_BSEPTH.nc TMP_TSTPTH.nc ${FIL_DIFFIL}
  ncwa -O -ymabs ${FIL_DIFFIL} ${FIL_MBSFIL}
  LST_CMPVAL=`ncdump ${FIL_MBSFIL} | sed -e '1,/data:/d' | grep " = "`
  ARY_CMPVAL=$(echo $LST_CMPVAL | tr -d '[:space:]' | tr ";" "\n")
  for CMPVAL in ${ARY_CMPVAL}; do
    VAR="$(echo ${CMPVAL} | cut -d'=' -f1)"
    VAL="$(echo ${CMPVAL} | cut -d'=' -f2)"
    if [[ $VAL != 0 ]]; then
      echo "ERROR: ${VAR} = ${VAL}"
      BOL_ERRORS=true
    fi
  done
  if [ "${BOL_ERRORS}" = true ]; then
    CNT_ERRORS=$((CNT_ERRORS + 1))
  fi
  rm -f TMP_BSEPTH.nc TMP_TSTPTH.nc
}

function nccmp_netcdf_lis {
  local FIL_BSEPTH="$1"
  local FIL_BSEFIL=$(basename ${FIL_BSEPTH})
  local FIL_TSTPTH=`find ${DIR_TSTLIS} -maxdepth 2 -type f -name ${FIL_BSEFIL}`
  echo "Checking: ${FIL_BSEFIL}"
  nccmp -d ${OPT_LISVAR} ${FIL_BSEPTH} ${FIL_TSTPTH}
  if [ $? -ne 0 ]; then
    CNT_ERRORS=$((CNT_ERRORS + 1))
  fi
}

function nccmp_netcdf_hyd {
  local FIL_BSEPTH="$1"
  local FIL_BSEFIL=$(basename ${FIL_BSEPTH})
  local FIL_TSTPTH=`find ${DIR_TSTHYD} -maxdepth 1 -type f -name ${FIL_BSEFIL}`
  echo "Checking: ${FIL_BSEFIL}"
  ncrename ${OPT_HYDVRN} ${FIL_BSEPTH} TMP_BSEPTH.nc > /dev/null
  ncrename ${OPT_HYDVRN} ${FIL_TSTPTH} TMP_TSTPTH.nc > /dev/null
  nccmp -d ${OPT_HYDVAR} TMP_BSEPTH.nc TMP_TSTPTH.nc
  if [ $? -ne 0 ]; then
    CNT_ERRORS=$((CNT_ERRORS + 1))
  fi
  rm -f TMP_BSEPTH.nc TMP_TSTPTH.nc
}

function check_exit {
  if [[ $CNT_ERRORS != 0 ]]; then
    echo ""
    echo "CHECK: Found $CNT_ERRORS file with errors"
    exit 1
  else
    echo ""
    echo "CHECK: No errors"
    exit 0
  fi
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

if [[ $# -ne 2 ]]; then
  echo "${INF_USG}" 1>&2
  list_directories 1>&2
  exit 1
else
  OPT_BSEDIR=$1
  OPT_TSTDIR=$2
fi

DIR_BSEDIR="${LISHYDRO_DIR}/run/${OPT_BSEDIR}"
if [ ! -d ${DIR_BSEDIR} ]; then
   echo "ERROR: Baseline directory missing [${OPT_BSEDIR}]"  1>&2
   list_directories
   exit 1
fi
DIR_BSELIS="${LISHYDRO_DIR}/run/${OPT_BSEDIR}/LIS_OUTPUT/SURFACEMODEL"
if [ ! -d ${DIR_BSELIS} ]; then
   echo "ERROR: LIS output directory missing [${DIR_BSELIS}]"  1>&2
   exit 1
fi
DIR_BSEHYD="${LISHYDRO_DIR}/run/${OPT_BSEDIR}"
if [ ! -d ${DIR_BSEHYD} ]; then
   echo "ERROR: Hydro output directory missing [${DIR_BSEHYD}]"  1>&2
   exit 1
fi

DIR_TSTDIR="${LISHYDRO_DIR}/run/${OPT_TSTDIR}"
if [ ! -d ${DIR_TSTDIR} ]; then
   echo "ERROR: Test directory missing [${OPT_TSTDIR}]"  1>&2
   list_directories
   exit 1
fi
DIR_TSTLIS="${LISHYDRO_DIR}/run/${OPT_TSTDIR}/LIS_OUTPUT/SURFACEMODEL"
if [ ! -d ${DIR_TSTLIS} ]; then
   echo "ERROR: LIS output directory missing [${DIR_TSTLIS}]"  1>&2
   exit 1
fi
DIR_TSTHYD="${LISHYDRO_DIR}/run/${OPT_TSTDIR}"
if [ ! -d ${DIR_TSTHYD} ]; then
   echo "ERROR: Hydro output directory missing [${DIR_TSTHYD}]"  1>&2
   exit 1
fi

LST_BSELIS=(`find ${DIR_BSELIS} -maxdepth 2 -type f -name ${PTN_LISHST} | sort`)
LST_BSEHYD=(`find ${DIR_BSEHYD} -maxdepth 1 -type f -name ${PTN_HYDCRT} | sort`)

check_command ncdump
check_command ncdiff
check_command ncwa
check_command ncrename
#check_command nccmp

mkdir -p "${OPT_DIFDIR}"
mkdir -p "${OPT_MBSDIR}"
if [ "${OPT_ALLFIL}" = true ]; then
  for FILENAME in ${LST_BSELIS[@]}; do
#    nccmp_netcdf_lis ${FILENAME}
    compare_netcdf_lis ${FILENAME}
  done
  for FILENAME in ${LST_BSEHYD[@]}; do
    compare_netcdf_hyd ${FILENAME}
  done
else
#  nccmp_netcdf_lis ${LST_BSELIS[-1]}
  compare_netcdf_lis ${LST_BSELIS[${#LST_BSELIS[@]}-1]}
  compare_netcdf_hyd ${LST_BSEHYD[${#LST_BSEHYD[@]}-1]}
fi

check_exit

