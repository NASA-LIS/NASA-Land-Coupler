#!/bin/bash

INF_OPT=":had:"
INF_USG="Usage: $0 [-h] [-a] [-d DATE] BASE_DIRECTORY TEST_DIRECTORY"
INF_HLP="\nHelp:\n"
INF_HLP+="\t[-h]\t\tPrints this help message then exits.\n"
INF_HLP+="\t[-a]\t\tChecks all output files.\n"
INF_HLP+="\t[-d DATE]\tChecks output file for date fmt=YYYYMMDDHHMM\n"
INF_HLP+="\tBASE_DIRECTORY\tBaseline comparison directory.\n"
INF_HLP+="\tTEST_DIRECTORY\tTest comparison directory.\n"

OPT_BSEDIR="missing"
OPT_TSTDIR="missing"
OPT_DTEPTN="00000000000000"
OPT_DIFDIR="diffs"
OPT_DIFPFX="DIFF"
OPT_MBSDIR="diffs"
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
    printf "${COMMAND}: command not found\n" 1>&2
    exit 1
  fi
}

function list_directories {
  local LST_DIRTRY=(`find ${LISHYDRO_DIR}/run -maxdepth 1 -mindepth 1 -type d`)
  LST_DIRTRY+=(`find ${LISHYDRO_DIR}/run -maxdepth 1 -mindepth 1 -type l`)
  printf "Available directories in ${LISHYDRO_DIR}/run:\n"
  for DIRECTORY in ${LST_DIRTRY[@]}; do
    printf "\t$(basename ${DIRECTORY})\n"
  done
}

function compare_netcdf_lis {
  local BOL_ERRORS=false
  local FIL_BSEPTH="$1"
  local FIL_BSEFIL=$(basename ${FIL_BSEPTH})
  local FIL_TSTPTH=`find ${DIR_TSTLIS}/ -maxdepth 2 -type f -name ${FIL_BSEFIL}`
  local FIL_DIFFIL=${OPT_DIFDIR}/${OPT_DIFPFX}_${FIL_BSEFIL}
  local FIL_MBSFIL=${OPT_MBSDIR}/${OPT_MBSPFX}_${FIL_BSEFIL}
  printf "Checking: ${FIL_BSEFIL}\n"
  if [ ! -f "${FIL_BSEPTH}" ]; then
    printf "ERROR: Baseline file missing [${FIL_BSEFIL}]\n" 1>&2
    CNT_ERRORS=$((CNT_ERRORS + 1))
    return 1
  fi
  if [ ! -f "${FIL_TSTPTH}" ]; then
    printf "ERROR: Test file missing [${FIL_BSEFIL}]\n" 1>&2
    CNT_ERRORS=$((CNT_ERRORS + 1))
    return 1
  fi
  ncdiff -O -C ${OPT_LISVAR} ${FIL_BSEPTH} ${FIL_TSTPTH} ${FIL_DIFFIL}
  ncwa -O -ymabs ${FIL_DIFFIL} ${FIL_MBSFIL}
  LST_CMPVAL=`ncdump ${FIL_MBSFIL} | sed -e '1,/data:/d' | grep " = "`
  ARY_CMPVAL=$(echo $LST_CMPVAL | tr -d '[:space:]' | tr ";" "\n")
  for CMPVAL in ${ARY_CMPVAL}; do
    VAR="$(echo ${CMPVAL} | cut -d'=' -f1)"
    VAL="$(echo ${CMPVAL} | cut -d'=' -f2)"
    if [[ $VAL != 0 ]]; then
      printf "ERROR: ${VAR} = ${VAL}\n" 1>&2
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
  local FIL_TSTPTH=`find ${DIR_TSTHYD}/ -maxdepth 1 -type f -name ${FIL_BSEFIL}`
  local FIL_DIFFIL=${OPT_DIFDIR}/${OPT_DIFPFX}_${FIL_BSEFIL}
  local FIL_MBSFIL=${OPT_MBSDIR}/${OPT_MBSPFX}_${FIL_BSEFIL}
  printf "Checking: ${FIL_BSEFIL}\n"
  if [ ! -f "${FIL_BSEPTH}" ]; then
    printf "ERROR: Baseline file missing [${FIL_BSEFIL}]\n" 1>&2
    CNT_ERRORS=$((CNT_ERRORS + 1))
    return 1
  fi
  if [ ! -f "${FIL_TSTPTH}" ]; then
    printf "ERROR: Test file missing [${FIL_BSEFIL}]\n" 1>&2
    CNT_ERRORS=$((CNT_ERRORS + 1))
    return 1
  fi
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
      printf "ERROR: ${VAR} = ${VAL}\n" 1>&2
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
  local FIL_TSTPTH=`find ${DIR_TSTLIS}/ -maxdepth 2 -type f -name ${FIL_BSEFIL}`
  printf "Checking: ${FIL_BSEFIL}\n"
  if [ ! -f "${FIL_BSEPTH}" ]; then
    printf "ERROR: Baseline file missing [${FIL_BSEFIL}]\n" 1>&2
    CNT_ERRORS=$((CNT_ERRORS + 1))
    return 1
  fi
  if [ ! -f "${FIL_TSTPTH}" ]; then
    printf "ERROR: Test file missing [${FIL_BSEFIL}]\n" 1>&2
    CNT_ERRORS=$((CNT_ERRORS + 1))
    return 1
  fi
  nccmp -d ${OPT_LISVAR} ${FIL_BSEPTH} ${FIL_TSTPTH}
  if [ $? -ne 0 ]; then
    CNT_ERRORS=$((CNT_ERRORS + 1))
  fi
}

function nccmp_netcdf_hyd {
  local FIL_BSEPTH="$1"
  local FIL_BSEFIL=$(basename ${FIL_BSEPTH})
  local FIL_TSTPTH=`find ${DIR_TSTHYD}/ -maxdepth 1 -type f -name ${FIL_BSEFIL}`
  printf "Checking: ${FIL_BSEFIL}\n"
  if [ ! -f "${FIL_BSEPTH}" ]; then
    printf "ERROR: Baseline file missing [${FIL_BSEFIL}]\n" 1>&2
    CNT_ERRORS=$((CNT_ERRORS + 1))
    return 1
  fi
  if [ ! -f "${FIL_TSTPTH}" ]; then
    printf "ERROR: Test file missing [${FIL_BSEFIL}]\n" 1>&2
    CNT_ERRORS=$((CNT_ERRORS + 1))
    return 1
  fi
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
    printf "\nCHECK: Found $CNT_ERRORS file(s) with errors\n"
    exit 1
  else
    printf "\nCHECK: No errors\n"
    exit 0
  fi
}

while getopts ${INF_OPT} opt; do
  case ${opt} in
    h )
      printf "${INF_USG}\n"
      printf "${INF_HLP}\n"
      exit 0
      ;;
    a )
      OPT_ALLFIL=true
      ;;
    d )
      OPT_ALLFIL=true
      OPT_DTEPTN="${OPTARG}"
      PTN_LISHST="${PTN_LISHST}${OPT_DTEPTN}*"
      PTN_HYDCRT="${OPT_DTEPTN}${PTN_HYDCRT}"
      ;;
    \? )
      printf "${INF_USG}\n" 1>&2
      exit 1
      ;;
  esac
done
shift $((OPTIND -1))

if [ -z "${LISHYDRO_DIR+x}" ]; then
   printf "ERROR: LISHYDRO_DIR is unset\n" 1>&2
   exit 1
fi

if [[ $# -ne 2 ]]; then
  printf "${INF_USG}\n" 1>&2
  list_directories 1>&2
  exit 1
else
  OPT_BSEDIR=$1
  OPT_TSTDIR=$2
fi

DIR_BSEDIR="${LISHYDRO_DIR}/run/${OPT_BSEDIR}"
if [ ! -e "${DIR_BSEDIR}" ]; then
   printf "ERROR: Baseline directory missing [${OPT_BSEDIR}]\n" 1>&2
   list_directories
   exit 1
fi
DIR_BSELIS="${LISHYDRO_DIR}/run/${OPT_BSEDIR}/LIS_OUTPUT/SURFACEMODEL"
if [ ! -e "${DIR_BSELIS}" ]; then
   printf "ERROR: LIS output directory missing [${DIR_BSELIS}]\n" 1>&2
   exit 1
fi
DIR_BSEHYD="${LISHYDRO_DIR}/run/${OPT_BSEDIR}"
if [ ! -e "${DIR_BSEHYD}" ]; then
   printf "ERROR: Hydro output directory missing [${DIR_BSEHYD}]\n" 1>&2
   exit 1
fi

DIR_TSTDIR="${LISHYDRO_DIR}/run/${OPT_TSTDIR}"
if [ ! -e "${DIR_TSTDIR}" ]; then
   printf "ERROR: Test directory missing [${OPT_TSTDIR}]\n" 1>&2
   list_directories
   exit 1
fi
DIR_TSTLIS="${LISHYDRO_DIR}/run/${OPT_TSTDIR}/LIS_OUTPUT/SURFACEMODEL"
if [ ! -e "${DIR_TSTLIS}" ]; then
   printf "ERROR: LIS output directory missing [${DIR_TSTLIS}]\n" 1>&2
   exit 1
fi
DIR_TSTHYD="${LISHYDRO_DIR}/run/${OPT_TSTDIR}"
if [ ! -e "${DIR_TSTHYD}" ]; then
   printf "ERROR: Hydro output directory missing [${DIR_TSTHYD}]\n" 1>&2
   exit 1
fi

LST_BSELIS=(`find ${DIR_BSELIS}/ -maxdepth 2 -type f -name ${PTN_LISHST} | sort`)
LST_BSEHYD=(`find ${DIR_BSEHYD}/ -maxdepth 1 -type f -name ${PTN_HYDCRT} | sort`)

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

