#!/bin/bash

find_system () {
  local sysname=`hostname`
  sysname="${sysname//[[:digit:]]/}"
  source /etc/os-release
  case ${sysname} in
    discover*) echo "$sysname-$VERSION_ID" ;;
    *) echo "$sysname" ;;
  esac
}


auto_environment () {
  case ${1} in
    "cheyenne")
      COMPILER="${COMPILER:-intel}"
      COMPILER_VERS="${COMPILER_VERS:-intel-19.1.1}"
      BATCH_SYS="${BATCH_SYS:-qsub}"
      CPPERNODE="${CPPERNODE:-36}"
      DATA_ROOT="${DATA_ROOT:-/glade/p/ral/hap/drosen/projects/LISHydro/data}"
      source "${2}/cheyenne/intel-19.1.1";;
    "discover-12.5")
      source /etc/profile.d/modules.sh
      COMPILER="${COMPILER:-intel}"
      COMPILER_VERS="${COMPILER_VERS:-intel-2021.4.0}"
      BATCH_SYS="${BATCH_SYS:-sbatch}"
      CPPERNODE="${CPPERNODE:-28}"
      DATA_ROOT="${DATA_ROOT:-/discover/nobackup/projects/nu-wrf/lishydro/data}"
      module use "${2}/discover-12.5/"
      module load "intel-2021.4.0";;
    "discover-15.4")
      source /etc/profile.d/modules.sh
      COMPILER="${COMPILER:-intel}"
      COMPILER_VERS="${COMPILER_VERS:-intel-2023.2.1}"
      BATCH_SYS="${BATCH_SYS:-sbatch}"
      CPPERNODE="${CPPERNODE:-28}"
      DATA_ROOT="${DATA_ROOT:-/discover/nobackup/projects/nu-wrf/lishydro/data}"
      module use "${2}/discover-15.4/"
      module load "intel-2023.2.1";;
    *) printf "ERROR: no modulefile file for ${1}\n"; exit 1 ;;
  esac
}
