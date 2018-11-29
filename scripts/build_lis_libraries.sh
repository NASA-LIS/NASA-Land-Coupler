#!/bin/bash

# Darwin (OSX) Support
if [[ $(uname) == "Darwin" ]]; then
   MD5="md5 -r"
   TAROPT=""
else
   MD5="md5sum"
   TAROPT="--skip-old-files"
fi

# Script Dev Settings
set -u
set -o pipefail

# Script Defaults
RED="" # Red
GRN="" # Green
YLW="" # Dark Gray
BLU="" # Blue
NCL="" # No Color
quiet=""
output="/dev/stdout"
declare -A libraries=( 
   [hdf5]=true
   [netcdf]=true
   [netcdf-fortran]=true
   [jasper]=true
   [grib_api]=true
   [esmf]=true
   [hdf]=true
   [hdfeos]=true
)
get=true
check=true
extract=true
config=true
build=true
scpdir="${PWD}"
libdir="${PWD}/lib"
achdir="${libdir}/archives"
srcdir="${libdir}/src"
logdir="${libdir}"
insdir="${libdir}"
clean=false
scnt=0
ecnt=0
declare -a elist=("")
compiler="default"
compiler_v="0.0.0"
ccmp="none"
fcmp="none"
f77c="none"
f90c="none"
ccmp_v="0.0.0"
fcmp_v="0.0.0"

# Script Options
usage="$(basename $0) [-h] [-l list] [-s #] [-d directory] [-p compiler]"
usage+=" [-q] [-c] [-o #] [-r]"
while getopts ":hl:s:p:qcd:o:r" opt; do
  case ${opt} in
    h ) printf "script usage: ${usage}\n"
        printf "\t-h\thelp\t\tprints this help information and exits\n"
        printf "\t-l\tlibraries\tinclude listed libraries, comma delimited\n"
        printf "\t-s\tstep\t\t1=get, 2=check, 3=extract, 4=config, 5=build\n"
        printf "\t-d\tdirectory\tinstallation directory\n"
        printf "\t-p\tcompiler\tset the compiler\n"
        printf "\t-q\tquiet\t\tsupress wget, unzip, and tar output\n"
        printf "\t-c\tclean\t\tdelete downloaded and extracted files\n"
        printf "\t-o\toutput\t\t0=none, 1=screen, 2=log\n"
        printf "\t-r\tcolor\t\tadds color to output\n"
        exit 0
      ;;
    l ) list=${OPTARG//,/ }
        unset libraries
        declare -A libraries=( 
           [hdf5]=false
           [netcdf]=false
           [netcdf-fortran]=false
           [jasper]=false
           [grib_api]=false
           [esmf]=false
           [hdf]=false
           [hdfeos]=false
        )
        for lib in ${list}; do
           libraries[${lib}]=true
        done
      ;;
    s ) case ${OPTARG} in
          0 ) get=false; check=false; extract=false; config=false; build=false
            ;;
          1 ) get=true; check=false; extract=false; config=false; build=false
            ;;
          2 ) get=false; check=true; extract=false; config=false; build=false
            ;;
          3 ) get=false; check=false; extract=true; config=false; build=false
            ;;
          4 ) get=false; check=false; extract=false; config=true; build=false
            ;;
          5 ) get=false; check=false; extract=false; config=false; build=true
            ;;
          ? ) printf "ERROR: no step option: ${OPTARG}\n" 1>&2
              exit 1
            ;;
        esac
      ;;
    d ) libdir="${OPTARG}"
        achdir="${libdir}/archives"
        srcdir="${libdir}/src"
        logdir="${libdir}"
        insdir="${libdir}"
      ;;
    p ) compiler=${OPTARG}
      ;;
    q ) quiet='-q'
      ;;
    c ) clean=true
      ;;
    o ) case ${OPTARG} in
          0 ) output="/dev/null"
            ;;
          1 ) output="/dev/stdout"
            ;;
          2 ) output="${logdir}/buildlibs.log"
              rm -f ${output}
              mkdir -p ${logdir}
            ;;
          ? ) printf "ERROR: no output option: ${OPTARG}\n" 1>&2
              exit 1
            ;;
        esac
      ;;
    r ) RED='\033[0;31m' # Red
        GRN='\033[0;32m' # Green
        YLW='\033[0;33m' # Dark Gray
        BLU='\033[0;34m' # Blue
        NCL='\033[0;00m' # No Color
      ;;
    ? ) printf "ERROR: script usage: ${usage}\n" 1>&2
        exit 1
      ;;
  esac
done
shift $((OPTIND -1))

# Determine Compiler
function set_compiler()
{
   local icc=false
   local ifort=false
   local gcc=false
   local gfotran=false
   icc --version >/dev/null 2>&1
   if [ "$?" -eq 0 ]; then icc=true; fi 
   ifort --version >/dev/null 2>&1
   if [ "$?" -eq 0 ]; then ifort=true; fi
   gcc --version >/dev/null 2>&1
   if [ "$?" -eq 0 ]; then gcc=true; fi
   gfortran --version >/dev/null 2>&1
   if [ "$?" -eq 0 ]; then gfortran=true; else gfortran=false ; fi
   if [[ "${compiler}" == "default" ]]; then 
      if [ "${icc}" = true ] && [ "${ifort}" = true ] ; then
         compiler='intel'
      elif [ "${gcc}" = true ] && [ "${gfortran}" = true ] ; then
         compiler='gnu'
      else
         compiler='none'
      fi
   fi
   if [[ "${compiler}" == "intel" ]]; then
      if [ "${icc}" = false ] || [ "${ifort}" = false ] ; then
         printf "${RED}ERROR  :${NCL} intel compilers not found\n" 1>&2
         return 1
      fi
      ccmp="icc"
      fcmp="ifort"
      f77c="ifort"
      f90c="ifort"
      ccmp_v=`${ccmp} --version 2>/dev/null | head -n 1`
      ccmp_v=`echo "${ccmp_v}" | sed 's/^[a-zA-Z ()]*//' | sed 's/ .*$//'`
      fcmp_v=`${fcmp} --version 2>/dev/null | head -n 1`
      fcmp_v=`echo "${fcmp_v}" | sed 's/^[a-zA-Z ()]*//' | sed 's/ .*$//'`
   elif [[ "${compiler}" == "gnu" ]]; then
      if [ "${gcc}" = false ] || [ "${gfortran}" = false ] ; then
         ccmp_v="0.0.0"
         fcmp_v="0.0.0"
         printf "${RED}ERROR  :${NCL} gnu compilers not found\n" 1>&2
         return 1
      fi
      ccmp="gcc"
      fcmp="gfortran"
      f77c="gfortran"
      f90c="gfortran"
      ccmp_v=`${ccmp} --version 2>/dev/null | head -n 1`
      ccmp_v=`echo "${ccmp_v}" | sed 's/^[a-zA-Z ()]*//' | sed 's/ .*$//'`
      fcmp_v=`${fcmp} --version 2>/dev/null | head -n 1`
      fcmp_v=`echo "${fcmp_v}" | sed 's/^[a-zA-Z ()]*//' | sed 's/ .*$//'`
   else
      ccmp_v="0.0.0"
      fcmp_v="0.0.0"
      printf "${RED}ERROR  :${NCL} compiler not supported [${compiler}]\n" 1>&2
      return 1
   fi
   compiler_v="${compiler}-${fcmp_v}"
   export CC="${ccmp}"
   export FC="${fcmp}"
   export F77="${f77c}"
   export F90="${f90c}"
   return 0
}

# Get File Using wget
function get_file()
{
   if [ "${get}" = false ] ; then
      return 0
   fi
   local l_fpth="$1"
   printf "${YLW}GET    :${NCL} [${l_fpth}]\n" 
   mkdir -p ${achdir}
   wget ${quiet} -P ${achdir} -nc ${l_fpth} >> ${output} 2>&1
   if [ "$?" -ne 0 ]; then return 1; fi
   return 0
}

# Check File Using md5sum
function check_file()
{
   if [ "${check}" = false ] ; then
      return 0
   fi
   local l_file="$1"
   local l_md5h="$2"
   local l_fpth="${achdir}/${l_file}"
   printf "${YLW}CHECK  :${NCL} [${l_file}]\n"
   if [ ! -f ${l_fpth} ]; then
      printf "${RED}ERROR  :${NCL} no file [${l_fpth}]\n" 1>&2
      return 1
   else
      l_md5s=`${MD5} ${l_fpth} | sed 's/ .*$//'`
      if [[ "${l_md5s}" != "${l_md5h}" ]]; then return 1; fi
   fi
   return 0
}

# Extract File Using unzip, gzip, tar
function extract_file()
{
   if [ "${extract}" = false ] ; then
      return 0
   fi
   local l_file="$1"
   local l_extn="$2"
   local l_fpth="${achdir}/${l_file}"
   printf "${YLW}EXTRACT:${NCL} [${l_file}]\n"
   mkdir -p ${srcdir}
   if [ ! -f ${l_fpth} ]; then
      printf "${RED}ERROR  :${NCL} no file [${l_fpth}]\n" 1>&2
      return 1
   elif [[ "${l_extn}" == "zip" ]]; then
      unzip ${quiet} -n -d ${srcdir} ${l_fpth} >> ${output} 2>&1
      if [ "$?" -ne 0 ]; then return 1; fi
   elif [[ ${l_extn} == "tar.gz" ]]; then
      tar ${TAROPT} -C ${srcdir} -zxf ${l_fpth} >> ${output} 2>&1
      if [ "$?" -ne 0 ]; then return 1; fi
   elif [[ ${l_extn} == "tar.Z" ]]; then
      tar ${TAROPT} -C ${srcdir} -zxf ${l_fpth} >> ${output} 2>&1
      if [ "$?" -ne 0 ]; then return 1; fi
   elif [[ ${l_extn} == "tar" ]]; then
      tar ${TAROPT} -C ${srcdir} -xf ${l_fpth} >> ${output} 2>&1
      if [ "$?" -ne 0 ]; then return 1; fi
   else
      printf "${RED}ERROR  :${NCL} extension unknown [${l_extn}]\n" 1>&2
      return 1
   fi
   return 0
}

function config_lib()
{
   if [ "${config}" = false ]; then
      return 0
   fi
   local l_edir="$1"
   local l_copt="$2"
   printf "${YLW}CONFIG :${NCL} [${l_edir}]\n"
   cd ${srcdir}/${l_edir}
   if [ ! -x configure ]; then
      printf "${RED}WARNING:${NCL} no configure script [${l_edir}]\n" 1>&2
   else
      ./configure ${quiet} ${l_copt} >> ${output} 2>&1
      if [ "$?" -ne 0 ]; then return 1; fi
   fi
   cd ${scpdir}
   return 0
}

function build_lib()
{
   if [ "${build}" = false ] ; then
      return 0
   fi
   local l_edir="$1"
   printf "${YLW}BUILD  :${NCL} [${l_edir}]\n"
   cd ${srcdir}/${l_edir}
   make >> ${output} 2>&1
   make install >> ${output} 2>&1
   if [ "$?" -ne 0 ]; then return 1; fi
   cd ${scpdir}
   return 0
}

# Clean Downloads and Extracted Files
function clean_lib()
{
   if [ "${clean}" = false ] ; then
      return 0
   fi
   local l_file="$1"
   local l_edir="$2"
   rm -f "${achdir}/${l_file}"
   if [ "$?" -ne 0 ]; then return 1; fi
   rm -rf "${srcdir}/${l_edir}"
   if [ "$?" -ne 0 ]; then return 1; fi
   return 0
}

# Get, Check, Extract, Config, Build, Clean
function install_lib()
{
   local l_file="$1"
   local l_extn="$2"
   local l_site="$3"
   local l_md5h="$4"
   local l_edir="$5"
   local l_copt="$6"
   local l_strt=`date +%s`
   if [ "${clean}" = false ] ; then
      get_file "${l_site}/${l_file}"
      if [ "$?" -ne 0 ]; then
         printf "${RED}ERROR  :${NCL} get failed [${l_file}]\n" 1>&2
         return 1
      fi
      check_file "${l_file}" "${l_md5h}"
      if [ "$?" -ne 0 ]; then
         printf "${RED}ERROR  :${NCL} check failed [${l_file}]\n" 1>&2
         return 1
      fi
      extract_file "${l_file}" "${l_extn}"
      if [ "$?" -ne 0 ]; then
         printf "${RED}ERROR  :${NCL} extract failed [${l_file}]\n" 1>&2
         return 1
      fi
      config_lib "${l_edir}" "${l_copt}"
      if [ "$?" -ne 0 ]; then
         printf "${RED}ERROR  :${NCL} config failed [${l_edir}]\n" 1>&2
         return 1
      fi
      build_lib "${l_edir}"
      if [ "$?" -ne 0 ]; then
         printf "${RED}ERROR  :${NCL} build failed [${l_edir}]\n" 1>&2
         return 1
      fi
   else
      clean_lib "${l_file}" "${l_edir}"
      if [ "$?" -ne 0 ]; then
         printf "${RED}ERROR  :${NCL} clean failed [${l_edir}]\n" 1>&2
         return 1
      fi
   fi
   local l_endt=`date +%s`
   local l_runt=$((l_endt-l_strt))
   printf "${GRN}SUCCESS:${NCL} [${l_edir}] in ${l_runt}s\n"
}

function summary()
{
   # Print Summary to Stdout
   printf "\n"
   printf "${BLU}###################################################${NCL}\n"
   printf "${BLU}#                     SUMMARY                     #${NCL}\n"
   printf "${BLU}###################################################${NCL}\n"
   printf "\n"
   printf "\t${YLW}C Compiler:${NCL} ${ccmp} ${ccmp_v}\n"
   printf "\t${YLW}Fortran Compiler:${NCL} ${fcmp} ${fcmp_v}\n"
   printf "\n"
   printf "\t${GRN}SUCCESS :${NCL} ${scnt}\n"
   printf "\t${RED}FAILURES:${NCL} ${ecnt} ${elist[*]}\n"
   printf "\n"
   printf "${BLU}###################################################${NCL}\n"
   # Print Result to Output Log
   echo   "---------------------------------------------------" >> ${output} 2>&1
   echo   "$(basename $0): ${ecnt} failure(s) ${elist[*]}" >> ${output} 2>&1
   exit ${ecnt}
}

# set compiler default
set_compiler

# hdf5
libr="hdf5"; vrsn="1.8.14"
file="hdf5-1.8.14.tar.gz"; extn="tar.gz"
edir="hdf5-1.8.14"
hdf5_idir="${insdir}/${libr}/${vrsn}_${compiler_v}"
copt="--enable-fortran --prefix=${hdf5_idir}"
site="https://support.hdfgroup.org/ftp/HDF5/releases/hdf5-1.8/hdf5-1.8.14/src"
md5h="a482686e733514a51cde12d6fe5c5d95"
if [ "${libraries[$libr]}" = true ]; then
   (
      export CFLAGS="-fpic"
      export FCFLAGS="-fpic"
      install_lib "${file}" "${extn}" "${site}" "${md5h}" "${edir}" "${copt}"
   )
   if [ "$?" -ne 0 ]; then ecnt=$((ecnt+1)); elist+=("${libr}");
   else scnt=$((scnt+1)); fi
fi

# netcdf
libr="netcdf"; vrsn="4.3.3.1"
file="v4.3.3.1.tar.gz"; extn="tar.gz"
edir="netcdf-c-4.3.3.1"
netcdf_idir="${insdir}/${libr}/${vrsn}_${compiler_v}"
copt="--enable-netcdf-4 --disable-dap-remote-tests --prefix=${netcdf_idir}"
site="https://github.com/Unidata/netcdf-c/archive"
md5h="41fe6758d46cccb1675693d155ee7001"
if [ "${libraries[$libr]}" = true ]; then
   (
      export CPPFLAGS="-I${hdf5_idir}/include"
      export LDFLAGS="-L${hdf5_idir}/lib"
      export CFLAGS="-fpic"
      export FCFLAGS="-fpic"
      install_lib "${file}" "${extn}" "${site}" "${md5h}" "${edir}" "${copt}"
   )
   if [ "$?" -ne 0 ]; then ecnt=$((ecnt+1)); elist+=("${libr}");
   else scnt=$((scnt+1)); fi
fi

# netcdf-fortran (install with netcdf)
libr="netcdf-fortran"; vrsn="4.2"
file="netcdf-fortran-4.2.tar.gz"; extn="tar.gz"
edir="netcdf-fortran-4.2"
netcdff_idir="${netcdf_idir}"
copt="--prefix=${netcdff_idir}"
site="ftp://ftp.unidata.ucar.edu/pub/netcdf"
md5h="cc3bf530223e8f4aff93793b9f197bf3"
if [ "${libraries[$libr]}" = true ]; then
   (
      export LD_LIBRARY_PATH="${netcdf_idir}/lib:${LD_LIBRARY_PATH}"
      export CPPFLAGS="-I${netcdf_idir}/include -DgFortran"
      export LDFLAGS="-L${netcdf_idir}/lib"
      export CFLAGS="-fpic"
      export FCFLAGS="-fpic"
      install_lib "${file}" "${extn}" "${site}" "${md5h}" "${edir}" "${copt}"
   )
   if [ "$?" -ne 0 ]; then ecnt=$((ecnt+1)); elist+=("${libr}");
   else scnt=$((scnt+1)); fi
fi

# jasper
libr="jasper"; vrsn="1.900.1"
file="jasper-1.900.1.zip"; extn="zip"
edir="jasper-1.900.1"
jasper_idir="${insdir}/${libr}/${vrsn}_${compiler_v}"
copt="--enable-shared --prefix=${jasper_idir}"
site="http://www.ece.uvic.ca/~frodo/jasper/software"
md5h="a342b2b4495b3e1394e161eb5d85d754"
if [ "${libraries[$libr]}" = true ]; then
   (
      install_lib "${file}" "${extn}" "${site}" "${md5h}" "${edir}" "${copt}"
   )
   if [ "$?" -ne 0 ]; then ecnt=$((ecnt+1)); elist+=("${libr}");
   else scnt=$((scnt+1)); fi
fi

# grib_api
libr="grib_api"; vrsn="1.12.3"
file="grib_api-1.12.3.tar.gz"; extn="tar.gz"
edir="grib_api-1.12.3"
gribapi_idir="${insdir}/${libr}/${vrsn}_${compiler_v}"
copt="--with-jasper=${jasper_idir} --with-netcdf=${netcdf_idir}"
copt+=" --prefix=${gribapi_idir}"
site="https://confluence.ecmwf.int/download/attachments/3473437"
md5h="584f60702aeed70330cca42d13b96889"
if [ "${libraries[$libr]}" = true ]; then
   (
      install_lib "${file}" "${extn}" "${site}" "${md5h}" "${edir}" "${copt}"
   )
   if [ "$?" -ne 0 ]; then ecnt=$((ecnt+1)); elist+=("${libr}");
   else scnt=$((scnt+1)); fi
fi

# esmf
libr="esmf"; vrsn="7_1_0r"
file="esmf_7_1_0r_src.tar.gz"; extn="tar.gz"
edir="esmf"
esmf_idir="${insdir}/${libr}/${vrsn}_${compiler_v}"
copt=""
site="https://sourceforge.net/projects/esmf/files/ESMF_7_1_0r"
md5h="9e455bc36a0aaa9b87e0bdedc78a47f5"
if [ "${libraries[$libr]}" = true ]; then
   (
      if [[ "${compiler}" == "intel" ]]; then
         export ESMF_COMPILER="intel"
         export ESMF_COMM="mpich2"
      elif [[ "${compiler}" == "gnu" ]]; then
         export ESMF_COMPILER="gfortran"
         export ESMF_COMM="mpich2"
      else
         printf "${RED}ERROR  :${NCL} compiler unknown [${compiler}]\n" 1>&2
         exit 1
      fi
      export ESMF_DIR="${srcdir}/${edir}"
      export ESMF_INSTALL_PREFIX="${esmf_idir}"
      export ESMF_BOPT="O"
      export ESMF_NETCDF="split"
      export ESMF_NETCDF_INCLUDE="${netcdf_idir}/include"
      export ESMF_NETCDF_LIBPATH="${netcdf_idir}/lib"
      export ESMF_INSTALL_HEADERDIR="include"
      export ESMF_INSTALL_MODDIR="mod"
      export ESMF_INSTALL_LIBDIR="lib"
      export ESMF_INSTALL_BINDIR="bin"
      export ESMF_INSTALL_DOCDIR="doc"
      install_lib "${file}" "${extn}" "${site}" "${md5h}" "${edir}" "${copt}"
   )
   if [ "$?" -ne 0 ]; then ecnt=$((ecnt+1)); elist+=("${libr}");
   else scnt=$((scnt+1)); fi
fi

# hdf4
libr="hdf"; vrsn="4.2.11"
file="hdf-4.2.11.tar.gz"; extn="tar.gz"
edir="hdf-4.2.11"
hdf_idir="${insdir}/${libr}/${vrsn}_${compiler_v}"
copt="--enable-fortran --disable-netcdf --prefix=${hdf_idir}"
site="https://support.hdfgroup.org/ftp/HDF/releases/HDF4.2.11/src"
md5h="063f9928f3a19cc21367b71c3b8bbf19"
if [ "${libraries[$libr]}" = true ]; then
   (
      install_lib "${file}" "${extn}" "${site}" "${md5h}" "${edir}" "${copt}"
   )
   if [ "$?" -ne 0 ]; then ecnt=$((ecnt+1)); elist+=("${libr}");
   else scnt=$((scnt+1)); fi
fi

# hdfeos
libr="hdfeos"; vrsn="2.19v1.00"
file="HDF-EOS2.19v1.00.tar.Z"; extn="tar.Z"
edir="hdfeos"
hdfeos_idir="${insdir}/${libr}/${vrsn}_${compiler_v}"
copt="--prefix=${hdfeos_idir}"
site="ftp://edhs1.gsfc.nasa.gov/edhs/hdfeos/previous_releases"
md5h="b8648484fc78a2db7073dd603f3fb251"
if [ "${libraries[$libr]}" = true ]; then
   (
      export CC="${hdf_idir}/bin/h4cc -Df2cFortran"
      export FC="${hdf_idir}/bin/h4fc"
      export F77="${hdf_idir}/bin/h4fc"
      export F90="${hdf_idir}/bin/h4fc"
      install_lib "${file}" "${extn}" "${site}" "${md5h}" "${edir}" "${copt}"
   )
   if [ "$?" -ne 0 ]; then ecnt=$((ecnt+1)); elist+=("${libr}");
   else scnt=$((scnt+1)); fi
fi

summary

