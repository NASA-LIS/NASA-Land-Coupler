#!/usr/bin/csh

if ("$1" == "") then
    echo "Usage setuprun.csh <compset>"
    exit 1
endif

set LISHYDRO_RUNCONFIG="./lishydro.runconfig.$1"

if (! -f $LISHYDRO_RUNCONFIG) then
    echo "There is no compset named $1"
    exit 1
endif

if (! $?LISHYDRO_DIR) then       
  echo "Error: Please set LISHYDRO_DIR to root directory of cloned repository."
  exit 1
endif

if (! -d $LISHYDRO_DIR) then
   echo "Error:  $LISHYDRO_DIR directory does not exist."
   exit 1
endif

set LISHYDRO_EXE=$LISHYDRO_DIR/src/driver/LISHydroApp

if (! -f $LISHYDRO_EXE) then
   echo "Error:  LISHydroApp executable does not exist."
   exit 1
endif

set COMPSET=$1

set RUNDIR=$LISHYDRO_DIR/run/$COMPSET
if (-d $RUNDIR) then
    echo "Error: Run directory already exists: $RUNDIR"
    echo "Please delete or move and try again"
    exit 1
endif

mkdir -p $RUNDIR

# WRF-Hydro setup
set DATA_HYD=$NOBACKUP/data/WRFHydro/frontrange
cp $DATA_HYD/namelist.hrldas $RUNDIR
cp $DATA_HYD/hydro.namelist  $RUNDIR
cp $DATA_HYD/CHANPARM.TBL    $RUNDIR
cp $DATA_HYD/GENPARM.TBL     $RUNDIR
cp $DATA_HYD/HYDRO.TBL       $RUNDIR
cp $DATA_HYD/MPTABLE.TBL     $RUNDIR
cp $DATA_HYD/SOILPARM.TBL    $RUNDIR
ln -sf $DATA_HYD/DOMAIN      $RUNDIR/DOMAIN
ln -sf $DATA_HYD/FORCING     $RUNDIR/FORCING

# LIS Setup

# Copy runconfig, executable and batch script
cp $LISHYDRO_RUNCONFIG $RUNDIR/lishydro.runconfig 
cp $LISHYDRO_EXE $RUNDIR
cp run.csh $RUNDIR

echo "Run directory set up in: $RUNDIR"
