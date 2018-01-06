#!/usr/bin/csh

if ("$1" == "") then
    echo "Usage setuprun.csh <compset>"
    exit 1
endif

set COMPSET=$1
set LISHYDRO_RUNCONFIG="./lishydro.runconfig.$COMPSET"

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

set RUNDIR=$LISHYDRO_DIR/run/$COMPSET
if (-d $RUNDIR) then
    echo "Error: Run directory already exists: $RUNDIR"
    echo "Please delete or move and try again"
    exit 1
endif

set HOSTNAME=`hostname`
if ($HOSTNAME =~ *cheyenne*) then
    set DATA_ROOT=/glade/p/work/dunlap/data
else if ($HOSTNAME =~ *discover*) then
    set DATA_ROOT=$NOBACKUP/data
else
    echo "Unsupported machine - no data directory found."
    exit 1
endif

mkdir -p $RUNDIR

# WRF-Hydro setup
set DATA_HYD=$DATA_ROOT/WRFHydro/$COMPSET
if (-d $DATA_HYD) then
    cp $DATA_HYD/namelist.hrldas $RUNDIR
    cp $DATA_HYD/hydro.namelist  $RUNDIR
    cp $DATA_HYD/CHANPARM.TBL    $RUNDIR
    cp $DATA_HYD/GENPARM.TBL     $RUNDIR
    cp $DATA_HYD/HYDRO.TBL       $RUNDIR
    cp $DATA_HYD/MPTABLE.TBL     $RUNDIR
    cp $DATA_HYD/SOILPARM.TBL    $RUNDIR
    ln -sf $DATA_HYD/DOMAIN      $RUNDIR/DOMAIN
    ln -sf $DATA_HYD/FORCING     $RUNDIR/FORCING
endif

# LIS Setup
set DATA_LND=$DATA_ROOT/LIS/$COMPSET
if (-d $DATA_LND) then
    cp $DATA_LND/lis.config $RUNDIR
    ln -s $DATA_LND/LIS_DOMAIN $RUNDIR/LIS_DOMAIN
    ln -s $DATA_LND/LIS_INPUTS $RUNDIR/LIS_INPUTS
    ln -s $DATA_LND/LIS_RSTRT $RUNDIR/LIS_RSTRT
    ln -s $DATA_LND/LIS_FORCING $RUNDIR/LIS_FORCING
    ln -s $DATA_LND/NOAH33_PARMS $RUNDIR/NOAH33_PARMS
endif

# Copy runconfig, executable and batch script
cp $LISHYDRO_RUNCONFIG $RUNDIR/lishydro.runconfig 
cp $LISHYDRO_EXE $RUNDIR
# TODO: Currently assumes we are running on Discover
cp run.csh.discover $RUNDIR/run.csh

echo "Run directory set up in: $RUNDIR"
echo "To execute:"
echo "$ cd $RUNDIR"
echo "$ sbatch < run.csh"

