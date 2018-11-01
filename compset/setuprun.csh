#!/usr/bin/csh

# Reset error flag
set ERROR=0

set COMPSET_FILES=`find . -maxdepth 1 -name 'settings.*'`
set COMPSET_LIST=`echo $COMPSET_FILES | sed "s/\.\/settings\./ /g"`
if ($#argv != 1) then
    echo "Usage $0 <compset>"
    echo "Available compsets:"
    foreach available (${COMPSET_LIST})
        echo "\t$available"
    end
    exit 1
else
    set COMPSET=$1
endif

# Include compset settings
set COMPSET_SETTINGS="./settings.$COMPSET"
if (! -f $COMPSET_SETTINGS) then
    echo "ERROR: Compset settings file is missing [$COMPSET_SETTINGS]"
    echo "Available compsets:"
    foreach available (${COMPSET_LIST})
        echo "\t$available"
    end
    exit 1
endif
source $COMPSET_SETTINGS >& /dev/null
if ($status != 0) then
   echo "ERROR: Compset settings file must be C Shell compatible [$COMPSET_SETTINGS]"
   exit 1
endif

# Check RUNCONFIG variable then set LISHYDRO_RUNCONFIG template file
if (! $?RUNCONFIG) then       
  echo "ERROR: RUNCONFIG variable is undefined. Check [$COMPSET_SETTINGS]"
  exit 1
endif
if ("$RUNCONFIG" == "")  then
  echo "ERROR: RUNCONFIG variable is blank. Check [$COMPSET_SETTINGS]"
  exit 1
endif
set LISHYDRO_RUNCONFIG="./runconfig/lishydro.runconfig.$RUNCONFIG"
if (! -f $LISHYDRO_RUNCONFIG) then
    echo "ERROR: LISHYDRO_RUNCONFIG file is missing [$LISHYDRO_RUNCONFIG]"
    exit 1
endif

# Set LISHYDRO_RUNSCRIPT template file
set HOSTNAME=`hostname | sed 's/[0-9]*$//g'`
set LISHYDRO_RUNSCRIPT="./run.csh.$HOSTNAME"
if (! -f $LISHYDRO_RUNSCRIPT) then
    echo "ERROR: LISHYDRO_RUNSCRIPT file is missing [$LISHYDRO_RUNSCRIPT]"
    exit 1
endif

# Check LISHYDRO_DIR and set LISHYDRO_EXE
if (! $?LISHYDRO_DIR) then       
  echo "ERROR: Please set LISHYDRO_DIR to root directory of cloned repository."
  exit 1
endif
if (! -d $LISHYDRO_DIR) then
   echo "ERROR: LISHYDRO_DIR directory does not exist [$LISHYDRO_DIR]"
   exit 1
endif
set LISHYDRO_EXE=$LISHYDRO_DIR/src/driver/LISHydroApp
if (! -f $LISHYDRO_EXE) then
   echo "ERROR: LISHydroApp executable does not exist."
   exit 1
endif

# Set RUNDIR and create RUNDIR
set RUNDIR=$LISHYDRO_DIR/run/$COMPSET
if (-d $RUNDIR) then
    echo "ERROR: Run directory already exists [$RUNDIR]"
    echo "Please delete or move and try again"
    exit 1
endif
mkdir -p $RUNDIR

# SET DATA_ROOT based on HOSTNAME
if ($HOSTNAME =~ *cheyenne*) then
    set DATA_ROOT=/glade/p/work/dunlap/data
else if ($HOSTNAME =~ *discover*) then
    set DATA_ROOT=/discover/nobackup/projects/nu-wrf/lishydro/data
else
    echo "ERROR: Unsupported machine - no data directory found."
    exit 1
endif

# WRF-Hydro setup
if ($RUNCONFIG =~ *hyd*) then
  set DATA_HYD=$DATA_ROOT/WRFHydro/$COMPSET
  if (-d $DATA_HYD) then
    cp $DATA_HYD/namelist.hrldas $RUNDIR
    cp $DATA_HYD/hydro.namelist  $RUNDIR
    cp $DATA_HYD/WRFHYDRO_PARMS/CHANPARM.TBL $RUNDIR
    ln -sf $DATA_HYD/WRFHYDRO_DOMAIN  $RUNDIR/WRFHYDRO_DOMAIN
    ln -sf $DATA_HYD/WRFHYDRO_FORCING $RUNDIR/WRFHYDRO_FORCING
    ln -sf $DATA_HYD/WRFHYDRO_PARMS   $RUNDIR/WRFHYDRO_PARMS
    ln -sf $DATA_HYD/WRFHYDRO_RESTART $RUNDIR/WRFHYDRO_RESTART
  else
    echo "ERROR: DATA_HYD directory not found [$DATA_HYD]"
    exit 1
  endif
endif

# LIS Setup
if ($RUNCONFIG =~ *lnd*) then
  set DATA_LND=$DATA_ROOT/LIS/$COMPSET
  if (-d $DATA_LND) then
    cp $DATA_LND/lis.config $RUNDIR
    ln -s $DATA_LND/LIS_DOMAIN $RUNDIR/LIS_DOMAIN
    ln -s $DATA_LND/LIS_INPUTS $RUNDIR/LIS_INPUTS
    ln -s $DATA_LND/LIS_RSTRT $RUNDIR/LIS_RSTRT
    ln -s $DATA_LND/LIS_FORCING $RUNDIR/LIS_FORCING
    ln -s $DATA_LND/NOAH33_PARMS $RUNDIR/NOAH33_PARMS
  else
    echo "ERROR: DATA_LND directory not found [$DATA_LND]"
    exit 1
  endif
endif

# Copy executable to RUNDIR
cp $LISHYDRO_EXE $RUNDIR/.

# Copy runconfig and process compset settings
cp $LISHYDRO_RUNCONFIG $RUNDIR/lishydro.runconfig
set settings=`grep -ow -e "__.*__" $RUNDIR/lishydro.runconfig`
foreach setting (${settings})
    set envvar=${setting:as/__//}
    set value=`eval echo \$$envvar >& /dev/null`
    if ($status != 0) then
      echo " ERROR: "$envvar" must be set in settings.$COMPSET"
      set ERROR=1
      continue
    else
      set value=`eval echo \$$envvar`
      sed -i "s/$setting/$value/g" $RUNDIR/lishydro.runconfig
    endif
end
if ($ERROR != 0) then
  echo "# Error processing $LISHYDRO_RUNCONFIG #"
  exit $ERROR
endif

# Copy batch script and process compset settings
cp $LISHYDRO_RUNSCRIPT $RUNDIR/run.csh
set settings=`grep -ow -e "__.*__" $RUNDIR/run.csh`
foreach setting (${settings})
    set envvar=${setting:as/__//}
    set value=`eval echo \$$envvar >& /dev/null`
    if ($status != 0) then
      echo " ERROR: "$envvar" must be set in settings.$COMPSET"
      set ERROR=1
      continue
    else
      set value=`eval echo \$$envvar`
      sed -i "s/$setting/$value/g" $RUNDIR/run.csh
    endif
end
if ($ERROR != 0) then
  echo "# Error processing $LISHYDRO_RUNSCRIPT #"
  exit $ERROR
endif

echo "Run directory set up in: $RUNDIR"
echo "To execute:"
echo "$ cd $RUNDIR"
echo "$ sbatch run.csh"

