#!/usr/bin/csh

# Reset error flag
set ERROR=0

# Script Configuration
set DIR_CWD=`pwd`
set DIR_SETNGS="runsettings"
set DIR_RUNCFG="runconfig"
set DIR_RUNSCP="runscripts"

# Set System Specific Values
if ($HOSTNAME =~ *cheyenne*) then
    set DATA_ROOT="/glade/p/ral/hap/drosen/projects/LISHydro/data"
    set BATCH_SYS="qsub"
    set CPPERNODE=36
    set COMP_VERS="intel17"
    set SYST_NAME="cheyenne"
else if ($HOSTNAME =~ *discover*) then
    set DATA_ROOT="/discover/nobackup/projects/nu-wrf/lishydro/data"
    set BATCH_SYS="sbatch"
    set CPPERNODE=28
    set COMP_VERS="intel14"
    set SYST_NAME="discover"
else
    echo "ERROR: Unsupported machine - no data directory found."
    exit 1
endif

# Generate Compset List
set COMPSET_FILES=`find $DIR_SETNGS -maxdepth 1 -type f -name '*' | sort`
set COMPSET_LIST=`echo $COMPSET_FILES | sed "s/$DIR_SETNGS\///g"`
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
set COMPSET_SETTINGS="./$DIR_SETNGS/$COMPSET"
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

# Calculate Nodes Needed
if ( ! $?TASKS ) then
   echo "ERROR: TASKS must be set in $COMPSET_SETTINGS"
   exit 1
else
    set REMAINDER=`expr $TASKS % $CPPERNODE`
    if ($REMAINDER > 0) then
        set NODES=`expr $TASKS / $CPPERNODE + 1`
    else
        set NODES=`expr $TASKS / $CPPERNODE`
    endif
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
set LISHYDRO_RUNCONFIG="./$DIR_RUNCFG/lishydro.runconfig.$RUNCONFIG"
if (! -f $LISHYDRO_RUNCONFIG) then
    echo "ERROR: LISHYDRO_RUNCONFIG file is missing [$LISHYDRO_RUNCONFIG]"
    exit 1
endif

# Set LISHYDRO_RUNSCRIPT template file
set HOSTNAME=`hostname | sed 's/[0-9]*$//g'`
set LISHYDRO_RUNSCRIPT="./$DIR_RUNSCP/run.csh.$HOSTNAME"
if (! -f $LISHYDRO_RUNSCRIPT) then
    echo "ERROR: LISHYDRO_RUNSCRIPT file is missing [$LISHYDRO_RUNSCRIPT]"
    exit 1
endif

# Check LISHYDRO_DIR and set LISHYDRO_EXE
if (! $?LISHYDRO_DIR) then
  setenv LISHYDRO_DIR `dirname $DIR_CWD`
  echo "INFO : LISHYDRO_DIR automatically set [$LISHYDRO_DIR]"
endif
if (! -d $LISHYDRO_DIR) then
   echo "ERROR: LISHYDRO_DIR directory does not exist [$LISHYDRO_DIR]"
   exit 1
endif
set MODL_FILE=$LISHYDRO_DIR/modules/build.$SYST_NAME.$COMP_VERS
if (! -f $MODL_FILE) then
   echo "ERROR: Environment settings file does not exist [$MODL_FILE]"
   exit 1
else
  echo "INFO : Environment automatically set [$MODL_FILE]"
  source $MODL_FILE >& /dev/null
  if ($status != 0) then
    echo "ERROR: Environment settings file error [$MODL_FILE]"
    exit 1
  endif
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
      echo "ERROR: "$envvar" must be set in $COMPSET_SETTINGS"
      set ERROR=1
      continue
    else
      set value=`eval echo \$$envvar`
      sed -i "s/$setting/$value/g" $RUNDIR/lishydro.runconfig
    endif
end
if ($ERROR != 0) then
  echo "ERROR: Processing failed [$LISHYDRO_RUNCONFIG]"
  exit $ERROR
endif

# Copy batch script and process compset settings
cp $LISHYDRO_RUNSCRIPT $RUNDIR/run.csh
set settings=`grep -ow -e "__.*__" $RUNDIR/run.csh`
foreach setting (${settings})
    set envvar=${setting:as/__//}
    set value=`eval echo \$$envvar >& /dev/null`
    if ($status != 0) then
      echo "ERROR: "$envvar" must be set in $COMPSET_SETTINGS"
      set ERROR=1
      continue
    else
      set value=`eval echo \$$envvar`
      sed -i "s/$setting/$value/g" $RUNDIR/run.csh
    endif
end
if ($ERROR != 0) then
  echo "ERROR: Processing failed [$LISHYDRO_RUNSCRIPT]"
  exit $ERROR
endif

echo "Run directory set up in: $RUNDIR"
echo "To execute:"
echo "$ cd $RUNDIR"
echo "$ $BATCH_SYS run.csh"

