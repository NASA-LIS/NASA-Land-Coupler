#!/usr/bin/csh
#SBATCH -J LISHydro
#SBATCH --nodes=1 --ntasks=24
#SBATCH --time=00:10:00
#SBATCH -o lishydro.%j
#SBATCH --account=s1657

#setenv LISHYDRO_DIR /discover/nobackup/rsdunlap/lishydro3
#source /discover/nobackup/rsdunlap/lishydro3/env.discover.intel14

setenv LISHYDRO_DIR $SLURM_SUBMIT_DIR/..
source $LISHYDRO_DIR/modules/modules.discover.intel14

set RUNDIR=$SLURM_SUBMIT_DIR/$SLURM_JOB_NAME.$SLURM_JOB_ID
mkdir -p $RUNDIR
cd $RUNDIR

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

# Link executable
ln -sf $SLURM_SUBMIT_DIR/wrf_hydro.exe $RUNDIR/wrf_hydro.exe

mpirun -np 24 ./wrf_hydro.exe >& log
