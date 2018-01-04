#!/usr/bin/csh
#SBATCH -J LISHydro
#SBATCH --nodes=1 --ntasks=24
#SBATCH --time=00:10:00
#SBATCH -o lishydro.%j
#SBATCH --account=s1657

setenv LISHYDRO_DIR $SLURM_SUBMIT_DIR/..
source $LISHYDRO_DIR/modules/modules.discover.intel14

#set RUNDIR=$SLURM_SUBMIT_DIR

mpirun -np 24 ./LISHydroApp >& log
