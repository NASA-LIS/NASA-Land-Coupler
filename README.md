# lishydro
Coupled hydrological application with LIS and WRF-Hydro

## SSH keys
We are using ssh URLs for cloning submodules, so you will
need to ensure that you have added your public SSH key from
your platform (Discover and/or Cheyenne) to your GitHub account.
Help is available here:
https://help.github.com/articles/connecting-to-github-with-ssh/

## Clone Instructions
These have been tested on **Discover (NASA)** and 
**Cheyenne (NCAR)**.
All instructions and scripts are based on tcsh, so go
ahead and switch to it now.
```
$ tcsh
```

Clone the repository including the submodules. 
```
$ git clone --recursive git@github.com:NESII/lishydro.git

# this repository is also available on internal NASA GitHub
# (must be on NASA network or have VPN access)
# Using this repo is NOT recommended as most new development
# occurs on the github.com repo, but this is listed here
# for completeness, or in case an internal NASA version is needed
$ git clone --recursive git@developer.nasa.gov:rsdunlap/lishydro.git
```

Set LISHYDRO_DIR to location of cloned repository.
```
$ setenv LISHYDRO_DIR /path/to/lishydro
```

Source the modules and environment variables used for the build.
There are options here for Discover and Cheyenne. Choose the
right one for your platform.

**On Discover**:
```
$ source $LISHYDRO_DIR/modules/build.discover.intel14 
```

**On Cheyenne**:
```
$ source $LISHYDRO_DIR/modules/build.cheyenne.intel17
```
LIS and WRF-Hydro are included as git submodules. The following
commands provide an alternative to using the --recursive git
clone option.
```
$ cd $LISHYDRO_DIR        # go into the cloned repository
$ git submodule init
$ git submodule update
```

## Build Instructions

**Build LIS**
```
$ cd $LISHYDRO_DIR/src/LISF/lis
$ ./configure    # accept all the default options
$ cd runmodes/nuopc_cpl_mode
$ make nuopcinstall INSTPATH=$LISHYDRO_DIR/LIS-INSTALL
```

**Build WRF-Hydro**
```
$ cd $LISHYDRO_DIR/src/wrf_hydro_nwm/trunk/NDHMS
$ ./configure   # select option 3: "ifort intel parallel"
$ cd CPL/NUOPC_cpl
$ make nuopcinstall INSTPATH=$LISHYDRO_DIR/WRFHydro-INSTALL
```

**Build Driver**
```
$ cd $LISHYDRO_DIR/src/driver
$ make
```

## Run Instructions

**NOTE:  Runs are currently only supported on Discover and Cheyenne.**

Individual configurations are called compsets and are
located in the $LISHYDRO_DIR/compset directory. Compsets
have the naming convention:  *runsettings/&lt;compset&gt;*.

Current supported compsets:

| Compset                      | Description                                              |
| ---------------------------- | -------------------------------------------------------- |
| frontrange.ldas              | WRF-Hydro standalone forced by LDAS output               |
| irene.nldas2                 | LIS standalone forced by NLDAS                           |
| coupled_tuolumne.noah.nldas2 | Coupled LIS, Mediator, and WRF-Hydro forced by NLDAS     |

**Setup Run Directory**
```
$ cd $LISHYDRO_DIR/compset
$ ./setuprun.csh <compset>
```
This will create and populate a run directory in 
*$LISHYDRO_DIR/run/&lt;compset&gt;*.

**Submit Run**

The batch script *run.csh* can be modified if needed, for example
to change the number of MPI tasks or the project number.
```
$ cd $LISHYDRO_DIR/run/<compset>
$ sbatch run.csh   # Discover - SLURM
$ qsub run.csh     # Cheyenne - PBS
```
This will submit the run to the batch queue.  Output
will appear in the same directory.

