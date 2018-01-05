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
$ git clone --recursive git@developer.nasa.gov:rsdunlap/lishydro.git
```

Set LISHYDRO_DIR to location of cloned repository.
```
$ setenv LISHYDRO_DIR /path/to/lishydro
```

Since LIS is not yet available in git, a manual step is
required to check out LIS from subversion.  Hopefully, LIS
will be moved into git soon and this step will be eliminated.
```
$ cd $LISHYDRO_DIR/src    # go into src directory of cloned repository
$ svn co --username=<you> https://progress.nccs.nasa.gov/svn/lis/external/LIS_NEMS LIS
```

## Build Instructions

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

**Build LIS**
```
$ cd $LISHYDRO_DIR/src/LIS
$ ./configure    # accept all the default options
$ cd runmodes/nuopc_cpl_mode
$ make nuopcinstall INSTPATH=$LISHYDRO_DIR/LIS-INSTALL
```

**Build WRF-Hydro**
```
$ cd $LISHYDRO_DIR/src/wrf_hydro_nwm/trunk/NDHMS
$ ./configure   # select option 3: "Linux ifort compiler dmpar"
$ cd CPL/NUOPC_cpl
$ make nuopcinstall INSTPATH=$LISHYDRO_DIR/WRFHydro-INSTALL
```

**Build Driver**
```
$ cd $LISHYDRO_DIR/src/driver
$ make
```

## Run Instructions

**NOTE:  Runs are currently only supported on Discover.  Support for Cheyenne
  is forthcoming.**

Individual configurations are called compsets and are
located in the $LISHYDRO_DIR/compset directory. Compsets
have the naming convention:  *lishydro.runconfig.&lt;compset&gt;*.

Current supported compsets:
| Compset                   | Description                                                 |
| ------------------------- | ----------------------------------------------------------- |
| frontrange.ldas           | WRF-Hydro standalone forced by LDAS output                  |
| irene.nldas2              | LIS standalone forced by NLDAS                              |

**Setup Run Directory**
```
$ cd $LISHYDRO_DIR/compset
$ ./setuprun.csh <compset>
```
This will create and populate a run directory in 
$LISHYDRO_DIR/run/&lt;compset&gt;.

**Submit Run**

```
$ cd $LISHYDRO_DIR/run/<compset>
$ sbatch < run.csh   # only works on Discover now
```
This will submit the run to the batch queue.  Output
will appear in the same directory.

