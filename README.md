# lishydro
Coupled hydrological application with LIS and WRF-Hydro

## Clone Instructions
All instructions and scripts are based on tcsh, so go
ahead and switch to it now.
```
$ tcsh
```

Clone the repository including the submodules:
```
$ git clone --recursive git@developer.nasa.gov:rsdunlap/lishydro.git
```

Since LIS is not yet available in git, a manual step is
required to check out LIS from subversion.  Hopefully, LIS
will be moved into git soon and this step will be eliminated.
```
$ cd lishydro    # go into cloned repository
$ svn co https://progress.nccs.nasa.gov/svn/lis/external/LIS_NEMS LIS
```

## Build Instructions

Set LISHYDRO_DIR to location of cloned repository.
```
$ setenv LISHYDRO_DIR /path/to/lishydro
```

Source the modules and environment variables used for the build
```
$ source env.discover.intel14   # load modules/env. vars for Intel 14
```

Build LIS
```
$ cd LIS
$ ./configure    # accept all the default options
$ cd runmodes/nuopc_cpl_mode
$ make nuopcinstall INSTPATH=$LISHYDRO_DIR/LIS-INSTALL
```

Build WRF-Hydro
```
$ cd wrf_hydro_nwm/trunk/NDHMS
$ ./configure   # select option 3: "Linux ifort compiler dmpar"
$ cd CPL/NUOPC_cpl
$ make nuopcinstall INSTPATH=$LISHYDRO_DIR/WRFHydro-INSTALL
```


