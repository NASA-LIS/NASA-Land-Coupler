# lishydro
Coupled hydrological application with LIS and WRF-Hydro

## Clone Instructions
This will clone the repository including the submodules.
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
Switch to tcsh shell
```
$ tcsh
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
$ make
```

Build WRF-Hydro
```
$ cd wrf_hydro_nwm/trunk/NDHMS
$ ./configure   # select option 3: "Linux ifort compiler dmpar"
$ cd CPL/NUOPC_cpl
$ make
```


