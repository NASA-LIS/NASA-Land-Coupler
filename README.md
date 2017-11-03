# lishydro
Coupled hydrological application with LIS and WRF-Hydro

## Clone Instructions
This will clone the repository including the submodules.
```
$ git clone --recursive git@developer.nasa.gov:rsdunlap/lishydro.git
```

## Build Instructions
Switch to tcsh shell
```
$ tcsh
```

Source the modules and environment variables used for the build
```
$ cd lishydro   # go into cloned repository
$ source env.discover.intel14   # load modules/env. vars for Intel 14
```

Build WRF-Hydro
```
# configure
$ cd wrf_hydro_nwm/trunk/NDHMS
$ ./configure   # select option 3: "Linux ifort compiler dmpar"

# compile
$ cd CPL/NUOPC_cpl
$ make
```
