# lishydro
Coupled surface-hydrology application with data assimilation.

## Components
**LIS-Hydro Driver and Mediator**:
The LIS-Hydro driver and mediator have been designed in collaboration by NOAA,
NASA, and NCAR. The custom driver couples LIS and WRF-Hydro within the NUOPC
framework. Coupled system configuration is controlled via settings in the
*lishydro.runconfig* file. The custom mediator couples an in memory ensemble to
multiple instances of a component, which allows users to run a coupled ensemble
entirely in memory. Each component is individually configured based on the needs
of the component (see documentation for each component).

**Land Information System (LIS)**:
LIS is a land surface modeling framework devloped by NASA.<br/>
<code>Documentation: https://lis.gsfc.nasa.gov/</code><br/>
<code>Repository: https://github.com/NASA-LIS/LISF</code>

**WRF-Hydro**:
WRF-Hydro is a hydrometeorological and hydrologic modeling system
developed by the research applications labratory at NCAR.<br/>
<code>Documentation: https://ral.ucar.edu/projects/wrf_hydro/overview</code><br/>
<code>Repository: https://github.com/NCAR/wrf_hydro_nwm_public</code>

## SSH keys
We are using ssh URLs for cloning submodules, so you will
need to ensure that you have added your public SSH key from
your platform (Discover and/or Cheyenne) to your GitHub account.
Help is available here:
https://help.github.com/articles/connecting-to-github-with-ssh/

## Clone Instructions
These have been tested on **Discover (NASA)** and 
**Cheyenne (NCAR)**.

Clone the repository including the submodules. 
```
$ git clone --recursive git@github.com:NESII/lishydro.git <directory>
```

LIS and WRF-Hydro are included as git submodules. The following
commands provide an alternative to using the --recursive git
clone option.
```
$ cd <directory>        # go into the cloned repository
$ git submodule init
$ git submodule update
```

## Build Instructions

**Configure Submodules**
Configure LIS and WRFHYDRO using the *configure.sh* script. For more information
run `./configure.sh --help`.
```
$ ./configure.sh
```

**Build Application**
Build LISHydroApp using the *build.sh* script. For more information run
`./build.sh --help`. LISHydroApp will be installed into src/driver.
```
$ ./build.sh
```

## Run Instructions

**NOTE:  Runs are currently only supported on Discover and Cheyenne.**

Use cases are preconfigured using settings in *usecases* directory and 
preprocessed data. Input data, including configuration, domain, 
and parameter files, is stored on Discover and Cheyenne 

Current supported use cases:

| Use Cases                                          | Description                                                  |
| -------------------------------------------------- | ------------------------------------------------------------ |
| coupled\_irene.noahmp.nldas2                       | Coupled LIS-WRFHYDRO Hurricane Irene NoahMP v3.6             |
| coupled\_irene.noah.nldas2                         | Coupled LIS-WRFHYDRO Hurricane Irene Noah v3.6               |
| coupled\_irene\_noMed.noah.nldas2                  | Directly Coupled LIS-WRFHYDRO Hurricane Irene (No MED)       |
| coupled\_tar.noahmp\_v4.0.1\_cold.nldas2           | Coupled LIS-WRFHYDRO Tar River NoahMP v4.0.1                 |
| coupled\_tuolumne.ens002.noahmp.nldas2             | Coupled LIS-WRFHYDRO Tuolumne NoahMP v4.0.1 Ensemble         |
| coupled\_tuolumne.ens020.noahmp\_v4.0.1\_da.nldas2 | Coupled LIS-WRFHYDRO Tuolumne NoahMP v4.0.1 Ensemble with DA |
| coupled\_tuolumne.noahmp.nldas2                    | Coupled LIS-WRFHYDRO Tuolumne NoahMP v3.6                    |
| coupled\_tuolumne.noahmp\_v4.0.1\_cold.nldas2      | Coupled LIS-WRFHYDRO Tuolumne NoahMP v4.0.1                  |
| coupled\_tuolumne.noahmp\_v4.0.1\_da.nldas2        | Coupled LIS-WRFHYDRO Tuolumne NoahMP v4.0.1 with DA          |
| coupled\_tuolumne.noah.nldas2                      | Coupled LIS-WRFHYDRO Tuolumne Noah v3.6                      |
| coupled\_tuolumne\_noMed.noah.nldas2               | Directly Coupled LIS-WRFHYDRO Tuolumne (No MED)              |
| sbys\_tuolumne\_hyd.ens002.ldas                    | Uncoupled LIS-WRFHYDRO Tuolumne Ensemble (2 member)          |
| stdalone\_frontrange\_hyd.ldas                     | Standalone WRFHYDRO Frontrange                               |
| stdalone\_irene\_lnd.nldas2                        | Standalone LIS Hurricane Irene                               |
| stdalone\_tuolumne\_hyd.ldas                       | Standalone WRFHYDRO Tuolumne                                 |


**Setup Run Directory**
Preconfigured use cases can be set up using the *setuprun.sh* script.
For more information run `./setuprun.sh --help`. For a list of use cases
run `./setuprun.sh --list-usecases`.
```
$ ./setuprun.sh USECASE
```
This will create and populate a run directory in 
*run/&lt;usecase&gt;*.

**Submit Run**

The batch script *run.sh* can be modified if needed, for example
to change the number of MPI tasks or the project number.
```
$ cd run/<usecase>
$ sbatch run.sh   # Discover - SLURM
$ qsub run.sh     # Cheyenne - PBS
```
This will submit the run to the batch queue.  Output
will appear in the same directory.

