# Analysis Toolkit
Analysis tools can be used to generate visuals of application output and diff files.

## Compare LIS Hydro files using the NetCDF Operator Toolkit
LIS Hydro output is compared between baseline and test directories for LIS history files and WRF-Hydro channel routing files. The script calls ncdiff to generate the difference between baseline and test directories. The script then calls ncwa to generate the maximum absolute value for specified variables. If the maximum absolute value is not zero then the script will report an error. The difference files are stored for further analysis. By default this script will compare the final LIS history files and final WRF-Hydro channel routing files.

Script:
```
COMPARE_LISHYDRO_NETCDF.sh
```
Usage:
```
$ ./COMPARE_LISHYDRO_NETCDF.sh [-h] [-a] [-d DATE] BASE_DIRECTORY TEST_DIRECTORY
```

## Generate LIS Hydro Plots using NOAA Ferret Utility
LIS Hydro plots are generated for LIS history files and WRF-Hydro channel routing files. The script calls Ferret using scripts designed for LIS history files and WRF-Hydro channel routing files. By default this script will plot the final LIS history file and final WRF-Hydro channel routing file.

Script:
```
GENERATE_LISHYDRO_PLOTS.sh
```
Usage:
```
$ ./GENERATE_LISHYDRO_PLOTS.sh [-h] [-a] [-p PREFIX] [-d DIRECTORY] RUN_DIRECTORY
```

### Plot LIS Output Files
Script:
```
FERRET_LIS_HIST.jnl
```
Usage:
```
$ ferret -gif -script FERRET_LIS_HIST.jnl [LIS_HIST file] [prefix]
```

### Plot HYDRO Channel Routing Files
Script:
```
FERRET_HYDRO_CHRTOUT.jnl
```
Usage:
```
$ ferret -gif -script FERRET_HYDRO_CHRTOUT.jnl [CHRTOUT file] [prefix]
```

## Other Utilities

### Plot LIS Restart Diff Files
Script:
```
FERRET_LIS_RST_NOAH33_IGNORE0.jnl
```
Usage:
```
$ ferret -gif -script FERRET_LIS_RST_NOAH33_IGNORE0.jnl [LIS_RST file] [prefix]
```

### Plot HYDRO Restart Diff Files
Script:
```
FERRET_HYDRO_RST_IGNORE0.jnl
```
Usage:
```
$ ferret -gif -script FERRET_HYDRO_RST_IGNORE0.jnl [HYDRO_RST file] [prefix]
```

