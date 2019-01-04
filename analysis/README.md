# Analysis Tools
Analysis tools can be used to generate visuals of application output and diff files.

## Plot HYDRO Channel Routing Files
Script:
```
FERRET_HYDRO_CHRTOUT.jnl
```
Usage:
```
$ ferret -gif -script FERRET_HYDRO_CHRTOUT.jnl [CHRTOUT file] [prefix]
```

## Plot HYDRO Restart Diff Files
Script:
```
FERRET_HYDRO_RST_IGNORE0.jnl
```
Usage:
```
$ ferret -gif -script FERRET_HYDRO_RST_IGNORE0.jnl [HYDRO_RST file] [prefix]
```

## Plot LIS Output Files
Script:
```
FERRET_LIS_HIST.jnl
```
Usage:
```
$ ferret -gif -script FERRET_LIS_HIST.jnl [LIS_HIST file] [prefix]
```

## Plot LIS Restart Diff Files
Script:
```
FERRET_LIS_RST_NOAH33_IGNORE0.jnl
```
Usage:
```
$ ferret -gif -script FERRET_LIS_RST_NOAH33_IGNORE0.jnl [LIS_RST file] [prefix]
```
