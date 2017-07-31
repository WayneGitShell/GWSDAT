

# GWSDAT Shiny Implementation

GWSDAT can be started in Server mode or Excel mode. 

The Server mode provides an extended interface with a data manager and analysis tab. It can run on Shiny Server (tested on Ubuntu 16.04 LTS, 64bit). Authentification is not yet implemented. 

The Excel mode is limited to the analysis tab and a single site that has to be specified prior to starting the app. 

## Usage Server Mode

* Download/clone the directory to the Shiny Server app directory (e.g. in /srv/shiny-server/).
* Navigate to website, e.g. 127.0.0.1:3838/Shiny_GWSDAT, to start the app.

## Usage Excel Mode

* Download/clone the directory and save the path (GWSDATHome).
* Create a list 'GWSDAT_Options' and populate with options, then

```R
setwd(GWSDATHome)
source("R/startExcelMode.R")
startExcelMode(GWSDAT_Options)                                
```

* As an alternative, create a template of GWSDAT_Options:

```R
setwd(GWSDATHome)
source("R/createOptionsR")
GWSDAT_Options <- createOptions()
# make any changes to GWSDAT_Options 
# ...
source("R/startExcelMode.R")
startExcelMode(GWSDAT_Options)                                
```

## Notes on Server Mode

When the server executes the app the GWSDAT_Options$HeadlessMode flag is set to TRUE. This will load the required R packages from the systems R library. Since the current version is not an R package, the user needs to install.packages() all dependencies. However, this will change in the near future. Linux R package binaries are not provided (as for Windows) because of the differences in Linux Server distributions. 

