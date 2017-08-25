

# GWSDAT (shiny)

R package of the GroundWater Spatiotemporal Data Analysis Tool (GWSDAT) for the analysis of groundwater monitoring data. 

## Usage 

* Run directly from github inside an R session: shiny::runGitHub('Shiny_GWSDAT', 'andrejadd')
* Download/clone the directory from github, unpack and run inside an R session: shiny::runApp()


## Usage ExcelMode

The 'ExcelMode' provides a slim version of the UI for exploring the data from the GWSDAT Excel AddOn. 

* Download/clone the directory and save the path (GWSDATHome).
* Create a list 'GWSDAT_Options' and populate with options, then

```R
setwd('path_to_gwsdat_folder')
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


