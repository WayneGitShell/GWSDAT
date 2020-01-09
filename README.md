# GWSDAT

This is the R Shiny application of the GroundWater Spatiotemporal Data Analysis Tool (GWSDAT) for the analysis of groundwater monitoring data. 

This app can be deployed on a Shiny Server supporting multiple data sets and users, and a stand-alone mode (Excel Mode), which is limited to a single data set.

## Install and Run

For latest development version, install from github using the `devtools` R package:

```r
devtools::install_github("andrejadd/GWSDAT")
GWSDAT::launchApp()
```

Or install from CRAN: 

```r
install.packages("GWSDAT")
GWSDAT::launchApp()
```


## Deploy on Shiny Server

On the server, install the package as shown above and create a directory `GWSDAT` inside the Shiny app folder defined in `/etc/shiny-server/shiny-server.conf`. Create the file `GWSDAT/app.R` with the following content:

```r
library("GWSDAT")
launchApp()
```

## Run in Stand-Alone Mode (ExcelMode)

The Stand-Alone Mode provides a slim version of the UI for exploring a single data set. Install the package as above and pass a `GWSDAT_Options` list to `launchApp()`. 

```r
library("GWSDAT")
# Define GWSDAT_Options
# ..
launchApp(GWSDAT_Options)
```

The `GWSDAT_Options` list must define the elements `WellDataFilename` and `WellCoordsFilename`. The method `GWSDAT::createOptions()` creates this `GWSDAT_Options` list.

```r
library(GWSDAT)
opt <- createOptions("Site Name")
opt$WellDataFilename <- 'path_to_concentration_file'
opt$WellCoordsFilename <- 'path_to_well_coordinate_file'
launchApp(opt)
``` 

## Save & Load Session File

Inside the Shiny app, a data analysis session can be saved inside the Analysis panel, in the top navigation bar under _More_ -> _Save Session_. This creates a `.rds` file that can be loaded with the `launchApp()` method:


```r
GWSDAT::launchApp(session_file = "path_to_file.rds")
``` 

