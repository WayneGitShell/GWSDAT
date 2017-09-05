

# GWSDAT (shiny)

R package of the GroundWater Spatiotemporal Data Analysis Tool (GWSDAT) for the analysis of groundwater monitoring data. 

This app can be deployed on a Shiny Server supporting multiple data sets and users, and a stand-alone mode (Excel Mode) that allows to explore a single data set.

## Install and Run

Open an R session (requires R package `devtools`):

```r
devtools::install_github("andrejadd/GWSDAT")
launchApp()
```

As an alternative, download/clone the folder and change into it:

```r
devtools::load_all()
launchApp()
```


## Deploy on Shiny Server

On the server, install the package with `devtools::install_github("andrejadd/GWSDAT")`. Create a directory `GWSDAT` inside the shiny app folder defined in `/etc/shiny-server/shiny-server.conf`. Create the file `GWSDAT/app.R` with the following content:

```r
library(GWSDAT)
launchApp()
```


## Run in Stand-Alone Mode (ExcelMode)

The Stand-Alone Mode provides a slim version of the UI for exploring a single data set. Install the package as above and pass a `GWSDAT_Options` list to `launchApp()`. 

```r
library(GWSDAT)
# Define GWSDAT_Options
# ..
launchApp(GWSDAT_Options)
```

The `GWSDAT_Options` list must define the elements `WellDataFilename` and `WellCoordsFilename`. A short-cut to creating `GWSDAT_Options` and these elements would be: 

```r
library(GWSDAT)
opt <- createOptions("Site Name")
opt$WellDataFilename <- 'path_to_concentration_file'
opt$WellCoordsFilename <- 'path_to_well_coordinate_file'
launchApp(opt)
``` 

## Save & Load Session File

A session can be saved in the Analysis panel, in the top navigation bar under _More_ -> _Save Session_.

To load the session start GWSDAT with

```r
GWSDAT::launchApp(session_file = "path_to_file.RData")
``` 




