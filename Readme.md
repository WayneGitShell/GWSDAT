

# GWSDAT (shiny)

R package of the GroundWater Spatiotemporal Data Analysis Tool (GWSDAT) for the analysis of groundwater monitoring data. 

This app can operate in server mode supporting multiple data set, and Excel Mode supporting a single data set.

## Install and Run

Open an R session (requires devtools):

```r
devtools::install_github("andrejadd/GWSDAT")
library(GWSDAT)
launchApp()
```

As an alternative, download/clone the folder and change into it:

```r
devtools::load_all()
launchApp()
```


## Run in Excel Mode

The Excel Mode provides a slim version of the UI for exploring a single data set. Install the package as above and pass a GWSDAT_Options list to launchApp(). 

```r
library(GWSDAT)
launchApp(GWSDAT_Options)
```


## Save & Load Session File

A session can be saved in the Analysis panel, in the top navigation bar under _More_ -> _Save Session_.

To load the session start GWSDAT with

```r
library(GWSDAT)
launchApp(session_file = "path_to_file.RData")
``` 




