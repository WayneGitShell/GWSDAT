# GWSDAT

This is the R Shiny application of the GroundWater Spatiotemporal Data Analysis Tool (GWSDAT) for the analysis of groundwater monitoring data. This package can be run locally or can be deployed on a Shiny Server. For more background see <http://www.gwsdat.net>. 
<!--- supporting multiple data sets and users, and a stand-alone mode (Excel Mode), which is limited to a single data set.and --->

## Install and Run

For latest development version, install from github using the `devtools` R package:

```r
devtools::install_github("WayneGitShell/GWSDAT")
options(shiny.useragg = FALSE) #to avoid artefacts in spatial plots
GWSDAT::launchApp()
```

Or install from CRAN: 

```r
install.packages("GWSDAT")
options(shiny.useragg = FALSE) #to avoid artefacts in spatial plots
GWSDAT::launchApp()
```
A couple of example data sets are already pre-loaded. See full options for interactive data input in section 6.1 of the GWSDAT user manual: <http://www.gwsdat.net/gwsdat_manual>

## Data input directly from R. 
<!---#The Stand-Alone Mode provides a slim version of the UI for exploring a single data set.  --->
Install the package as above and pass a `GWSDAT_Options` list to `launchApp()`. The method `GWSDAT::createOptions()` creates this `GWSDAT_Options` list. The `GWSDAT_Options` list must either contain the `WellData` and `WellCoords` data.frames: 

```r
library(GWSDAT)
# Define GWSDAT_Options
opt <- createOptions("Site Name")
opt$WellData <- read.csv(system.file("extdata","ComprehensiveExample_WellData.csv",package="GWSDAT"))
opt$WellCoords <- read.csv(system.file("extdata","ComprehensiveExample_WellCoords.csv",package="GWSDAT"))
launchApp(opt)
``` 
or the location of input csv files `WellDataFilename` and `WellCoordsFilename`. 

```r
library("GWSDAT")
# Define GWSDAT_Options
GWSDAT_Options <- createOptions("Example Site")
GWSDAT_Options$WellDataFilename <- system.file("extdata","BasicExample_WellData.csv",package="GWSDAT")
GWSDAT_Options$WellCoordsFilename <- system.file("extdata","BasicExample_WellCoords.csv",package="GWSDAT")
launchApp(GWSDAT_Options)
```
<!---
The `GWSDAT_Options` list must define the elements `WellDataFilename` and `WellCoordsFilename`. The method `GWSDAT::createOptions()` creates this `GWSDAT_Options` list.

```r
library(GWSDAT)
# Define GWSDAT_Options
opt <- createOptions("Site Name")
opt$WellDataFilename <- 'path_to_concentration_file'
opt$WellCoordsFilename <- 'path_to_well_coordinate_file'
launchApp(opt)
```
--->

## Save & Load Session File

Inside the Shiny app, a data analysis session can be saved inside the Analysis panel, in the top navigation bar under _More_ -> _Save Session_. This creates a `.rds` file that can be loaded with the `launchApp()` method:


```r
GWSDAT::launchApp(session_file = "path_to_file.rds")
``` 


## Deploy on Posit Connect

Create the file `app.R` with the following content and publish to Posit Connect server in the usual manner:

```r
library("GWSDAT")
launchApp()
```
