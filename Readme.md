

# R-Shiny implementation of GWSDAT

The Shiny UI can be started in Multi-Data mode or Single-Data mode.
The Multi-Data mode includes an extended user interface with a data manager. The Single-Data mode is limited to the analysis of a single data set (start from Excel VBA). 

## Usage

* Download/clone the directory and change into it. 
* Execute R in this directory. 
* Start Multi-Data Mode: 

```R
source("R/start_GWSDAT.R")
start_GWSDAT()
```

* Start in Single-Data Mode by specifying ExcelMode <- TRUE. More Options are listed inside create_GWSDAT_Instance.R.

```R
source("R/start_GWSDAT.R")
GWSDAT_Options <- create_GWSDAT_Instance()                   
GWSDAT_Options$ExcelMode <- TRUE
GWSDAT_Options$WellDataFilename <- 'WellData_file.csv'      # Change well data file. 
GWSDAT_Options$WellCoordsFilename <- 'WellCoord_file.csv'   # Change coordinates file.
start_GWSDAT(GWSDAT_Options)                                
```
