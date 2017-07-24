

# R-Shiny implementation of GWSDAT

The Shiny UI can be started in Multi-Data mode or Single-Data mode.
The Multi-Data mode includes an extended user interface with a data manager. The Single-Data mode is limited to the analysis of a single data set (start from Excel VBA). 

## Usage

* Download/clone the directory and change into it. 
* Execute R in this directory. 
* Start Multi-Data Mode: 

```
> source("R/start_GWSDAT.R")
> start_GWSDAT()
```

* Start in Single-Data Mode by specifying input data:

```
> source("R/start_GWSDAT.R")
> GWSDAT_Options <- create_GWSDAT_Instance()                  # Create a GWSDAT_Options list. 
> GWSDAT_Options$WellDataFilename <- 'WellData_file.csv'      # Change any options. 
> GWSDAT_Options$WellCoordsFilename <- 'WellCoord_file.csv'
> start_GWSDAT(GWSDAT_Options)                                # Pass the options list: single-data mode is invoked.
```
