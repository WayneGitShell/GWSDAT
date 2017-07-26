
source("R/GWSDAT_Setup.R")


# Print warnings when they happen.
options(warn = 1)


start_GWSDAT <- function(GWSDAT_Options = NULL) {

  # Load the sources and libraries.
  GWSDAT_Setup()

  # If a GWSDAT_Options object was not defined, create one here.
  if (is.null(GWSDAT_Options)) {
    
    GWSDAT_Options = create_GWSDAT_Instance()
  
    
    # Change some Option for testing.
    GWSDAT_Options[['SiteName']] <- 'Comprehensive Example'
    GWSDAT_Options[['WellDataFilename']] <- 'data/ComprehensiveExample_WellData.csv'
    GWSDAT_Options[['WellCoordsFilename']] <- 'data/ComprehensiveExample_WellCoords.csv'
    GWSDAT_Options[['ShapeFileNames']] <- c(GWSDAT_Options[['ShapeFileNames']],'data/GIS_Files/GWSDATex2.shp')
    
  } else {

    # This will cause a the data analyse window to appear.       
    if (!GWSDAT_Options$Excel_Mode)
      GWSDAT_Options[['Excel_Mode']] <- TRUE
      
  }
  
  
  
   
  
  # Initialize the data and do the fitting.
  curr_site = GWSDAT_Init(GWSDAT_Options)
  
  
  # Get return status and display.
  if (class(curr_site) == "GWSDAT_Error")
    stop(ret$status$msg)
  if (class(curr_site) == "GWSDAT_Warning")
    stop(ret$status$msg)

  
  # Create a complete GWSDAT instance with data, model, and options. 
  pnl <- Create_PanelAttr(curr_site)
  
  
  # Put into global environment, so the shiny server can see it. 
  .GlobalEnv$pnl <- pnl
  
  
  # on.exit(rm(pnl, envir = .GlobalEnv))
  
  runApp()
  
  
}

start_GWSDAT()