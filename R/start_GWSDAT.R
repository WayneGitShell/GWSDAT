
source("R/GWSDAT_Setup.R")
source("R/create_GWSDAT_Instance.R")


# Print warnings when they happen.
options(warn = 1)


start_GWSDAT <- function(GWSDAT_Options = NULL) {

 
  # Setting this to TRUE will run Shiny in multi-data mode (with data manager and dashboard)
  # Setting this to FALSE will run it in single data mode (for Excel).
  RUN_SINGLE_INSTANCE = TRUE
 
  
  # If a GWSDAT_Options object was not defined, create one here.
  if (is.null(GWSDAT_Options)) {
    
    GWSDAT_Options = create_GWSDAT_Instance()
    RUN_SINGLE_INSTANCE = FALSE
    
  } 
  
  
  #
  # Change some Option for testing.
  # 
  GWSDAT_Options[['WellDataFilename']] <- 'data/ComprehensiveExample_WellData.csv'
  GWSDAT_Options[['WellCoordsFilename']] <- 'data/ComprehensiveExample_WellCoords.csv'
  
  
  
  # Load the sources and libraries.
  GWSDAT_Setup() # GWSDAT_Options$GWSDATHome, FALSE)# GWSDAT_Options$UseGWSDATLib)
  
  
  # Initialize the data and do the fitting.
  ret = GWSDAT_Init(GWSDAT_Options)
  
  
  # Get return status and display.
  if (class(ret$status) == "GWSDAT_Error")
    stop(ret$status$msg)
  if (class(ret$status) == "GWSDAT_Warning")
    stop(ret$status$msg)

  
  # Create a complete GWSDAT instance with data, model, and options. 
  pnl <-  Create_PanelAttr(ret$Curr_Site_Data, RUNNING_SHINY = TRUE)
  
  
  # Put into global environment, so the shiny server can see it. 
  .GlobalEnv$pnl <- pnl
  .GlobalEnv$RUN_SINGLE_INSTANCE <- RUN_SINGLE_INSTANCE
  
  # on.exit(rm(pnl, envir = .GlobalEnv))
  
  runApp()
  
  
}