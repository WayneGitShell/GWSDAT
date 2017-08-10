

initSite <- function(GWSDAT_Options, progressBar) {
  
  Run_status = GWSDAT_OK() 
 
  
  # Read Well data and coordinates from file.
  solute_data <- readConcData(GWSDAT_Options$WellDataFilename)
  well_data <- readWellCoords(GWSDAT_Options$WellCoordsFilename)
  
  # Prepare the input data.
  All.Data <- try(processData(solute_data, well_data, 
                               GWSDAT_Options))
  
  if (inherits(All.Data, 'try-error')) {
       return(GWSDAT_Error("Error in inputting and formatting data."))
  }

  
  # Fit the input data.
  Fitted.Data = fitData(All.Data, GWSDAT_Options, progressBar)
  
  if (class(Fitted.Data) != "gwsdat_fit") {
    stop("No fitted data returned, return class is: ", class(Fitted.Data), "\n")
  }
  

  # Create UI attributes
  ui_attr <- createUIAttr(All.Data, Fitted.Data, GWSDAT_Options)

  # Build list with all data.
  site_data <- list(All.Data       = All.Data,
                    Fitted.Data    = Fitted.Data,
                    GWSDAT_Options = GWSDAT_Options,
                    Traffic.Lights = attr(Fitted.Data,"TrafficLights"),
                    ui_attr        = ui_attr
                    )
  
  #class(site_data) <- 'GWSDAT.Data'
  
  
  try(rm(Fitted.Data, All.Data, GWSDAT_Options, ui_attr))
  
  return(site_data)

}
