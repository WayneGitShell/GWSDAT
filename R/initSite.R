

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
  
  
  # Build list with all data.
  Curr.Site.Data <-  list(All.Data = All.Data,
                      Fitted.Data = Fitted.Data,
                      GWSDAT_Options = GWSDAT_Options
                     )
  
  class(Curr.Site.Data) <- 'GWSDAT.Data'
  
  
  try(rm(Fitted.Data, All.Data, GWSDAT_Options))
  
  return(Curr.Site.Data)

}
