

initSite <- function(GWSDAT_Options, progressBar) {
  
  Run_status = GWSDAT_OK() 
 
  
  # Read Well data and coordinates from file.
  solute_data <- Read_Well_Data(GWSDAT_Options$WellDataFilename)
  well_data <- Read_Well_Coords(GWSDAT_Options$WellCoordsFilename)
  
  
  # Save the distance unit of the well coordinates.
  GWSDAT_Options$WellCoordsLengthUnits <- well_data$WellCoordsLengthUnits
  

  # Prepare the input data.
  All.Data <- try(prepare_data(solute_data, well_data, 
                               GWSDAT_Options))
  
  if (inherits(All.Data, 'try-error')) {
       return(GWSDAT_Error("Error in inputting and formatting data."))
  }

  
  # Fit the input data.
  Fitted.Data = GWSDAT_Fit_Data(All.Data, GWSDAT_Options, progressBar)
  
  if (class(Fitted.Data) != "gwsdat_fit") {
    stop("There was a problem with GWSDAT_Fit_Data() .. no fitted data returned, object class is: ", 
         class(Fitted.Data), "\n")
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
