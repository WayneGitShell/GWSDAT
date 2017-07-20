

GWSDAT_Init <- function(GWSDAT_Options) {

  # Set this to GWSDAT_Error() or GWSDAT_Warning() if something bad happens.
  # Note: GWSDAT_Error() should be returned immediately to stop execution.
  #       GWSDAT_Warning() can continue running.
  Run_status = GWSDAT_OK() 
  
  
  #
  # Read Well data and coordinates from file.
  #
  AGALL <- Read_Well_Data(GWSDAT_Options$WellDataFilename)
  well_coords <- Read_Well_Coords(GWSDAT_Options$WellCoordsFilename)
  
  WellCoords <- well_coords$WellCoords
  GWSDAT_Options$WellCoordsLengthUnits <-  well_coords$WellCoordsLengthUnits
  
  ############################# Consolidate Data together ##########################################
  
  All.Data <- try(GWSDAT.Init.Data(AGALL, WellCoords, GWSDAT_Options))
  
  if (inherits(All.Data, 'try-error')) {
    
    #TK stuff:
    #tkmessageBox(title="Error!",message="Error in inputting and formatting data.",icon="error",type="ok")
    #try(close(pb))
    #stop("Error in inputting and formatting data.")
    
    return(GWSDAT_Error("Error in inputting and formatting data."))
    
  }
  
  try(rm(list = c('AGALL','WellCoords')))
  
  #TK stuff:
  #PctDone = 1 / (NumConts + 3)
  
  #-------------------------------------------------------------------------------------------------#
  
  
  Fitted.Data = GWSDAT_Fit_Data(All.Data, GWSDAT_Options)
  
  if (class(Fitted.Data) != "gwsdat_fit") {
    stop("There was a problem with GWSDAT_Fit_Data() .. no fitted data returned, object class is: ", class(Fitted.Data), "\n")
  }
  
 
 
  
  ############################ Clean Up #################################################################################
  
  Curr.Site.Data <-  list(All.Data = All.Data,
                      Fitted.Data = Fitted.Data,
                      GWSDAT_Options = GWSDAT_Options
                     )
  attr(Curr.Site.Data, 'class') <- 'GWSDAT.Data'
  
  #TK stuff:
  #try(close(pb))
  #try(rm(pb, PctDone))
  
  try(rm(Fitted.Data,All.Data,GWSDAT_Options))
  #try(rm(i))
  #try(rm(i,NumConts,ContNames))
  
  #----------------------------------------------------------------------------------------------------------------------#
  
  
  
  return(list(status = Run_status, Curr_Site_Data = Curr.Site.Data))

}