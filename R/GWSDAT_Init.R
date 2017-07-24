

GWSDAT_Init <- function(GWSDAT_Options) {

  Run_status = GWSDAT_OK() 
  
 
  All.Data <- try(GWSDAT.Init.Data(AGALL, WellCoords, GWSDAT_Options))
  
  if (inherits(All.Data, 'try-error')) {
       return(GWSDAT_Error("Error in inputting and formatting data."))
  }
  
  
  
  Fitted.Data = GWSDAT_Fit_Data(All.Data, GWSDAT_Options)
  
  if (class(Fitted.Data) != "gwsdat_fit") {
    stop("There was a problem with GWSDAT_Fit_Data() .. no fitted data returned, object class is: ", class(Fitted.Data), "\n")
  }
  
 
  Curr.Site.Data <-  list(All.Data = All.Data,
                      Fitted.Data = Fitted.Data,
                      GWSDAT_Options = GWSDAT_Options
                     )
  attr(Curr.Site.Data, 'class') <- 'GWSDAT.Data'
  
 
  try(rm(Fitted.Data,All.Data,GWSDAT_Options))

  return(list(status = Run_status, Curr_Site_Data = Curr.Site.Data))

}