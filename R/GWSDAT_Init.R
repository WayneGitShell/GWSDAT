

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
  
  if(inherits(All.Data, 'try-error')){
    
    #TK stuff:
    #tkmessageBox(title="Error!",message="Error in inputting and formatting data.",icon="error",type="ok")
    #try(close(pb))
    #stop("Error in inputting and formatting data.")
    
    return(GWSDAT_Error("Error in inputting and formatting data."))
    
  }
  
  try(rm(list=c('AGALL','WellCoords')))
  
  #NumConts = length(All.Data$All.Conts)         # only needed inside Fit_Data, delete at some point..
  #ContNames = All.Data$All.Conts                # only needed for TK progress inside Fit_Data, delete at some point.. 
  
  #TK stuff:
  #PctDone = 1 / (NumConts + 3)
  
  #-------------------------------------------------------------------------------------------------#
  
  
  Fitted.Data = GWSDAT_Fit_Data(All.Data, GWSDAT_Options)
  
  if(class(Fitted.Data) != "gwsdat_fit") {
    stop("There was a problem with GWSDAT_Fit_Data() .. no fitted data returned, object class is: ", class(Fitted.Data), "\n")
  }
  
  ############################ Set initial panel control attributes ###############################################################
  
    
  Cont.rg = names(Fitted.Data)[1]    
  rgUnits = "ug/l"
  rgUnits_choice = list("ng/l","ug/l","mg/l")
  
  
  #
  # Define Time-Series Plot draw options.
  #
  dlines = NULL  
  dlines["Conc. Trend Smoother"] <- TRUE
  dlines["Conc. Linear Trend Fit"] <- FALSE
  dlines["Show Legend"] <- FALSE
  dlines["Scale to Conc. Data"] <- FALSE
  dlines["Log Conc. Scale"] <- TRUE
  dlines["Overlay GW levels"] <- FALSE
  #dlines["Overlay NAPL Thickness"] <- FALSE  ## depends on 'NAPL.Present' below
  
  
  #
  # Define ImagePlot draw options.
  #
  # Initialization values from Default.Values, do this later.
  # initval = if(Use.Defaults && !is.null(Default.Values$ScaleCols)){Default.Values$ScaleCols}else{c(TRUE,FALSE,TRUE,FALSE,if(is.null(All.Data$ShapeFiles)){FALSE}else{TRUE},FALSE)
  ScaleCols <-  NULL
  ScaleCols["Show Well Labels"] <- TRUE
  ScaleCols["Scale colours to Data"] <- FALSE
  ScaleCols["Show Conc. Values"] <- TRUE
  ScaleCols["Show GW Contour"] <- FALSE
  ScaleCols["Overlay ShapeFiles"] <- FALSE
  ScaleCols["Plume Diagnostics"] <- FALSE

  
  #rp.listbox(GWSDATpnl,Well,labels=sort(as.character(All.Data$All.Wells)),
  #           vals=sort(as.character(All.Data$All.Wells)),
  #           grid="ControlsGrid" ,
  #           initval=if(Use.Defaults && !is.null(Default.Values$Well)){Default.Values$Well}else{sort(as.character(All.Data$All.Wells))[1]},  
  #           row = 1, column = 1,title="Select Monitoring Well",rows =min(10,length(as.character(All.Data$All.Wells))),action=listbox.Well.Select)
  sorted_Wells = sort(as.character(All.Data$All.Wells))
  Well = sorted_Wells[1]
  
  
  ############################ Clean Up #################################################################################
  
  Curr.Site.Data=list(All.Data=All.Data,
                      Fitted.Data=Fitted.Data,
                      GWSDAT_Options=GWSDAT_Options, 
                      Cont.rg = Cont.rg,
                      rgUnits = rgUnits,
                      rgUnits_choice = rgUnits_choice,
                      dlines = dlines,
                      ScaleCols = ScaleCols,
                      Well = Well
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