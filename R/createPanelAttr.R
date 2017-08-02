#
# This function takes the data contained in Curr.Site.Data (created by GWSDAT_Init()) and transforms it to a 
# panel variable. 
#
#  Issue: Basically its a copy of Curr.Site.Data (+ some more variables) and restructured a little to make drawing easier.
#         The point is to create a tcltk 'panel' variable here from rp.control if not running Shiny.   
#                 
#  How can I decrease the overhead ? 
#
#


createPanelAttr <- function(Curr.Site.Data) {
  
  All.Data <- Curr.Site.Data$All.Data
  GWSDAT_Options <- Curr.Site.Data$GWSDAT_Options
  Fitted.Data <- Curr.Site.Data$Fitted.Data
  Cont.Names <- names(Fitted.Data)
  Num.Conts <- length(Cont.Names)
 
  
  #
  # Set 'Use.Defaults'.
  #
  if (!is.null(attributes(Curr.Site.Data)$Default.panel.Values)) {
    Use.Defaults <- TRUE
    Default.Values <- attributes(Curr.Site.Data)$Default.panel.Values
  } else { 
    Use.Defaults <- FALSE
  }
  
  
  #
  # Set 'ContLimEntry'
  #
  if (Use.Defaults && !is.null(Default.Values$ContLimEntry)) { 
    ContLimEntry <- Default.Values$ContLimEntry
  } else {
    ContLimEntry <- as.character(rep(GWSDAT_Options$DefContThresh, Num.Conts)); 
    names(ContLimEntry) <- Cont.Names
  }
  
  
  # 
  # Set 'PlumeLimEntry'
  #
  if (Use.Defaults && !is.null(Default.Values$PlumeLimEntry)) { 
    PlumeLimEntry <- Default.Values$PlumeLimEntry
  } else {
    PlumeLimEntry <- as.character(rep(GWSDAT_Options$DefPlumeThresh,Num.Conts)); 
    names(PlumeLimEntry) <- Cont.Names
  }
  
  
  #
  # Set 'Porosity'
  #
  if (Use.Defaults && !is.null(Default.Values$Porosity)) {
    Porosity <- Default.Values$Porosity
  } else {
    Porosity <- as.character(GWSDAT_Options$DefPorosity)
  }
  
 
  
  panel = list()
  
  
  #
  #
  # Define Display & Control Attributes (only affects GUI)
  #
  #
  
  panel$All.Data <- All.Data
  panel$Fitted.Data <- Fitted.Data
  panel$Traffic.Lights <- attr(Fitted.Data,"TrafficLights")
  panel$GWSDAT_Options <- GWSDAT_Options
  
  
  #
  # Model attributes
  #
  panel$tempTotalPlumeDetails <-  NULL
  panel$PlumeLimEntry <- PlumeLimEntry
  panel$Porosity <- Porosity
  
  panel$rgUnits <- "ug/l"
  panel$rgUnits_choice <-  list("ng/l","ug/l","mg/l")
  panel$Cont.rg <- names(Fitted.Data)[1]
  
  sorted_Wells <-  sort(as.character(All.Data$All.Wells))
  panel$Well <- if (Use.Defaults && !is.null(Default.Values$Well)) {Default.Values$Well} else {sorted_Wells[1]}
  
  panel$timestep_range = c(1, length(All.Data$All.Agg.Dates))
  panel$timestep = 1
  
  #panel$NAPL.Present <- any("napl" %in% tolower(as.character(Well.Data$Result))) ||   nrow(panel$All.Data$NAPL.Thickness.Data[as.character(panel$All.Data$NAPL.Thickness.Data$WellName)==panel$Well,])>0
  #if (NAPLThickPresent) { panel$NAPL.Present <- FALSE }
  
  # time-series plot attributes
  dlines = NULL  
  dlines["Conc. Trend Smoother"] <- TRUE
  dlines["Conc. Linear Trend Fit"] <- FALSE
  dlines["Show Legend"] <- FALSE
  dlines["Scale to Conc. Data"] <- FALSE
  dlines["Log Conc. Scale"] <- TRUE
  dlines["Overlay GW levels"] <- FALSE
  
  # Checks if any NAPL thickness is present.
  NAPLThickPresent <- !is.null(All.Data$NAPL.Thickness.Data)
  if (NAPLThickPresent) dlines["Overlay NAPL Thickness"] <- FALSE  
  
  panel$dlines <- dlines
  
  # Checks if NAPL thickness is present for current well and solute 
  #  This is to activate/inactivate the 'Overlay NAPL Thickness' control.
  panel$NAPL.Present <- existsNAPL(panel$All.Data, panel$Well, panel$Cont.rg) 
 
  
  
  
  panel$lev.cut <-  c(0,5,10,25,50,75,100,200, 400, 800, 1500, 3000, 5000, 5000000)
  panel$sd.lev.cut <- 100*c(seq(0,3,by = 0.25),10000000)
  panel$ContLimEntry <- ContLimEntry #fix for R-3.0.0 tkrplot bug
  panel$rg1 <- if (Use.Defaults && !is.null(Default.Values$rg1)) {Default.Values$rg1}else{"Trend"}
  panel$rg1_choice <- c("Trend","Threshold - Absolute","Threshold - Statistical")
  panel$Stat.Lim <- as.numeric(panel$ContLimEntry[match(panel$Cont.rg, Cont.Names)])
  
  
  # Options for the spatial plot.
  ScaleCols <-  NULL
  ScaleCols["Show Well Labels"] <- TRUE
  ScaleCols["Scale colours to Data"] <- FALSE
  ScaleCols["Show Conc. Values"] <- TRUE
  ScaleCols["Show GW Contour"] <- FALSE
  if (!is.null(GWSDAT_Options$ShapeFileNames)) ScaleCols["Overlay ShapeFiles"] <- TRUE
  ScaleCols["Plume Diagnostics"] <- FALSE
  panel$ScaleCols <- ScaleCols
 
  panel$GW.disp_choice <-  c("None", "Same Length", "Weighted Length")
  panel$GW.disp <- if (Use.Defaults && !is.null(Default.Values$GW.disp)) {
    Default.Values$GW.disp}else{"Weighted Length"
      }
  panel$Color.type_choice <- c("Conc-Terrain", "Conc-Topo","Conc-GreyScale","Conc-Terrain-Circles","Conc-Topo-Circles","Conc-GreyScale-Circles", if (!is.null(All.Data$NAPL.Thickness.Data)) {"NAPL-Circles"})  
  panel$Color.type <- if (Use.Defaults && !is.null(Default.Values$Color.type)) {Default.Values$Color.type}else{"Conc-Terrain"}
  panel$ColTrafficListbox = "All"
  panel$ColTrafficListbox_choice = c("All","Reds","Greens","White","Non-Detects","Greys")
  
  
  #
  # Should be always set to "Predicted". Corresponding Radiogroup control is not active.
  #
  if (is.null(panel$PredInterval)) {
    panel$PredInterval = "Predicted"
  }
  
  
  # Define the image formats that can be saved.
  panel$image_formats <- list("png", "jpg", "pdf", "ps")
  if (.Platform$OS.type == "windows") {
    panel$image_formats[[length(panel$image_formats) + 1]] <- "wmf"
    panel$image_formats[[length(panel$image_formats) + 1]] <- "ppt"
  }
  
    
  
  return(panel)
  
  
}