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


Create_PanelAttr <- function(Curr.Site.Data, RUNNING_SHINY = FALSE) {
  
  All.Data <- Curr.Site.Data$All.Data
  GWSDAT_Options <- Curr.Site.Data$GWSDAT_Options
  Fitted.Data <- Curr.Site.Data$Fitted.Data
  Cont.Names <- names(Fitted.Data)
  Num.Conts <- length(Cont.Names)
  Scale.panelImage = list( hscale = GWSDAT_Options$Scale.panelImage$hscale, vscale = GWSDAT_Options$Scale.panelImage$vscale )
  my.lev.cut <- c(0,5,10,25,50,75,100,200, 400, 800, 1500, 3000, 5000, 5000000)
  sd.lev.cut <- 100*c(seq(0,3,by=0.25),10000000)
  
  
  #
  # Set 'Use.Defaults'.
  #
  if(!is.null(attributes(Curr.Site.Data)$Default.panel.Values)) {
    Use.Defaults <- TRUE
    Default.Values<-attributes(Curr.Site.Data)$Default.panel.Values
  } else { 
    Use.Defaults <- FALSE
  }
  
  
  #
  # Set 'ContLimEntry'
  #
  if(Use.Defaults && !is.null(Default.Values$ContLimEntry)) { 
    ContLimEntry <- Default.Values$ContLimEntry
  } else {
    ContLimEntry <- as.character(rep(GWSDAT_Options$DefContThresh,Num.Conts)); 
    names(ContLimEntry) <- Cont.Names
  }
  
  
  # 
  # Set 'PlumeLimEntry'
  #
  if(Use.Defaults && !is.null(Default.Values$PlumeLimEntry)) { 
    PlumeLimEntry <- Default.Values$PlumeLimEntry
  } else {
    PlumeLimEntry <- as.character(rep(GWSDAT_Options$DefPlumeThresh,Num.Conts)); 
    names(PlumeLimEntry) <- Cont.Names
  }
  
  
  #
  # Set 'Porosity'
  #
  if(Use.Defaults && !is.null(Default.Values$Porosity)){
    Porosity <- Default.Values$Porosity
  } else {
    Porosity <- as.character(GWSDAT_Options$DefPorosity)
  }
  
  
  
  #
  #
  # Define all Drawing Related Variables (DRV) - directly usable for panel drawing.
  #
  #
  DRV <-  list()
  DRV$Cont.Data <- All.Data$Cont.Data
  DRV$All.Dates <- All.Data$All.Dates
  DRV$All.Agg.Dates <- All.Data$All.Agg.Dates
  DRV$Cont.Names <- Cont.Names
  DRV$All.Data <- All.Data
  DRV$Fitted.Data <- Fitted.Data
  DRV$Scale.panelImage <- Scale.panelImage
  DRV$Traffic.Lights <- attr(Fitted.Data,"TrafficLights")
  DRV$lev.cut <-  my.lev.cut
  DRV$sd.lev.cut <- sd.lev.cut
  DRV$GWSDAT_Options <- GWSDAT_Options
  DRV$tempTotalPlumeDetails <-  NULL
  DRV$Num.Conts <-  Num.Conts
  DRV$PlumeLimEntry <- PlumeLimEntry
  DRV$Porosity <- Porosity
  
  
  if(!RUNNING_SHINY) {
    
    # Create the TK panel when running Stand-Alone.
    panel <- rp.control(
      title = if(All.Data$Aq.sel==""){ 
        GWSDAT_Options$SiteName 
      } else { 
        paste(GWSDAT_Options$SiteName,": Aquifer-",All.Data$Aq.sel,sep="")
      }, 
      panelname = "GWSDATpnl",
      DRV = DRV )
  } else {
    
    # Create panel variable for Shiny.
    panel <- list(DRV = DRV)
  }
  
  #
  # Set 'rgUnits' - done with Combo control (GWSDAT MakePanel.R:3305)
  #
  panel$rgUnits <- Curr.Site.Data$rgUnits
  panel$rgUnits_choice <- Curr.Site.Data$rgUnits_choice

  
  panel$Cont.rg <- Curr.Site.Data$Cont.rg
  panel$Well <- if(Use.Defaults && !is.null(Default.Values$Well)) {Default.Values$Well} else {Curr.Site.Data$Well}
  panel$ContLimEntry <- ContLimEntry #fix for R-3.0.0 tkrplot bug
  panel$rg1 <- if(Use.Defaults && !is.null(Default.Values$rg1)){Default.Values$rg1}else{"Trend"}
  panel$rg1_choice <- c("Trend","Threshold - Absolute","Threshold - Statistical")
  panel$Stat.Lim <- as.numeric(panel$ContLimEntry[match(panel$Cont.rg, panel$DRV$Cont.Names)])
  panel$dlines <- Curr.Site.Data$dlines
  
  # time steps:
  # Set the value and range of the 'Time Step' slider control (for the ImagePlot)
  panel$timestep_range = c(1, length(All.Data$All.Agg.Dates))
  panel$timestep = 1
  #panel$aggregate_data = Curr.Site.Data$GWSDAT_Options$Aggby
  #panel$aggregate_data_choice = Curr.Site.Data$GWSDAT_Options$Aggby_choice
  
  panel$ScaleCols <- Curr.Site.Data$ScaleCols
  panel$GW.disp_choice <-  c("None", "Same Length", "Weighted Length")
  panel$GW.disp <- if(Use.Defaults && !is.null(Default.Values$GW.disp)){Default.Values$GW.disp}else{"Weighted Length"}
  panel$Color.type_choice <- c("Conc-Terrain", "Conc-Topo","Conc-GreyScale","Conc-Terrain-Circles","Conc-Topo-Circles","Conc-GreyScale-Circles",if(!is.null(All.Data$NAPL.Thickness.Data)){"NAPL-Circles"})  
  panel$Color.type <- if(Use.Defaults && !is.null(Default.Values$Color.type)){Default.Values$Color.type}else{"Conc-Terrain"}
  
  panel$ColTrafficListbox = "All"
  panel$ColTrafficListbox_choice = c("All","Reds","Greens","White","Non-Detects","Greys")
  #
  # Should be always set to "Predicted". Corresponding Radiogroup control is not active.
  #
  if(is.null(panel$PredInterval)){
    panel$PredInterval="Predicted"
  }
  
  
  return(panel)
  
  
}