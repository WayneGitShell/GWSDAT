

GWSDAT_Setup <- function(GWSDATHome = ".", UseGWSDATLib = FALSE){

  
  
  ##
  ## Setup the libPaths to the additional packages required by GWSDAT.
  ##
  if ( as.numeric(R.Version()$major) == 2) {
  
  	try(.libPaths(c(paste(GWSDATHome,'R',paste('RLibsMajVer2',sep = ''),sep = '/'),.libPaths())))
  
  } else{
  
  	try(.libPaths(c(paste(GWSDATHome,'R',paste('RLibsMajVer3',sep = ''),sep = '/'),.libPaths())))
  
  }
  
  try(assign('.lib.loc', shortPathName(get('.lib.loc', envir = environment(.libPaths))),envir = environment(.libPaths)))
  
  
  if (!UseGWSDATLib) {
    
    source(paste(GWSDATHome, "R/GWSDAT Input Data.R", sep = "/"))
  	source(paste(GWSDATHome, "R/GWSDAT Traffic Lights.R", sep = "/"))
  	source(paste(GWSDATHome, "R/GWSDAT GWFlow.R", sep = "/"))
    source(paste(GWSDATHome, "R/GWSDAT.filled.contour.R", sep = "/"))
  	source(paste(GWSDATHome, "R/GWSDAT SVM.R", sep = "/"))
  	source(paste(GWSDATHome, "R/GWSDAT PSpline Utils.R", sep = "/"))
  	source(paste(GWSDATHome, "R/GWSDAT Auto Gamma.R", sep = "/"))
  	source(paste(GWSDATHome, "R/GWSDAT Shapefile Functions.R", sep = "/"))
  
    # replace this one by Plot_* function below
    source(paste(GWSDATHome, "R/GWSDAT MakePanel.R", sep = "/"))
    
    # added 07/2017
    source(paste(GWSDATHome, "R/GWSDAT_Init.R", sep = "/"))
    source(paste(GWSDATHome, "R/GWSDAT_Msg.R", sep = "/"))
    source(paste(GWSDATHome, "R/Plot_SmoothTimeSeries.R", sep = "/"))
    source(paste(GWSDATHome, "R/Plot_ImagePlot.R", sep = "/"))
    source(paste(GWSDATHome, "R/Plot_TrafficTable.R", sep = "/"))
    source(paste(GWSDATHome, "R/Plot_Legend.R", sep = "/"))
    source(paste(GWSDATHome, "R/Create_PanelAttr.R", sep = "/"))
    source(paste(GWSDATHome, "R/GWSDAT_Fit_Data.R", sep = "/"))
    source(paste(GWSDATHome, "R/Read_Data.R", sep = "/"))
    source(paste(GWSDATHome, "R/Utility_fct.R", sep = "/"))
    source(paste(GWSDATHome, "R/shiny_ui_analysepanel.R", sep = "/"))
    source(paste(GWSDATHome, "R/shiny_ui_datamanager.R", sep = "/"))
    
    
    
    
    
    
  	if (!GWSDAT_Load_Libs()) { stop("Missing packages") }
  
  } else {
  
    if (!require(GWSDAT)) {
  	  	  
      stop("Error: Cannot find package GWSDAT.")
    }
  
  }
  
  set.seed(1)
  
 
}
#----------------------------------------------------------------------------------------------------------------------#
