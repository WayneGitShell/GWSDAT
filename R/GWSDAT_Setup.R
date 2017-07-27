
#
# Former Call: GWSDAT_Setup(GWSDAT_Options$GWSDATHome, GWSDAT_Options$UseGWSDATLib)
#

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
    
  	source(paste(GWSDATHome, "R/GWSDAT Traffic Lights.R", sep = "/"))
  	source(paste(GWSDATHome, "R/GWSDAT GWFlow.R", sep = "/"))
    source(paste(GWSDATHome, "R/GWSDAT.filled.contour.R", sep = "/"))
  	source(paste(GWSDATHome, "R/GWSDAT SVM.R", sep = "/"))
  	source(paste(GWSDATHome, "R/GWSDAT PSpline Utils.R", sep = "/"))
  	source(paste(GWSDATHome, "R/GWSDAT Auto Gamma.R", sep = "/"))
  	source(paste(GWSDATHome, "R/GWSDAT Shapefile Functions.R", sep = "/"))
  
    # added 07/2017
    source(paste(GWSDATHome, "R/GWSDAT_Init.R", sep = "/"))
    source(paste(GWSDATHome, "R/prepare_data.R", sep = "/"))
    source(paste(GWSDATHome, "R/GWSDAT_Msg.R", sep = "/"))
    source(paste(GWSDATHome, "R/Plot_SmoothTimeSeries.R", sep = "/"))
    source(paste(GWSDATHome, "R/Plot_ImagePlot.R", sep = "/"))
    source(paste(GWSDATHome, "R/Plot_TrafficTable.R", sep = "/"))
    source(paste(GWSDATHome, "R/Plot_Legend.R", sep = "/"))
    source(paste(GWSDATHome, "R/createPanelAttr.R", sep = "/"))
    source(paste(GWSDATHome, "R/GWSDAT_Fit_Data.R", sep = "/"))
    source(paste(GWSDATHome, "R/read_data.R", sep = "/"))
    source(paste(GWSDATHome, "R/utility_fcts.R", sep = "/"))
    source(paste(GWSDATHome, "R/shiny_ui_analysepanel.R", sep = "/"))
    source(paste(GWSDATHome, "R/shiny_ui_datamanager.R", sep = "/"))
    source(paste(GWSDATHome, "R/aggregate_data.R", sep = "/"))
    source(paste(GWSDATHome, "R/create_GWSDAT_Instance.R", sep = "/"))
    source(paste(GWSDATHome, "R/make_animation.R", sep = "/"))
    
    
    
    
    # do I need it?
    # Called from: GWSDAT_Fit_Data.R, plot_Traffic_Table.R, GWSDAT_Input_Data.R (each one time)
    source(paste(GWSDATHome, "R/GWSDAT_select_list.R", sep = "/"))
    
    
    
    
    
    
  	if (!GWSDAT_Load_Libs()) { stop("Missing packages") }
  
  } else {
  
    if (!require(GWSDAT)) {
  	  	  
      stop("Error: Cannot find package GWSDAT.")
    }
  
  }
  
  set.seed(1)
  
 
}
#----------------------------------------------------------------------------------------------------------------------#



GWSDAT_Load_Libs <- function(){
  
  Require <- function(pkg) {
    
    if (data.class(result<-try(find.package(pkg,lib.loc=.libPaths()),TRUE))=="try-error")
    {
      # tkmessageBox(title="An error has occured!",message=paste("Cannot find package \"",pkg,"\"",sep=""),icon="error",type="ok")
      
      ## pass error+message somehow back without calling tkmessageBox
      # return(GWSDAT_Error(paste("Cannot find package \"",pkg,"\"",sep="")))  
      # .. not doing this here because we need FALSE back.
      return (FALSE)
    }
    else
    {
      require(pkg,character.only=TRUE)
      return (TRUE)
    }
  }
  
  
  
  try(options(editor="notepad"))
  if(!Require("sm")){return(FALSE)}
  if(!Require("zoo")){return(FALSE)}
  if(!Require("tkrplot")){return(FALSE)}
  if(!Require("splancs")){return(FALSE)}
  if(!Require("Kendall")){return(FALSE)}
  if(!Require("animation")){return(FALSE)}
  if(!Require("rpanel")){return(FALSE)}
  if(!Require("deldir")){return(FALSE)}
  if(!Require("maptools")){return(FALSE)}
  if(!Require("geometry")){return(FALSE)}
  if(!Require("Matrix")){return(FALSE)}
  if(!Require("shiny")){return(FALSE)}
  if(!Require("shinyjs")){return(FALSE)}
  if(!Require("shinydashboard")){return(FALSE)}
  Require("RDCOMClient")  
  
  
  return(TRUE)
}


