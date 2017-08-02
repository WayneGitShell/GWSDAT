

GWSDAT_Setup <- function(GWSDATHome = ".", UseGWSDATLib = FALSE){

  
  
  #if (!UseGWSDATLib) {

  # Use the method below to load packages if not on a Linux system.
  if (.Platform$OS.type != "unix") { 

    if (!GWSDAT_Load_Libs(GWSDATHome)) { 
      stop("Missing packages") 
    }

  } else {

    # On Linux, directly load the packages. 
      
    require(sm)
    require(zoo)
    # require("tkrplot")
    require(splancs)
    require(Kendall)
    # require("animation")
    # require("rpanel")
    require(deldir)
    require(maptools)
    require(geometry)
    require(Matrix)
    require(shiny)
    require(shinyjs)
    require(shinydashboard)
        
  }

    source(paste(GWSDATHome, "R/GWSDAT Traffic Lights.R", sep = "/"))
    source(paste(GWSDATHome, "R/GWSDAT GWFlow.R", sep = "/"))
    source(paste(GWSDATHome, "R/GWSDAT.filled.contour.R", sep = "/"))
    source(paste(GWSDATHome, "R/GWSDAT SVM.R", sep = "/"))
    source(paste(GWSDATHome, "R/GWSDAT PSpline Utils.R", sep = "/"))
    source(paste(GWSDATHome, "R/GWSDAT Auto Gamma.R", sep = "/"))
    source(paste(GWSDATHome, "R/GWSDAT Shapefile Functions.R", sep = "/"))
  
    # added 07/2017
    
    source(paste(GWSDATHome, "R/prepare_data.R", sep = "/"))
    source(paste(GWSDATHome, "R/GWSDAT_Msg.R", sep = "/"))
    source(paste(GWSDATHome, "R/Plot_Legend.R", sep = "/"))
    source(paste(GWSDATHome, "R/read_data.R", sep = "/"))
    
  
    source(paste(GWSDATHome, "R/fitData.R", sep = "/"))
    source(paste(GWSDATHome, "R/createPanelAttr.R", sep = "/"))
    source(paste(GWSDATHome, "R/initSite.R", sep = "/"))
    source(paste(GWSDATHome, "R/utilities.R", sep = "/"))
    source(paste(GWSDATHome, "R/aggregateData.R", sep = "/"))
    source(paste(GWSDATHome, "R/createOptions.R", sep = "/"))
    source(paste(GWSDATHome, "R/plotWellReport.R", sep = "/"))
    source(paste(GWSDATHome, "R/plotTimeSeries.R", sep = "/"))
    source(paste(GWSDATHome, "R/plotSpatialImage.R", sep = "/"))
    source(paste(GWSDATHome, "R/plotTrendTable.R", sep = "/"))
    source(paste(GWSDATHome, "R/plumeDiagnostics.R", sep = "/"))
    source(paste(GWSDATHome, "R/uiAnalyse.R", sep = "/"))
    source(paste(GWSDATHome, "R/uiDataManager.R", sep = "/"))
    source(paste(GWSDATHome, "R/uiWellReport.R", sep = "/"))

    # For the well report, might change.  
    source(paste(GWSDATHome, "R/chooserInput.R", sep = "/"))
    
    # Need this as long as I'm not fully dependent on shiny.
    source(paste(GWSDATHome, "R/GWSDAT_select_list.R", sep = "/"))

}


#----------------------------------------------------------------------------------------------------------------------#



GWSDAT_Load_Libs <- function(GWSDATHome){


 
  Require <- function(pkg) {
    
    if (data.class(result <- try(find.package(pkg, lib.loc = .libPaths()), TRUE)) == "try-error") {
      browser()
      # tkmessageBox(title="An error has occured!",message=paste("Cannot find package \"",pkg,"\"",sep=""),icon="error",type="ok")
      
      ## pass error+message somehow back without calling tkmessageBox
      # return(GWSDAT_Error(paste("Cannot find package \"",pkg,"\"",sep="")))  
      # .. not doing this here because we need FALSE back.
      return(FALSE)
    }
    else
    {
      require(pkg, character.only = TRUE)
      return(TRUE)
    }
  }
  
  
  
  ##
  ## Setup the libPaths to the additional packages required by GWSDAT.
  ##
  local_libPath = paste(GWSDATHome, '/R/RLibsMajVer', as.numeric(R.Version()$major), 
                        "/", .Platform$OS.type, sep = "")
  
  
  try(.libPaths(c(local_libPath, .libPaths() )))
  
  try(assign('.lib.loc', shortPathName(get('.lib.loc', envir = environment(.libPaths))),envir = environment(.libPaths)))
  
 
  
  try(options(editor = "notepad"))
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
    
  #if(.Platform$OS.type != "unix")
  Require("RDCOMClient")  
  
  
  return(TRUE)
}


