

loadPackages <- function(){
  
  
  Require <- function(pkg) {
    
    if (data.class(result <- try(find.package(pkg, lib.loc = .libPaths()), TRUE)) == "try-error") {
      
      cat(paste("Cannot find package \"", pkg, "\"", sep = ""))
      return(FALSE)
      
    } else {
      
      require(pkg, character.only = TRUE)
      return(TRUE)
      
    }
  }
  
  
  
  #
  # Setup the libPaths to the additional packages required by GWSDAT.
  #
  local_libPath = paste(HomeDir, '/R/RLibsMajVer', as.character(R.Version()$major), sep = '')
  
  .libPaths(c(local_libPath, .libPaths() ))
  
  assign('.lib.loc', shortPathName(get('.lib.loc', envir = environment(.libPaths))),
         envir = environment(.libPaths))
  
  if (!Require("sm")) {return(FALSE)}
  if (!Require("zoo")) {return(FALSE)}
  if (!Require("splancs")) {return(FALSE)}
  if (!Require("Kendall")) {return(FALSE)}
  if (!Require("deldir")) {return(FALSE)}
  if (!Require("maptools")) {return(FALSE)}
  if (!Require("geometry")) {return(FALSE)}
  if (!Require("Matrix")) {return(FALSE)}
  if (!Require("shiny")) {return(FALSE)}
  if (!Require("shinyjs")) {return(FALSE)}
  if (!Require("shinydashboard")) {return(FALSE)}
  if (!Require("shinycssloaders")) {return(FALSE)}
  if (!Require("rhandsontable")) {return(FALSE)}
  if (!Require("readxl")) {return(FALSE)}
  if (!Require("RDCOMClient")) {return(FALSE)}  
  
  return(TRUE)
}


 HomeDir = "."


# Use the method below to load packages if not on a Linux system.
if (.Platform$OS.type == "windows") { 

  # Loads the windows packages that are included with this app
  if (!loadPackages()) { 
    stop("Missing packages") 
  }

} else {

  # On Linux, load the packages from the system. 
  require(sm)
  require(zoo)
  require(splancs)
  require(Kendall)
  require(deldir)
  require(maptools)
  require(geometry)
  require(Matrix)
  require(shiny)
  require(shinyjs)
  require(shinydashboard)
  require(shinycssloaders)
  require(rhandsontable)
  require(readxl)
    
}

source(paste(HomeDir, "R/calcTrafficLights.R", sep = "/"))
source(paste(HomeDir, "R/calcGWFlow.R", sep = "/"))
source(paste(HomeDir, "R/fitPSpline.R", sep = "/"))
source(paste(HomeDir, "R/selectGamma.R", sep = "/"))
source(paste(HomeDir, "R/readShapeFile.R", sep = "/"))
source(paste(HomeDir, "R/readData.R", sep = "/"))
source(paste(HomeDir, "R/processData.R", sep = "/"))
source(paste(HomeDir, "R/fitData.R", sep = "/"))
source(paste(HomeDir, "R/utilities.R", sep = "/"))
source(paste(HomeDir, "R/aggregateData.R", sep = "/"))
source(paste(HomeDir, "R/createOptions.R", sep = "/"))
source(paste(HomeDir, "R/plotWellReport.R", sep = "/"))
source(paste(HomeDir, "R/plotTimeSeries.R", sep = "/"))
source(paste(HomeDir, "R/plotSpatialImage.R", sep = "/"))
source(paste(HomeDir, "R/plotTrendTable.R", sep = "/"))
source(paste(HomeDir, "R/plotPlumeTimeSeries.R", sep = "/"))
source(paste(HomeDir, "R/plumeDiagnostics.R", sep = "/"))
source(paste(HomeDir, "R/uiAnalyse.R", sep = "/"))
source(paste(HomeDir, "R/uiWellReport.R", sep = "/"))
source(paste(HomeDir, "R/uiPlumeDiagnostics.R", sep = "/"))
source(paste(HomeDir, "R/uiAnalyseOptions.R", sep = "/"))
source(paste(HomeDir, "R/uiAttr.R", sep = "/"))
source(paste(HomeDir, "R/uiSession.R", sep = "/"))
source(paste(HomeDir, "R/interpData.R", sep = "/"))
source(paste(HomeDir, "R/chooserInput.R", sep = "/"))









