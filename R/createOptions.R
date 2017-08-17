 
 

#' Create a GWSDAT instance.
#'
#' @return GWSDAT_Options List with essential GWSDAT options. 
#' @export 
#'
#' @examples
createOptions <- function() {
  
  GWSDAT_Options <- list()
  GWSDAT_Options[['Aggby']] <- 'Monthly'
  GWSDAT_Options[['AggMethod']] <- 'Mean'
  GWSDAT_Options[['NDMethod']] <- 'Half of ND Value'
  GWSDAT_Options[['ModelMethod']] <- 'pspline'
  GWSDAT_Options[['cross']] <- 10
  GWSDAT_Options[['Tune']] <- TRUE
  GWSDAT_Options[['gamma']] <- c(0)
  GWSDAT_Options[['cost']] <- 2^c(0,1,2,3,4,5)
  GWSDAT_Options[['PSplineVars']] <- list()
  GWSDAT_Options[['PSplineVars']][['NIG.a']] <- 0.0001
  GWSDAT_Options[['PSplineVars']][['NIG.b']] <- 0.0001
  GWSDAT_Options[['PSplineVars']][['pord']] <- 1
  GWSDAT_Options[['PSplineVars']][['bdeg']] <- 2
  GWSDAT_Options[['PSplineVars']][['Trial.Lambda']] <- 10^seq(-6,0,length = 30)
  GWSDAT_Options[['PSplineVars']][['nseg']] <- 6
  GWSDAT_Options[['DefContThresh']] <- 500
  GWSDAT_Options[['DefPlumeThresh']] <- 10
  GWSDAT_Options[['DefPorosity']] <- 0.25
  GWSDAT_Options[['smThreshSe']] <- 1.1512
  GWSDAT_Options[['smThreshSe']] <- as.numeric(GWSDAT_Options[['smThreshSe']])
  GWSDAT_Options[['smMethod']] <- 'aicc'
  GWSDAT_Options[['Version']] <- '3.00'
  GWSDAT_Options[['Version']] <- as.numeric(GWSDAT_Options[['Version']])
  
  XtempScreenExp <- as.character('1,6')
  YtempScreenExp <- as.character('1,125')
  XtempScreenExp <- gsub(',','.',XtempScreenExp)
  YtempScreenExp <- gsub(',','.',YtempScreenExp)
  XtempScreenExp <- as.numeric(XtempScreenExp)
  YtempScreenExp <- as.numeric(YtempScreenExp)
  
  if (is.na(XtempScreenExp)) {XtempScreenExp <- 1}
  if (is.na(YtempScreenExp)) {YtempScreenExp <- 1}
  
  GWSDAT_Options[['Scale.panelImage']] <- list(hscale = as.numeric(XtempScreenExp), vscale = as.numeric(YtempScreenExp))
  
  if (exists('YtempScreenExp')) {rm(YtempScreenExp)}
  if (exists('XtempScreenExp')) {rm(XtempScreenExp)}
  
  GWSDAT_Options[['SiteName']] <- 'Basic Example'
  GWSDAT_Options[['WellDataFilename']] <- 'data/BasicExample_WellData.csv'
  GWSDAT_Options[['WellCoordsFilename']] <- 'data/BasicExample_WellCoords.csv'
  GWSDAT_Options[['ShapeFileNames']] <- NULL
  
  GWSDAT_Options[['SiteName']] <- 'Comprehensive Example'
  GWSDAT_Options[['WellDataFilename']] <- 'data/ComprehensiveExample_WellData.csv'
  GWSDAT_Options[['WellCoordsFilename']] <- 'data/ComprehensiveExample_WellCoords.csv'
  GWSDAT_Options[['ShapeFileNames']] <- c(GWSDAT_Options[['ShapeFileNames']],'data/GIS_Files/GWSDATex2.shp')
  
  
  #GWSDAT_Options[['RDataSet']] <- 'data/GWSDAT_BasicExample.Rdata'
  #GWSDAT_Options[['RDataSet']] <- 'data/GWSDAT_ComprExample.Rdata'
  #GWSDAT_Options[['RDataSet']] <- 'data/GWSDAT_Examples.Rdata'
  
  
  return(GWSDAT_Options)
}
