 
 

#' Create a list with default start options.
#'
#' \code{createOptions} creates a list with start options that can be modified 
#' and passed as argument to \code{\link{launchApp}}.   
#'
#' 
#' @param site_name An arbitrary string containing the name of the monitoring site.
#'
#' @return A list containing essential model parameters and start options.
#'
#' @export 
#'
#' @examples 
#' opt <- createOptions("New Site 1")
#' opt$PSplineVars$nseg <- 10  # modify model parameter for p-splines.
#' opt$WellDataFilename <- 'path_to_concentration_file.csv'
#' opt$WellCoordsFilename <- 'path_to_well_coordinate_file.csv'
#' if(interactive()) {
#' launchApp(opt)
#' }
createOptions <- function(site_name = NULL) {
  
  GWSDAT_Options <- list()
  
  GWSDAT_Options[['Aggby']] <- 'Month'
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
  GWSDAT_Options[['smMethod']] <- 'aicc'
  GWSDAT_Options[['Version']] <- packageVersion('GWSDAT')
  GWSDAT_Options[['ShapeFileNames']] <- NULL
  
  
  
  #GWSDAT_Options[['SiteName']] <- 'Basic Example'
  #GWSDAT_Options[['WellDataFilename']] <- 'data/BasicExample_WellData.csv'
  #GWSDAT_Options[['WellCoordsFilename']] <- 'data/BasicExample_WellCoords.csv'
  
  #GWSDAT_Options[['SiteName']] <- 'Comprehensive Example'
  #GWSDAT_Options[['WellDataFilename']] <- 'data/ComprehensiveExample_WellData.csv'
  #GWSDAT_Options[['WellCoordsFilename']] <- 'data/ComprehensiveExample_WellCoords.csv'
  #GWSDAT_Options[['ShapeFileNames']] <- c(GWSDAT_Options[['ShapeFileNames']],'data/GIS_Files/GWSDATex2.shp')
  
  
  if (!is.null(site_name)) GWSDAT_Options[['SiteName']] <- site_name
  
  return(GWSDAT_Options)
}
