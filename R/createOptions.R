 
 

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
  
  GWSDAT_Options[['Aggby']] <- 'Month'                  #Default Aggregate by option in Spatial plot is monthly       
  GWSDAT_Options[['AggMethod']] <- 'Mean'               #If multiple groundwater elevations sampled in same aggregate by window then use mean to aggregate
  GWSDAT_Options[['NDMethod']] <- 'Half of ND Value'    #Defult non-detect - handling is substitute by half detection limit
  GWSDAT_Options[['ModelMethod']] <- 'pspline'          #Legacy - ignore 
  GWSDAT_Options[['cross']] <- 10                       #Legacy - ignore 
  GWSDAT_Options[['Tune']] <- TRUE                      #Legacy - ignore 
  GWSDAT_Options[['gamma']] <- c(0)                     #Legacy - ignore    
  GWSDAT_Options[['cost']] <- 2^c(0,1,2,3,4,5)          #Legacy - ignore 
  GWSDAT_Options[['PSplineVars']] <- list()
  GWSDAT_Options[['PSplineVars']][['NIG.a']] <- 0.0001  #Parameter a in Bayesian spatiotemporal smoothing section of paper doi: 10.1002/env.2347.
  GWSDAT_Options[['PSplineVars']][['NIG.b']] <- 0.0001  #Parameter b in Bayesian spatiotemporal smoothing section of paper doi: 10.1002/env.2347.
  GWSDAT_Options[['PSplineVars']][['pord']] <- 1        #Order of difference penalty in penalised spline methodology
  GWSDAT_Options[['PSplineVars']][['bdeg']] <- 2        #Order of polynomial in penalised spline methodology
  GWSDAT_Options[['PSplineVars']][['Trial.Lambda']] <- 10^seq(-6,0,length = 30) #Trial value of Lambda smoothing parameter
  GWSDAT_Options[['PSplineVars']][['nseg']] <- 6       #number of segments - i.e the model resolution 
  GWSDAT_Options[['DefContThresh']] <- 500             #Default of 500 ug/L for Trend/Threshold indicator matrix plot. 
  GWSDAT_Options[['DefPlumeThresh']] <- 10             #Default of 10 ug/L for plume dileniation. 
  GWSDAT_Options[['DefPorosity']] <- 0.25              #Default of 25% ground porosity fr plume mass estimation
  GWSDAT_Options[['smThreshSe']] <- 1.1512             #constant used for confidence bands in well time series smoother. 
  GWSDAT_Options[['smMethod']] <- 'aicc'               #default method for picking smoothness parameter in well time series smoother. 
  GWSDAT_Options[['Version']] <- packageVersion('GWSDAT') #GWSDAT package Version.
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
