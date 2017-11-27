
GWSDAT_Options <- list()

GWSDAT_Options[['Aggby']] <- 'Month'   # 'Day', 'Month', 'Quarter', 'Year'
GWSDAT_Options[['AggMethod ']] <- 'Mean'
GWSDAT_Options[['NDMethod']] <- 'Half of ND Value'
GWSDAT_Options[['ModelMethod']] <- 'pspline'
GWSDAT_Options[['cross']] <- 10
GWSDAT_Options[['Tune']]  <- TRUE
GWSDAT_Options[['gamma']] <- c(0)
GWSDAT_Options[['cost']]  <- 2^c(0,1,2,3,4,5)
GWSDAT_Options[['PSplineVars']] <- list()
GWSDAT_Options[['PSplineVars']][['NIG.a']] <- 0.0001
GWSDAT_Options[['PSplineVars']][['NIG.b']] <- 0.0001
GWSDAT_Options[['PSplineVars']][['pord']]  <- 1
GWSDAT_Options[['PSplineVars']][['bdeg']]  <- 2
GWSDAT_Options[['PSplineVars']][['Trial.Lambda']] <- 10^seq(-6, 0, length = 30)
GWSDAT_Options[['PSplineVars']][['nseg']] <- 6
GWSDAT_Options[['DefContThresh']]  <- 500
GWSDAT_Options[['DefPlumeThresh']] <- 10
GWSDAT_Options[['DefPorosity']] <- 0.25
GWSDAT_Options[['smThreshSe']] <- 1.1512
GWSDAT_Options[['smThreshSe']] <- as.numeric(GWSDAT_Options[['smThreshSe']])
GWSDAT_Options[['smMethod']] <- 'aicc'
GWSDAT_Options[['Version']] <- '2.11'
GWSDAT_Options[['Version']] <- as.numeric(GWSDAT_Options[['Version']])
GWSDAT_Options[['ShapeFileNames']] <- NULL


# 'Basic Example'
GWSDAT_Options[['SiteName']] <- 'Basic Example'
GWSDAT_Options[['WellDataFilename']]   <- 'D:/1_Arbeit/1_GWSDAT/3_Shiny_Dev/GWSDAT/data/BasicExample_WellData.csv'
GWSDAT_Options[['WellCoordsFilename']] <- 'D:/1_Arbeit/1_GWSDAT/3_Shiny_Dev/GWSDAT/data/BasicExample_WellCoords.csv'

# 'Comprehensive Example'
#GWSDAT_Options[['SiteName']] <- 'Comprehensive Example'
#GWSDAT_Options[['WellDataFilename']]   <- 'D:/1_Arbeit/1_GWSDAT/3_Shiny_Dev/GWSDAT/data/ComprehensiveExample_WellData.csv'
#GWSDAT_Options[['WellCoordsFilename']] <- 'D:/1_Arbeit/1_GWSDAT/3_Shiny_Dev/GWSDAT/data/ComprehensiveExample_WellCoords.csv'
#GWSDAT_Options[['ShapeFileNames']]     <- c('D:/1_Arbeit/1_GWSDAT/3_Shiny_Dev/GWSDAT/data/GIS_Files/GWSDATex2.shp')



#
# Plume Areas (different Aquifer)
#
#GWSDAT_Options[['SiteName']] <- 'Site 25'
#GWSDAT_Options[['WellDataFilename']]   <- 'D:/1_Arbeit/1_GWSDAT/3_Shiny_Dev/EPA_Data/Site_25_plumeareas_concdata.csv'
#GWSDAT_Options[['WellCoordsFilename']] <- 'D:/1_Arbeit/1_GWSDAT/3_Shiny_Dev/EPA_Data/Site_25_plumeareas_wellcoords.csv'


#
# Site 25, site-wide (no Aquifer groups)
#

#GWSDAT_Options[['SiteName']] <- 'Site 25'
#GWSDAT_Options[['WellDataFilename']]   <- 'D:/1_Arbeit/1_GWSDAT/3_Shiny_Dev/EPA_Data/Site_25_sitewide_concdata.csv'
#GWSDAT_Options[['WellCoordsFilename']] <- 'D:/1_Arbeit/1_GWSDAT/3_Shiny_Dev/EPA_Data/Site_25_sitewide_wellcoords.csv'
#GWSDAT_Options[['ShapeFileNames']] <- c(GWSDAT_Options[['ShapeFileNames']],'D:/1_Arbeit/1_GWSDAT/3_Shiny_Dev/EPA_Data/Site_25_Railroads.shp')
#GWSDAT_Options[['ShapeFileNames']] <- c(GWSDAT_Options[['ShapeFileNames']],'D:/1_Arbeit/1_GWSDAT/3_Shiny_Dev/EPA_Data/Site_25_Roads.shp')
#GWSDAT_Options[['ShapeFileNames']] <- c(GWSDAT_Options[['ShapeFileNames']],'D:/1_Arbeit/1_GWSDAT/3_Shiny_Dev/EPA_Data/Site_25_Boundary.shp')
#GWSDAT_Options[['ShapeFileNames']] <- c(GWSDAT_Options[['ShapeFileNames']],'D:/1_Arbeit/1_GWSDAT/3_Shiny_Dev/EPA_Data/Site_25_Buildings.shp')

#GWSDAT_Options[['SiteName']] <- 'New Data'
#GWSDAT_Options[['WellDataFilename']]   <- 'D:/1_Arbeit/1_GWSDAT/3_Shiny_Dev/2_Bugtesting/VBAF11FWellData.csv'
#GWSDAT_Options[['WellCoordsFilename']] <- 'D:/1_Arbeit/1_GWSDAT/3_Shiny_Dev/2_Bugtesting/VBAF11FWellCoords.csv'



#devtools::install_github("andrejadd/GWSDAT")
#library(GWSDAT)
devtools::load_all()
launchApp(GWSDAT_Options)
#launchApp(session_file = "data/GWSDAT_debug_shapefile.RData")

