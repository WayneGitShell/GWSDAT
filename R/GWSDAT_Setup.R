########################################################################################################################

GWSDAT.excelDate2Date<-function (excelDate) 
{
    Date <- excelDate + as.Date("1900-01-01") - 2
    return(Date)
}
#----------------------------------------------------------------------------------------------------------------------#


########################################################################################################################

.my.tkdev <- function (hscale = 1, vscale = 1){
win.metafile(width = 4 * hscale, height = 4 * vscale, restoreConsole =FALSE)
}

#----------------------------------------------------------------------------------------------------------------------#



GWSDAT_Setup <- function(){

  GWSDAT_Options<-list()
  GWSDAT_Options[['Aggby']]<-'Monthly'
  GWSDAT_Options[['AggMethod']]<-'Mean'
  GWSDAT_Options[['NDMethod']]<-'Half of ND Value'
  GWSDAT_Options[['ModelMethod']]<-'pspline'
  GWSDAT_Options[['cross']]<-10
  GWSDAT_Options[['Tune']]<-TRUE
  GWSDAT_Options[['gamma']]<-c(0)
  GWSDAT_Options[['cost']]<-2^c(0,1,2,3,4,5)
  GWSDAT_Options[['PSplineVars']]<-list()
  GWSDAT_Options[['PSplineVars']][['NIG.a']]<-0.0001
  GWSDAT_Options[['PSplineVars']][['NIG.b']]<-0.0001
  GWSDAT_Options[['PSplineVars']][['pord']]<-1
  GWSDAT_Options[['PSplineVars']][['bdeg']]<-2
  GWSDAT_Options[['PSplineVars']][['Trial.Lambda']]<-10^seq(-6,0,length=30)
  GWSDAT_Options[['PSplineVars']][['nseg']]<-6
  GWSDAT_Options[['OutputGraphics']]<-'wmf'
  GWSDAT_Options[['DefContThresh']]<-500
  GWSDAT_Options[['DefPlumeThresh']]<-10
  GWSDAT_Options[['DefPorosity']]<-0.25
  GWSDAT_Options[['smThreshSe']]<-1.1512
  GWSDAT_Options[['smThreshSe']]<-as.numeric(GWSDAT_Options[['smThreshSe']])
  GWSDAT_Options[['smMethod']]<-'aicc'
  GWSDAT_Options[['SiteName']]<-'GWSDAT Basic Example'
  GWSDAT_Options[['Version']]<-'2.11'
  GWSDAT_Options[['Version']]<-as.numeric(GWSDAT_Options[['Version']])
  
  XtempScreenExp<-as.character('1,6')
  YtempScreenExp<-as.character('1,125')
  XtempScreenExp<-gsub(',','.',XtempScreenExp)
  YtempScreenExp<-gsub(',','.',YtempScreenExp)
  XtempScreenExp<-as.numeric(XtempScreenExp)
  YtempScreenExp<-as.numeric(YtempScreenExp)
  
  if(is.na(XtempScreenExp)){XtempScreenExp<-1}
  if(is.na(YtempScreenExp)){YtempScreenExp<-1}
  
  GWSDAT_Options[['Scale.panelImage']]<-list(hscale=as.numeric(XtempScreenExp),vscale=as.numeric(YtempScreenExp))
  
  if(exists('YtempScreenExp')){rm(YtempScreenExp)}
  if(exists('XtempScreenExp')){rm(XtempScreenExp)}
  
  #GWSDAT_Options[['RScriptFilename']] <- 'C:/Users/Andruscha/Desktop/DropboxDrive/Dropbox/1_GWSDAT/3_Shiny_Dev/Shiny_GWSDAT/R_entry_point.R'
  GWSDAT_Options[['GWDSDATHome']] <- '.'
  GWSDAT_Options[['WellDataFilename']] <- 'data/BasicExample_WellData.csv'
  GWSDAT_Options[['WellCoordsFilename']] <- 'data/BasicExample_WellCoords.csv'
  GWSDAT_Options[['ShapeFileNames']]<-NULL
  GWSDAT_Options[['UseGWSDATLib']]<-FALSE
  
  GWSDATHome <- GWSDAT_Options$GWDSDATHome
  UseGWSDATLib <- GWSDAT_Options$UseGWSDATLib

  
  
  
  ##
  ## Setup the libPaths to the additional packages required by GWSDAT.
  ##
  if( as.numeric(R.Version()$major)==2){
  
  	try(.libPaths(c(paste(GWSDATHome,'R',paste('RLibsMajVer2',sep=''),sep='/'),.libPaths())))
  
  }else{
  
  	try(.libPaths(c(paste(GWSDATHome,'R',paste('RLibsMajVer3',sep=''),sep='/'),.libPaths())))
  
  }
  
  try(assign('.lib.loc', shortPathName(get('.lib.loc', envir = environment(.libPaths))),envir = environment(.libPaths)))
  
  
  if(!UseGWSDATLib){
  
  	source(paste(GWSDATHome, "R/GWSDAT Input Data.R", sep="/"))
  	source(paste(GWSDATHome, "R/GWSDAT Traffic Lights.R", sep="/"))
  	source(paste(GWSDATHome, "R/GWSDAT GWFlow.R", sep="/"))
  	
    # replace this one by Plot_* function below
    source(paste(GWSDATHome, "R/GWSDAT MakePanel.R", sep="/"))
  	
    source(paste(GWSDATHome, "R/GWSDAT.filled.contour.R", sep="/"))
  	source(paste(GWSDATHome, "R/GWSDAT SVM.R", sep="/"))
  	source(paste(GWSDATHome, "R/GWSDAT PSpline Utils.R", sep="/"))
  	source(paste(GWSDATHome, "R/GWSDAT Auto Gamma.R", sep="/"))
  	source(paste(GWSDATHome, "R/GWSDAT Shapefile Functions.R", sep="/"))
    # added 07/2017
    source(paste(GWSDATHome, "R/GWSDAT_Init.R", sep="/"))
    source(paste(GWSDATHome, "R/GWSDAT_Msg.R", sep="/"))
    source(paste(GWSDATHome, "R/Plot_SmoothTimeSeries.R", sep="/"))
    source(paste(GWSDATHome, "R/Plot_ImagePlot.R", sep="/"))
    source(paste(GWSDATHome, "R/Plot_TrafficTable.R", sep="/"))
    source(paste(GWSDATHome, "R/Create_PanelAttr.R", sep="/"))
    
    
  	if(!GWSDAT_Load_Libs()){stop("Missing packages")}
  
  } else{
  
    ##
    ## Try to load the GWSDAT package. [CURRENTLY NOT SUPPORTED]
    ##  
    ##Note: the tcltk code has to be changed to Shiny; if this is going to be low level, just 
    ##      print to stdout.
    ##
    	if(!require(GWSDAT)){
  	
  		  #require("tcltk")
  		  #tkmessageBox(title="An error has occured!",message=paste("Cannot find package \"","GWSDAT","\"",sep=""),icon="error",type="ok")
  		  #stop("Cannot find package GWSDAT")
    	  
    	  return(GWSDAT_Error("Error: Cannot find package GWSDAT."))
    
  	}
  
  }
  
  set.seed(1)
  
  return(GWSDAT_Options)
}
#----------------------------------------------------------------------------------------------------------------------#
