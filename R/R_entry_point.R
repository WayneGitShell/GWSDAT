
R_entry_point <- function() {
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
  
  # library(tcltk)
  
  pb<-tkProgressBar('GWSDAT Progress', 'Loading R packages...',0, 1, 0)
  #GWSDAT_Options[['RScriptFilename']]<-'C:/Users/ANDRUS~1/AppData/Local/Temp/VBAF3A9RScript.R'
  GWSDAT_Options[['RScriptFilename']] <- 'C:/Users/Andruscha/Desktop/DropboxDrive/Dropbox/1_GWSDAT/3_Shiny_Dev/Shiny_GWSDAT/R_entry_point.R'
  
  #GWSDAT_Options[['GWDSDATHome']]<-'C:/Users/Andruscha/Desktop/DropboxDrive/Dropbox/1_GWSDAT/GWSDAT_v2.11'
  GWSDAT_Options[['GWDSDATHome']] <- 'C:/Users/Andruscha/Desktop/DropboxDrive/Dropbox/1_GWSDAT/3_Shiny_Dev/Shiny_GWSDAT'
  
  #GWSDAT_Options[['WellDataFilename']]<-'C:/Users/ANDRUS~1/AppData/Local/Temp/VBAF3A9WellData.csv'
  #GWSDAT_Options[['WellCoordsFilename']]<-'C:/Users/ANDRUS~1/AppData/Local/Temp/VBAF3A9WellCoords.csv'
  GWSDAT_Options[['WellDataFilename']] <- 'C:/Users/Andruscha/Desktop/DropboxDrive/Dropbox/1_GWSDAT/3_Shiny_Dev/Shiny_GWSDAT/data/BasicExample_WellData.csv'
  GWSDAT_Options[['WellCoordsFilename']] <- 'C:/Users/Andruscha/Desktop/DropboxDrive/Dropbox/1_GWSDAT/3_Shiny_Dev/Shiny_GWSDAT/data/BasicExample_WellCoords.csv'
  
  #GWSDAT_Options[['RtermHome']]<-'D:/Win10_ProgramFiles/R/R-3.3.1/bin/x64/Rterm.exe'
  
  GWSDAT_Options[['ShapeFileNames']]<-NULL
  GWSDAT_Options[['UseGWSDATLib']]<-FALSE
  
  source(paste(GWSDAT_Options[['GWDSDATHome']],'/R/GWSDAT Setup.R',sep=''))
  
  GWSDAT.Setup(GWSDAT_Options)
  
  #source(paste(GWSDAT_Options[['GWDSDATHome']],'/R/GWSDAT Run.R',sep=''))
  source(paste(GWSDAT_Options[['GWDSDATHome']],'/R/GWSDAT_Run_shiny.R',sep=''))
  GWSDAT_Run_shiny(GWSDAT_Options)
  
}
