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






########################################################################################################################

GWSDAT.Setup<-function(GWSDAT_Options){


GWSDATHome<-GWSDAT_Options$GWDSDATHome
UseGWSDATLib<-GWSDAT_Options$UseGWSDATLib


if( as.numeric(R.Version()$major)==2){

	try(.libPaths(c(paste(GWSDATHome,'R',paste('RLibsMajVer2',sep=''),sep='/'),.libPaths())))

}else{

	try(.libPaths(c(paste(GWSDATHome,'R',paste('RLibsMajVer3',sep=''),sep='/'),.libPaths())))

}



try(assign('.lib.loc', shortPathName(get('.lib.loc', envir = environment(.libPaths))),envir = environment(.libPaths)))



if(!UseGWSDATLib){

	source(paste(GWSDATHome , "/R/GWSDAT Input Data.R",sep=""))
	source(paste(GWSDATHome , "/R/GWSDAT Traffic Lights.R",sep=""))
	source(paste(GWSDATHome , "/R/GWSDAT GWFlow.R",sep=""))
	source(paste(GWSDATHome , "/R/GWSDAT MakePanel.R",sep=""))
	source(paste(GWSDATHome , "/R/GWSDAT.filled.contour.R",sep=""))
	source(paste(GWSDATHome , "/R/GWSDAT SVM.R",sep=""))
	source(paste(GWSDATHome , "/R/GWSDAT PSpline Utils.R",sep=""))
	source(paste(GWSDATHome , "/R/GWSDAT Auto Gamma.R",sep=""))
	source(paste(GWSDATHome , "/R/GWSDAT Shapefile Functions.R",sep=""))
	
	if(!GWSDAT.Load.Libs()){stop("Missing packages")}

}else{


	if(!require(GWSDAT)){
	
		require("tcltk")
		tkmessageBox(title="An error has occured!",message=paste("Cannot find package \"","GWSDAT","\"",sep=""),icon="error",type="ok")
		stop("Cannot find package GWSDAT")
	
	}


}

set.seed(1)

}
#----------------------------------------------------------------------------------------------------------------------#
