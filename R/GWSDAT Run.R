

############################# Read Historical Monitoring Data Table ####################################################

AGALL<-try(read.csv(GWSDAT_Options$WellDataFilename))
if(!"flags" %in% tolower(names(AGALL))){AGALL$Flags<-rep(NA,nrow(AGALL))}
AGALL<-try(AGALL[,which(tolower(names(AGALL))=="wellname"):which(tolower(names(AGALL))=="flags")])

if(inherits(AGALL, 'try-error')){

tkmessageBox(title="Error!",message="Error reading Monitoring data.",icon="error",type="ok")
  
try(close(pb))
stop("Error in inputting and formatting data.")

}

AGALL$SampleDate<-GWSDAT.excelDate2Date(floor(as.numeric(as.character(AGALL$SampleDate)))) ##added floor function 28/6/2012


if(any(is.na(AGALL$SampleDate))){

AGALL<-AGALL[!is.na(AGALL$SampleDate),]

tkmessageBox(title="Warning!",message="Incorrect input date value(s) detected.",icon="error",type="ok")

	if(nrow(AGALL)==0 || as.character(tkmessageBox(message="Do you wish to continue?",icon="question",type="yesno",default="yes"))!="yes"){

		try(close(pb))	
		stop("Stop due to bad dates")
			
	}else{
		tkmessageBox(title="Warning!",message="Incorrect date values data will be omitted.",icon="error",type="ok")
		

	}

}
#----------------------------------------------------------------------------------------------------------------------#



############################# Read Well Coordinates Data Table #########################################################

WellCoords<-try(read.csv(GWSDAT_Options$WellCoordsFilename))

GWSDAT_Options$WellCoordsLengthUnits<-as.character(WellCoords$CoordUnits[1])

if(length(GWSDAT_Options$WellCoordsLengthUnits)==0 || is.na(GWSDAT_Options$WellCoordsLengthUnits)){
  GWSDAT_Options$WellCoordsLengthUnits<-NULL
}

if(!"aquifer" %in% tolower(names(WellCoords))){
  WellCoords$Aquifer<-rep(NA,nrow(WellCoords))
}

WellCoords <- try(WellCoords[,which(tolower(names(WellCoords))=="wellname"):which(tolower(names(WellCoords))=="aquifer")]) #Ensuring correct columns are selected

if(inherits(WellCoords, 'try-error')){

  tkmessageBox(title="Error!",message="Error reading Well Coordinates data.",icon="error",type="ok")
  try(close(pb))
    stop("Error in inputting and formatting data.")

}
#----------------------------------------------------------------------------------------------------------------------#


############################# Consolidate Data together ################################################################

All.Data<-try(GWSDAT.Init(AGALL,WellCoords,GWSDAT_Options))

if(inherits(All.Data, 'try-error')){

	tkmessageBox(title="Error!",message="Error in inputting and formatting data.",icon="error",type="ok")
	try(close(pb))
	stop("Error in inputting and formatting data.")

}

try(rm(list=c('AGALL','WellCoords')))
NumConts = length(All.Data$All.Conts)
ContNames = All.Data$All.Conts
PctDone = 1 / (NumConts + 3)
#----------------------------------------------------------------------------------------------------------------------#



############################# Fit Model to each Contaminant ############################################################

Fitted.Data<-list()



if(!tolower(GWSDAT_Options$ModelMethod) %in% c("svm","pspline")){

	GWSDAT_Options$ModelMethod<-GWSDAT.select.list(c("pspline","svm"))

}

for(i in 1:NumConts){

setTkProgressBar(pb, PctDone,NULL,paste("Fitting ",ContNames[i]," Data."))



	
	if(tolower(GWSDAT_Options$ModelMethod)=="svm"){

		temp.fit<-try(GWSDAT.svmfit(All.Data,All.Data$All.Conts[i],GWSDAT_Options))

	}else{

		temp.fit<-GWSDAT.PSplinefit(All.Data,All.Data$All.Conts[i],GWSDAT_Options)

	}





	if(inherits(temp.fit, 'try-error')){

	tkmessageBox(title="Error!",message=paste("Error in fitting",All.Data$All.Conts[i],"data."),icon="error",type="ok")


	}else{

	Fitted.Data[[All.Data$All.Conts[i]]]<-temp.fit
	
	}
PctDone = (1 + i) / (NumConts + 3)

}

if(exists('temp.fit')){try(rm(temp.fit))}

if(length(Fitted.Data)==0){

	tkmessageBox(title="Error!",message=paste("Error in fitting data."),icon="error",type="ok")
	try(close(pb))
	stop("Error in fitting data.")

}
#----------------------------------------------------------------------------------------------------------------------#


################### Traffic Lights #####################################################################################

setTkProgressBar(pb, PctDone,NULL,"Calculating Traffic Lights")

Traffic.Lights<-try(GWSDAT.Traffic.Lights(All.Data,Fitted.Data,GWSDAT_Options))
attr(Fitted.Data,'TrafficLights')<-if(!inherits(Traffic.Lights, 'try-error')){Traffic.Lights}else{NULL}
try(rm(Traffic.Lights))

PctDone = (NumConts + 2) / (NumConts + 3)
setTkProgressBar(pb, PctDone)
#----------------------------------------------------------------------------------------------------------------------#



################### Groundwater Calc####################################################################################

setTkProgressBar(pb, PctDone,NULL,"Calculating GroundWater Flows")


if(!is.null(All.Data$Agg_GW_Data)){

GW.Flows<-try(do.call('rbind',by(All.Data$Agg_GW_Data,All.Data$Agg_GW_Data$AggDate,GWSDAT.GW.Comp)))
if(!inherits(GW.Flows, 'try-error')){

	GW.Flows$R<-GW.Flows$R/quantile(GW.Flows$R,p=0.9,na.rm=T)
	GW.Flows$R[GW.Flows$R>1]<-1

} 
GW.Flows=na.omit(GW.Flows)
attr(Fitted.Data,'GWFlows')<-if(!inherits(GW.Flows, 'try-error')){GW.Flows}else{NULL}
try(rm(GW.Flows))

}
PctDone = (NumConts + 3) / (NumConts + 3)

setTkProgressBar(pb, PctDone,NULL,"Generating Panel...")
#----------------------------------------------------------------------------------------------------------------------#



############################ Clean Up #################################################################################

Curr.Site.Data=list(All.Data=All.Data,Fitted.Data=Fitted.Data,GWSDAT_Options=GWSDAT_Options)
attr(Curr.Site.Data, 'class') <- 'GWSDAT.Data'
try(close(pb))
try(rm(Fitted.Data,All.Data,GWSDAT_Options,pb))
try(rm(i,NumConts,PctDone,ContNames))
#----------------------------------------------------------------------------------------------------------------------#



######################################  Create GWSDAT Panel  ###########################################################

# plot.GWSDAT.Data(Curr.Site.Data)
#----------------------------------------------------------------------------------------------------------------------#





