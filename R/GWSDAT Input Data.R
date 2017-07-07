##############################################################################################################

GWSDAT_Load_Libs <- function(){

  Require <- function(pkg) {
     
    if (data.class(result<-try(find.package(pkg,lib.loc=.libPaths()),TRUE))=="try-error")
    {
      # tkmessageBox(title="An error has occured!",message=paste("Cannot find package \"",pkg,"\"",sep=""),icon="error",type="ok")
      
      ## pass error+message somehow back without calling tkmessageBox
      # return(GWSDAT_Error(paste("Cannot find package \"",pkg,"\"",sep="")))  
      # .. not doing this here because we need FALSE back.
      return (FALSE)
    }
    else
    {
        require(pkg,character.only=TRUE)
        return (TRUE)
    }
  }



  try(options(editor="notepad"))
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
  Require("RDCOMClient")
  
  
  return(TRUE)
}



#------------------------------------------------------------------------------------------------------------#


##############################################################################################################

GWSDAT.Create.Agg.Date<-function(x,type=c("All Dates","Monthly","Quarterly"),my.seq=NULL){

require(zoo)
type <- match.arg(type)

if(type=="All Dates"){x$AggDate<-as.Date(x$SampleDate)}

if(type=="Monthly"){
	
	if(is.null(my.seq)){my.seq<-as.Date(sort(seq.Date(max(x$SampleDate),min(x$SampleDate)-500,by="-1 month")))}
	x$AggDate<-cut.Date(x$SampleDate,breaks=my.seq,include.lowest=T,right=T,labels=as.character(my.seq[-1]))
	x$AggDate<-as.Date(as.character(x$AggDate))
}

if(type=="Quarterly"){

	if(is.null(my.seq)){my.seq<-as.Date(sort(seq.Date(max(x$SampleDate),min(x$SampleDate)-500,by="-3 months")))}
	x$AggDate<-cut.Date(x$SampleDate,breaks=my.seq,include.lowest=T,right=T,labels=as.character(my.seq[-1]))
	x$AggDate<-as.Date(as.character(x$AggDate))
}


return(x)
}
#------------------------------------------------------------------------------------------------------------#







##############################################################################################################

GWSDAT.Create.Agg<-function(x,Well.Coords,type=c("mean","median","min","max")){

require(zoo)
type <- match.arg(type)

out<-aggregate(x$Result.Corr.ND,by=list(AggDate=x$AggDate,WellName=x$WellName,Constituent=x$Constituent),type)
names(out)[which(names(out)=="x")]<-"Result.Corr.ND"
out$XCoord<-Well.Coords[match(as.character(out$WellName),as.character(Well.Coords$WellName)),]$XCoord
out$YCoord<-Well.Coords[match(as.character(out$WellName),as.character(Well.Coords$WellName)),]$YCoord

if(is.factor(out$AggDate)){
out$AggDate<-as.Date(as.numeric(as.character(out$AggDate))) #Compatibility with older versions which returns factor
}




out$XCoord<-as.numeric(as.character(out$XCoord))
out$YCoord<-as.numeric(as.character(out$YCoord))

return(out)
}
#------------------------------------------------------------------------------------------------------------#



##############################################################################################################

GWSDAT.Create.Agg.GW<-function(x,Well.Coords,type=c("mean","median","min","max")){

#require(zoo)
type <- match.arg(type)


out<-aggregate(x$Result,by=list(AggDate=x$AggDate,WellName=x$WellName),type)
names(out)[which(names(out)=="x")]<-"Result"

out$XCoord<-Well.Coords[match(as.character(out$WellName),as.character(Well.Coords$WellName)),]$XCoord
out$YCoord<-Well.Coords[match(as.character(out$WellName),as.character(Well.Coords$WellName)),]$YCoord

if(is.factor(out$AggDate)){
out$AggDate<-as.Date(as.numeric(as.character(out$AggDate))) #Compatibility with older versions of R
}

out$XCoord<-as.numeric(as.character(out$XCoord))
out$YCoord<-as.numeric(as.character(out$YCoord))

return(out)
}
#------------------------------------------------------------------------------------------------------------#


##############################################################################################################

GWSDAT.rm.spaces<-function(x){

#Function to remove trailing and leading spaces!
if(!is.character(x)){stop("not of class character")}
x<-sub('[[:blank:]]+?','',x)
x<-sub(" *$","",x)
return(x)
}
#------------------------------------------------------------------------------------------------------------#














################################# GWSDAT.Init Function #######################################################

GWSDAT.Init.Data <- function(AG.ALL,Well.Coords,GWSDAT_Options){


#################Input Shapefiles ########################

ShapeFiles<-NULL

if(length(GWSDAT_Options$ShapeFileNames)>0){
ShapeFiles<-list()
mycount<-0

	for(i in 1:length(GWSDAT_Options$ShapeFileNames)){
	
	tempShapeFile<-try(GWSDAT.readShapeFile(GWSDAT_Options$ShapeFileNames[i]))
	
	

		if(inherits(tempShapeFile, "try-error")){
		
			tkmessageBox(title="Warning!",message=paste("Error Inputting '",GWSDAT_Options$ShapeFileNames[i],"' shapefile",sep=""),
			icon="warning",type="ok")

			if(length( grep(".shp",tolower(GWSDAT_Options$ShapeFileNames[i])))==0){

				tkmessageBox(title="Warning!",message=paste("Shape Files usually have a .shp file extension"),
				icon="warning",type="ok")

			}

		
			if(file.exists(GWSDAT_Options$ShapeFileNames[i])!=TRUE){

				tkmessageBox(title="Warning!",message=paste("File: '",GWSDAT_Options$ShapeFileNames[i],"' does not exist.",sep=""),
				icon="warning",type="ok")

			}
		
			if(as.character(tkmessageBox(message="Do you wish to continue?",icon="question",type="yesno",default="yes"))!="yes"){
				stop("Error Inputting ShapeFile")
			}


		}else{

			mycount<-mycount+1
			ShapeFiles[[mycount]]<-tempShapeFile
			

		}

	}

if(length(ShapeFiles)==0){ShapeFiles<-NULL}
}




#--------------------------------------------------------#

#################Input Date field and format ###########################


if(class(AG.ALL$SampleDate)[1] %in% c("POSIXct","POSIXt","Date")){

if(class(AG.ALL$SampleDate)[1] %in% c("POSIXct","POSIXt")){

	AG.ALL$SampleDate<-format(AG.ALL$SampleDate+12*60*60,"%Y-%m-%d") #Watch for time zone differences here!
	AG.ALL$SampleDate<-as.Date(AG.ALL$SampleDate,"%Y-%m-%d")

}

}else{

	tkmessageBox(title="Error!",message="Trouble reading Date Format, Please convert Input Excel Data to Date format",icon="error",type="ok")
	stop("Trouble reading Date Format, Please convert Input Excel Data to Date format")

}
#--------------------------------------------------------#



#Pick up Electron Acceptors before deleting non-aquifer wells. 
ElecAccepts<-unique(as.character(AG.ALL[ tolower(as.character(AG.ALL$Flags)) %in% c("e-acc","notinnapl","redox"),"Constituent"]))


############ Well Data Sort  #############################

AG.ALL$WellName<-factor(GWSDAT.rm.spaces(as.character(AG.ALL$WellName)))
Well.Coords$WellName<-factor(GWSDAT.rm.spaces(as.character(Well.Coords$WellName)))



#### Aquifer Selection ####################################

if(!all(is.na(Well.Coords$Aquifer)) && !all(as.character(Well.Coords$Aquifer)=="")){

	
	un.Aq.sel<-unique(as.character(Well.Coords$Aquifer))
	if(any(un.Aq.sel=="")){un.Aq.sel[un.Aq.sel==""]<-"Blank"}
	if(length(un.Aq.sel)==1){Aq.sel<-as.character(un.Aq.sel)}else{Aq.sel<-GWSDAT.select.list(un.Aq.sel,title="Select an Aquifer to Analyse")}
	if(Aq.sel==""){stop("User must select an Aquifer")}
	if(Aq.sel=="Blank"){Aq.sel<-""}
	Well.Coords<-Well.Coords[as.character(Well.Coords$Aquifer)==Aq.sel,]
	AG.ALL<-AG.ALL[as.character(AG.ALL$WellName) %in% as.character(Well.Coords$WellName),]
	
}else{

Aq.sel<-""

}

Well.Coords<-Well.Coords[,c("WellName","XCoord","YCoord")]


#---------------------------------------------------------#



All.Wells<-unique(as.character(AG.ALL$WellName))
Well.Coords$XCoord<-as.numeric(GWSDAT.rm.spaces(as.character(Well.Coords$XCoord)))
Well.Coords$YCoord<-as.numeric(GWSDAT.rm.spaces(as.character(Well.Coords$YCoord)))
Well.Coords<-na.omit(Well.Coords)
Well.Coords<-unique(Well.Coords)


if(any(table(Well.Coords$WellName)>1)){
tkmessageBox(title="Error!",message="Non-Unique Well Names in Well Coords Table.",icon="error",type="ok")
stop("Non Unique Well Names")
}

if(nrow(unique(Well.Coords[,c("XCoord","YCoord")]))<nrow(Well.Coords)){
tkmessageBox(title="Warning!",message="Non-Unique Well Coordinates in Well Coords Table. \nGroundwater elevations for non-unique well coordinates will be substituted by their mean value.",icon="warning",type="ok")
if(as.character(tkmessageBox(message="Do you wish to continue?",icon="question",type="yesno",default="yes"))!="yes"){
stop("Non Unique Well Coordinates")
}
}

Well.Coords<-Well.Coords[as.character(Well.Coords$WellName) %in% All.Wells,]

if(length(setdiff(All.Wells,as.character(Well.Coords$WellName)))!=0){

tkmessageBox(title="Warning!",message=paste("Missing Well Coordinates for: ",paste(setdiff(All.Wells,as.character(Well.Coords$WellName)),collapse=", ")),icon="warning",type="ok")

	if(as.character(tkmessageBox(message="Do you wish to continue?",icon="question",type="yesno",default="yes"))=="yes"){

		AG.ALL<-AG.ALL[as.character(AG.ALL$WellName) %in% All.Wells,]
		AG.ALL$WellName<-factor(as.character(AG.ALL$WellName))}else{

		stop("Missing Well Coordinates")
	}

}

Well.Area<-areapl(as.matrix(Well.Coords[chull(Well.Coords[,c("XCoord","YCoord")]),c("XCoord","YCoord")]))
#--------------------------------------------------------#



####################### Contaminant Data ###############################################################


################ Flags Handling ##################################
if(!all(is.na(AG.ALL$Flags))){  

	AG.ALL<-AG.ALL[tolower(as.character(AG.ALL$Flags))!="omit",]


}else{

	AG.ALL$Flags<-rep("",nrow(AG.ALL))

}


#-----------------------------------------------------------------#

AG.ALL$Constituent<-factor(GWSDAT.rm.spaces(as.character(AG.ALL$Constituent)))

#### Case Sensitivity of Constituents! ##########################
if(length(unique(as.character(AG.ALL$Constituent)))!=length(unique(toupper(as.character(AG.ALL$Constituent))))){

	tkmessageBox(title="Warning!",message="Constituent types have different letter cases (e.g. 'MTBE v mtbe') 
	\n GWSDAT will modify all constituent types to upper case.",icon="warning",type="ok")

	AG.ALL$Constituent<-factor(toupper(as.character(AG.ALL$Constituent)))
}



AG.ALL$Result<-factor(GWSDAT.rm.spaces(as.character(AG.ALL$Result)))
AG.ALL$Units<-factor(GWSDAT.rm.spaces(as.character(AG.ALL$Units)))
Cont.Data<-AG.ALL[tolower(as.character(AG.ALL$Constituent))!="gw",]


############### Contaminant Data Type Processing #####################
ContTypeData="Default"

if(nrow(Cont.Data)==0){ #GW only type data. 

	ContTypeData="NoConcData"
	Cont.Data<-rbind(Cont.Data,data.frame(WellName=as.character(All.Wells[1]),Constituent=" ",#Constituent="Dummy", 
	SampleDate=max(AG.ALL$SampleDate),Result=NA,Units="ug/l",Flags=""))
	
}

if(nrow(Cont.Data[tolower(as.character(Cont.Data$Constituent))!="napl",])==0){ #NAPL Only type data

	ContTypeData="NoConcData"
	Cont.Data<-rbind(Cont.Data,data.frame(WellName=as.character(All.Wells[1]),Constituent=" ",#Constituent="Dummy", 
	SampleDate=max(AG.ALL$SampleDate),Result=NA,Units="ug/l",Flags=""))
}


Cont.Data$Constituent<-factor(as.character(Cont.Data$Constituent))
All.Conts<-unique(as.character(Cont.Data$Constituent))


########################## Units Checking #########################


if(any(!tolower(as.character(Cont.Data$Units[tolower(as.character(Cont.Data$Constituent))!="napl"])) %in% c("ug/l","mg/l","ng/l"))){

tkmessageBox(title="Units Error",message="Solute data must be one of 'ng/l', 'ug/l' or 'mg/l'.\n\nPlease correct and re-run GWSDAT analysis.",icon="error",type="ok")
stop("Incorrect Solute Units")


}

#-----------------------------------------------------------------#



Cont.Data$ND<-rep(FALSE,nrow(Cont.Data))
Cont.Data$ND[grep("<",as.character(Cont.Data$Result))]<-TRUE
Cont.Data$Result.Corr.ND<-rep(NA,nrow(Cont.Data))
Cont.Data$Result.Corr.ND[!Cont.Data$ND]<-as.numeric(as.character(Cont.Data$Result[!Cont.Data$ND]))


############# Checking for 0 conc concentration data ##############
if(any(Cont.Data$Result.Corr.ND[tolower(as.character(Cont.Data$Constituent))!="napl"]==0,na.rm=TRUE)){

tkmessageBox(title="Error",message="Zero solute concentration data detected in input data - this is not permissible.\n\nPlease correct and re-run GWSDAT analysis.",icon="error",type="ok")

stop("Zero Concentration data values!")
}
#-----------------------------------------------------------------#



temp.hold<-sub(".*<", "", as.character(Cont.Data$Result[Cont.Data$ND]))

if(any(grep("nd",temp.hold,ignore.case =T))){

tkmessageBox(title="Warning!",message="'<ND' detected. Non-Detect limits must be specified. Omitting unspecified Non Detect Data.",icon="warning",type="ok")
}
Cont.Data$Result.Corr.ND[Cont.Data$ND]<-as.numeric(temp.hold)



##############Solute Unit Handling converts all ng/l and mg/l to ug/l#################

Cont.Data$Units<-tolower(as.character(Cont.Data$Units))

if(any(grep("mg",Cont.Data$Units))){

Cont.Data$Units[grep("mg",Cont.Data$Units)]<-"mg/l"
Cont.Data$Result.Corr.ND[Cont.Data$Units=="mg/l"]<-1000*Cont.Data$Result.Corr.ND[Cont.Data$Units=="mg/l"]
Cont.Data$Result<-as.character(Cont.Data$Result)
Cont.Data$Result[Cont.Data$Units=="mg/l" & !Cont.Data$ND]<-as.character(Cont.Data$Result.Corr.ND[Cont.Data$Units=="mg/l" & !Cont.Data$ND])
Cont.Data$Result[Cont.Data$Units=="mg/l" & Cont.Data$ND]<-paste("ND<",as.character(Cont.Data$Result.Corr.ND[Cont.Data$Units=="mg/l" & Cont.Data$ND]),sep="")
Cont.Data$Result<-factor(as.character(Cont.Data$Result))
Cont.Data$Units[Cont.Data$Units=="mg/l"]<-"ug/l"

}

if(any(grep("ng",Cont.Data$Units))){

Cont.Data$Units[grep("ng",Cont.Data$Units)]<-"ng/l"
Cont.Data$Result.Corr.ND[Cont.Data$Units=="ng/l"]<-0.001*Cont.Data$Result.Corr.ND[Cont.Data$Units=="ng/l"]
Cont.Data$Result<-as.character(Cont.Data$Result)
Cont.Data$Result[Cont.Data$Units=="ng/l" & !Cont.Data$ND]<-as.character(Cont.Data$Result.Corr.ND[Cont.Data$Units=="ng/l" & !Cont.Data$ND])
Cont.Data$Result[Cont.Data$Units=="ng/l" & Cont.Data$ND]<-paste("ND<",as.character(Cont.Data$Result.Corr.ND[Cont.Data$Units=="ng/l" & Cont.Data$ND]),sep="")
Cont.Data$Result<-factor(as.character(Cont.Data$Result))
Cont.Data$Units[Cont.Data$Units=="ng/l"]<-"ug/l"

}


Cont.Data$Units<-factor(as.character(Cont.Data$Units))
#-------------------------------------------------------------------#







##############NAPL handling Subsituted with max observed value on a cont by cont basis#################

if("napl" %in% tolower(as.character(Cont.Data$Constituent))){

print("NAPL Detected!")
NAPL.Thickness.Data <- Cont.Data[tolower(as.character(Cont.Data$Constituent))=="napl",]




###################### NAPL Units Checking #########################

NAPL.Units<-unique(tolower(as.character(NAPL.Thickness.Data$Units)))

if(length(NAPL.Units)>1){
tkmessageBox(title="Units Error",message="Multiple units detected for NAPL thickness in input dataset. \nPlease ensure same thickness units are used throughout.",icon="error",type="ok")
stop("Multiple NAPL Units")
}

if(length(NAPL.Units)>0){
if(!NAPL.Units %in% c("level","mm","cm","metres","inches","feet")){
tkmessageBox(title="Units Error",message="NAPL thickness units must be one of 'level', 'mm', 'cm', 'metres', 'inches' or 'feet'.\n\nPlease correct and re-run GWSDAT analysis.",icon="error",type="ok")
stop("Incorrect NAPL Units")
}
}

#-----------------------------------------------------------------#


NAPL.Thickness.Data <- try(NAPL.Thickness.Data[order(NAPL.Thickness.Data$SampleDate),])
NAPL.Thickness.Data[,c("XCoord","YCoord")]<-Well.Coords[match(as.character(NAPL.Thickness.Data$WellName),as.character(Well.Coords$WellName)),c("XCoord","YCoord")]


if(ContTypeData=="NoConcData" || as.character(tkmessageBox(message="Do you wish to substitute NAPL values with maximum observed solute concentrations? 
\nNote: NAPL measurements for electron acceptor, Redox or 'NotInNapl' flagged constituents will be ignored.",icon="question",type="yesno",default="yes",title="NAPL Data Detected"))=="yes"){

	

	All.Conts.No.NAPL<-All.Conts[tolower(All.Conts)!="napl"]
	All.Conts.No.NAPL<-setdiff(All.Conts.No.NAPL,ElecAccepts) #omit e-acc constituent from NAPL set
	

	############################# NAPL and dissolved conflict resolution ##########################################
	NAPLWellandDates<-   unique(Cont.Data[tolower(Cont.Data$Constituent)=="napl",c("WellName","SampleDate","Constituent"),drop=FALSE])[,1:2]
	NonNAPLWellandDates<-unique(Cont.Data[tolower(Cont.Data$Constituent)!="napl" & !is.na(Cont.Data$Result.Corr.ND),c("WellName","SampleDate","Constituent"),drop=FALSE])[,1:2]
	NAPLConflictDateandWells<-NonNAPLWellandDates[which(apply(NonNAPLWellandDates,1,paste,collapse="") %in% apply(NAPLWellandDates,1,paste,collapse="")),]
	NAPLConflictDateandWells<-unique(NAPLConflictDateandWells); 
	
	if(nrow(NAPLConflictDateandWells)>0){

	
	if(all(Cont.Data[apply(Cont.Data[,c("WellName","SampleDate")],1,paste,collapse="") %in% apply(NAPLConflictDateandWells,1,paste,collapse="") 
		& tolower(Cont.Data$Constituent)=="napl",]$Result.Corr.ND==0)){myans<-"yes"}else{

		myans<-as.character(tkmessageBox(message="Concentration data reported in presence of NAPL. Do you wish to use concentration data (Yes) or substitue these NAPL values with maximum observed solute concentrations (No)?
		\nNote: NAPL measurements for electron acceptor, Redox or 'NotInNapl' flagged constituents will be ignored.",icon="question",type="yesno",
		default="yes",title="NAPL Data Conflict"))
	}

	if(myans=="no"){




		for(i in 1:nrow(NAPLConflictDateandWells)){
			Cont.Data<-Cont.Data[-which(as.character(Cont.Data$WellName)==as.character(NAPLConflictDateandWells$WellName)[i] & 
			Cont.Data$SampleDate==NAPLConflictDateandWells$SampleDate[i] & tolower(as.character(Cont.Data$Constituent))!="napl"),]
		}

	}else{


		for(i in 1:nrow(NAPLConflictDateandWells)){
			Cont.Data<-Cont.Data[-which(as.character(Cont.Data$WellName)==as.character(NAPLConflictDateandWells$WellName)[i] & 
			Cont.Data$SampleDate==NAPLConflictDateandWells$SampleDate[i] & tolower(as.character(Cont.Data$Constituent))=="napl"),]
		}



	}

	}


	
	#-------------------------------------------------------------------------------------------------------------#

	NAPL.Data<-Cont.Data[tolower(as.character(Cont.Data$Constituent))=="napl",]
	No.NAPL.Data<-Cont.Data[tolower(as.character(Cont.Data$Constituent))!="napl",]
	No.NAPL.Data$Constituent<-factor(as.character(No.NAPL.Data$Constituent))
	No.NAPL.Data<-No.NAPL.Data[,c("WellName","Constituent","SampleDate","Result","Units","ND","Result.Corr.ND")]



	New.NAPL.Data<-data.frame(
	WellName=rep(NAPL.Data$WellName,length(All.Conts.No.NAPL)),
	Constituent=rep(All.Conts.No.NAPL,each=nrow(NAPL.Data)),
	SampleDate=rep(NAPL.Data$SampleDate,length(All.Conts.No.NAPL))
	)
	
	New.NAPL.Data$Result=rep("NAPL",nrow(New.NAPL.Data))
	New.NAPL.Data$Units=rep(NAPL.Units,nrow(New.NAPL.Data)); 
	New.NAPL.Data$ND=rep(FALSE,nrow(New.NAPL.Data))
	New.NAPL.Data$Result.Corr.ND<-
	tapply(No.NAPL.Data$Result.Corr.ND,No.NAPL.Data$Constituent,max,na.rm=T)[as.character(New.NAPL.Data$Const)]
	
	
	Cont.Data<-rbind(No.NAPL.Data,New.NAPL.Data)
	All.Conts<-unique(as.character(Cont.Data$Constituent))
	

}else{

	All.Conts.No.NAPL<-All.Conts[tolower(All.Conts)!="napl"]
	NAPL.Data<-Cont.Data[tolower(as.character(Cont.Data$Constituent))=="napl",]
	No.NAPL.Data<-Cont.Data[tolower(as.character(Cont.Data$Constituent))!="napl",]
	No.NAPL.Data$Constituent<-factor(as.character(No.NAPL.Data$Constituent))

	Cont.Data<-No.NAPL.Data
	All.Conts<-unique(as.character(Cont.Data$Constituent))
	
}


}
#---------------------------------------------------------------------------------------------------------#


##################################### ND correction Handling ##############################################

if(GWSDAT_Options$NDMethod=="Half of ND Value"){Cont.Data$Result.Corr.ND[Cont.Data$ND]<-0.5*Cont.Data$Result.Corr.ND[Cont.Data$ND]}
Cont.Data[,c("XCoord","YCoord")]<-Well.Coords[match(as.character(Cont.Data$WellName),as.character(Well.Coords$WellName)),c("XCoord","YCoord")]
#-------------------------------------------------------------------------------------------------------#



####################### Groundwater Data ###############################################################

GW.Data<-AG.ALL[tolower(as.character(AG.ALL$Constituent))=="gw",]
GW.Units<-unique(tolower(as.character(GW.Data$Units)))

if(length(GW.Units)>1){
tkmessageBox(title="Units Error",message="Multiple units detected for GroundWater elevation in input dataset. \nPlease ensure same elevation units are used throughout.",icon="error",type="ok")
stop("Multiple GW Units")
}

if(length(GW.Units)>0){
if(!GW.Units %in% c("level","mm","cm","metres","inches","feet")){
tkmessageBox(title="Units Error",message="GroundWater elevation units must be one of 'level', 'mm', 'cm', 'metres', 'inches' or 'feet'.\n\nPlease correct and re-run GWSDAT analysis.",icon="error",type="ok")
stop("Incorrect GW Units")
}
}

GW.Data$Result<-as.numeric(as.character(GW.Data$Result))
GW.Data<-GW.Data[!is.na(GW.Data$Result),]
GW.Data[,c("XCoord","YCoord")]<-Well.Coords[match(as.character(GW.Data$WellName),as.character(Well.Coords$WellName)),c("XCoord","YCoord")]
#-------------------------------------------------------------------------------------------------------#



####################### Aggregate Cont, GW and NAPL Data ######################################################
All.Dates<-sort(unique(c(GW.Data$SampleDate,AG.ALL$SampleDate)))

if(exists("NAPL.Thickness.Data")){All.Dates<-sort(unique(c(All.Dates,NAPL.Thickness.Data$SampleDate)))}

if(GWSDAT_Options$Aggby=="All Dates")	{my.seq<-NULL}
if(GWSDAT_Options$Aggby=="Monthly")	{my.seq<-as.Date(sort(seq.Date(max(as.Date(All.Dates)),min(as.Date(All.Dates))-500,by="-1 months")))}
if(GWSDAT_Options$Aggby=="Quarterly")	{my.seq<-as.Date(sort(seq.Date(max(as.Date(All.Dates)),min(as.Date(All.Dates))-500,by="-3 month")))}

Cont.Data<-GWSDAT.Create.Agg.Date(Cont.Data,GWSDAT_Options$Aggby,my.seq)
All.Agg.Dates<-as.Date(sort(unique(Cont.Data$AggDate)))



if(nrow(na.omit(GW.Data))>0){

	Agg_GW_Data<-na.omit(GW.Data)
	Agg_GW_Data<-GWSDAT.Create.Agg.Date(Agg_GW_Data,GWSDAT_Options$Aggby,my.seq=my.seq)
	Agg_GW_Data<-GWSDAT.Create.Agg.GW(Agg_GW_Data,Well.Coords,tolower(GWSDAT_Options$AggMethod))
	All.Agg.Dates<-as.Date(sort(unique(c(All.Agg.Dates,unique(Agg_GW_Data$AggDate)))))

}


if(exists("NAPL.Thickness.Data")){ # Just add AggDate column

	NAPL.Thickness.Data<-GWSDAT.Create.Agg.Date(NAPL.Thickness.Data,GWSDAT_Options$Aggby,my.seq=my.seq)
	attr(NAPL.Thickness.Data,"lev.cuts")<-pretty(seq(0,max(NAPL.Thickness.Data$Result.Corr.ND,na.rm=T),l=13),n=12)
	attr(NAPL.Thickness.Data,"NAPL.Wells")<-sort(unique(as.character(NAPL.Thickness.Data$WellName)))

}


#-------------------------------------------------------------------------------------------------------#




All.Data<-list(GW.Data=GW.Data,Agg_GW_Data=if(exists("Agg_GW_Data")){Agg_GW_Data}else{NULL},NAPL.Thickness.Data=if(exists("NAPL.Thickness.Data")){NAPL.Thickness.Data}else{NULL},
Cont.Data=Cont.Data,All.Conts=All.Conts,
All.Dates=All.Dates,All.Agg.Dates=All.Agg.Dates,All.Wells=All.Wells,Well.Coords=Well.Coords,All.Well.Area=Well.Area,ShapeFiles=ShapeFiles,Aq.sel=Aq.sel,GW.Units=GW.Units,
NAPL.Units=if(exists("NAPL.Units")){NAPL.Units}else{NULL},ElecAccepts=ElecAccepts)

return(All.Data)
}
#------------------------------------------------------------------------------------------------------------#

