




############################################### Create the GWSDAT rpanel ###################################################

GWSDAT.Make.Panel<-function(Curr.Site.Data) {



  #### Data Initialisation
  All.Data<-Curr.Site.Data$All.Data
  Fitted.Data<-Curr.Site.Data$Fitted.Data
  Cont.Names<-names(Fitted.Data)
  Num.Conts<-length(Cont.Names)
  GWSDAT_Options<-Curr.Site.Data$GWSDAT_Options


  ####### Local admin Panel Functions for GWSDAT ###################

  redraw <- function(panel) {

        rp.tkrreplot(panel, ImagePlot)
        rp.tkrreplot(panel, SmoothPlot)
        rp.tkrreplot(panel, TrafficLightPlot)
	  #print("panel<<-panel")
	  #panel<<-panel
	  return(panel)

  }

  replot.SmoothPlot<-function(panel){

	  rp.tkrreplot(panel, SmoothPlot)
	  return(panel)
  }


  replot.ImagePlot<-function(panel){

	  rp.tkrreplot(panel, ImagePlot)
	  return(panel)
  }

  replot.Make.Traffic.Plot<-function(panel){

	  rp.tkrreplot(panel, TrafficLightPlot)
	  rp.tkrreplot(panel, SmoothPlot)
	  return(panel)
  }


  listbox.Well.Select<-function(panel){

	  #panel$Well<-panel$Well.Select
	  replot.SmoothPlot(panel)
	  return(panel)
  }

#----------------------------------------------------------------#












  ################Calculate Max Concentration on Plume Boundary#############################



  TunePlumeQuant<-function(panel){


    #### Get Hull data points function ########
    Getchulldatapoints<-function(myhull){
    myhull<-rbind(myhull,myhull[1,,drop=FALSE])
  
  
    Perimeters<-sqrt(apply((apply(myhull,2,diff)^2),1,sum))
    Npts<-250
    Ptsperunitlength=Npts/sum(Perimeters)
    Perimeter.Npts<-round(Ptsperunitlength*Perimeters,0)
    chullseglist<-list()
  
  	for(i in 1:(nrow(myhull)-1)){
  		chullseglist[[i]]<-as.data.frame(approx(myhull[i:(i+1),],n=max(Perimeter.Npts[i],3)))
  	}
  
    return(do.call("rbind",chullseglist))
  }
    
  #----------------------------------------#


  Well.Coords<-panel$All.Data$Well.Coords
  jjj<-panel$jjj
  Cont<-panel$Cont.rg
  Time.Eval<-panel$Fitted.Data[[Cont]]$Time.Eval
  temp.df<-data.frame(Time.Eval=panel$Fitted.Data[[Cont]]$Time.Eval)
  temp.df$MaxConc<-rep(NA,nrow(temp.df))
  temp.df$MaxInteriorConc<-rep(NA,nrow(temp.df))

  model<-panel$Fitted.Data[[Cont]][["Model.tune"]]$best.model




  for(i in 1:length(panel$Fitted.Data[[Cont]]$Time.Eval)){

    temp.time.eval<-panel$Fitted.Data[[Cont]]$Time.Eval[i]
    Good.Wells<-as.character(unique(panel$Fitted.Data[[Cont]]$Cont.Data[as.numeric(panel$Fitted.Data[[Cont]]$Cont.Data$AggDate)<=temp.time.eval,]$WellName))
    Good.Wells<-intersect(Good.Wells,as.character(unique(panel$Fitted.Data[[Cont]]$Cont.Data[as.numeric(panel$Fitted.Data[[Cont]]$Cont.Data$AggDate)>=temp.time.eval,]$WellName)))
    
    
    if(length(Good.Wells)>2){
    
    	### Calculate Max Conc on hull boundary
    	my.area<-as.matrix(Well.Coords[as.character(Well.Coords$WellName) %in% as.character(Good.Wells),c("XCoord","YCoord")])
    	myhull<-my.area[chull(my.area),]
    	hulldatapoints<-Getchulldatapoints(myhull)
    	my.df<-data.frame(XCoord=hulldatapoints$x,YCoord=hulldatapoints$y,AggDate=temp.time.eval)
    	temp.df$MaxConc[i]=max(exp(predict(model,newdata=my.df)$predicted))
    
    	### Calculate Max Conc on interior points of hull. 
    	InteriorPoints<-gridpts(myhull,200)
    	my.df<-data.frame(XCoord=InteriorPoints[,1],YCoord=InteriorPoints[,2],AggDate=temp.time.eval)
    	temp.df$MaxInteriorConc[i]=max(exp(predict(model,newdata=my.df)$predicted))
    	
    }
  }


  temp.df$MaxInteriorConc[temp.df$MaxInteriorConc<temp.df$MaxConc]<- temp.df$MaxConc[temp.df$MaxInteriorConc<temp.df$MaxConc]

  my.ylim=c(min(temp.df[,c("MaxInteriorConc","MaxConc")],na.rm=T),max(temp.df[,c("MaxInteriorConc","MaxConc")],na.rm=T))
  graphics.off();
  windows(record=T,width =11, height = 9)

  plot(MaxInteriorConc~Time.Eval,data=temp.df,log = "y",type="b",ylim=my.ylim,xlab="Date",ylab="Concentration(ug/l)",main=paste("Estimated Plume Delineation Concentration Region for",Cont,"at",panel$GWSDAT_Options$SiteName,""), pch=19,cex.main=.85)
  lines(MaxConc~Time.Eval,data=temp.df,type="b",pch=19,col="black")

  try(polygon(c(temp.df$Time.Eval, rev(temp.df$Time.Eval)), c(temp.df$MaxInteriorConc, rev(temp.df$MaxConc)),col = "grey", border = NA))
  try(grid(NA,NULL,lwd = 1,lty=1,equilogs = FALSE))
  try(abline(h=as.numeric(panel$PlumeLim[panel$Cont.rg]),col="red",lwd=2,lty=2))
  try(bringToTop())

  return(panel)

}

#----------------------------------------------------------------#







#-------------------------------------- GWSDAT Spatial Plot Function ----------------------------------------------------#





#### GW ContourInterpolation Function
"GWSDAT.GW.Contour"<-function(temp.GW.Flows){

  options(warn=-1)
  my.lo<-try(loess(Result~XCoord+YCoord,temp.GW.Flows,span=1,degree=if(nrow(temp.GW.Flows)<20){1}else{2},control = loess.control(surface = c("interpolate", "direct")[1])),silent=T)
  if(inherits(my.lo, "try-error")){options(warn=0); stop("Unable to fit loess")}
  options(warn=0)
  xo=seq(min(temp.GW.Flows$XCoord),max(temp.GW.Flows$XCoord),l=40)
  yo=seq(min(temp.GW.Flows$YCoord),max(temp.GW.Flows$YCoord),l=40)
  my.df<-expand.grid(XCoord=xo,YCoord=yo)
  
  lo.pred<-predict(my.lo,my.df)
  my.hull<-temp.GW.Flows[chull(temp.GW.Flows[,c("XCoord","YCoord")]),c("XCoord","YCoord")]
  temp.pip<-point.in.polygon(my.df$XCoord,my.df$YCoord,my.hull$XCoor,my.hull$YCoor)==0
  lo.pred[matrix(temp.pip,nrow=length(xo))]<-NA
  return(list(x=xo,y=yo,z=lo.pred))

}



click.MakeSmoothPlotNoPlot<-function(panel,x,y){

  graphics.off();
  windows(record=T,width =11, height = 9)
  MakeImagePlotNoPlot(panel)
  try(bringToTop())
  return(panel)

}


replot.MakeImagePlotNoPlot<-function(panel){

  rp.tkrreplot(panel, ImagePlot)
  return(panel)

}




MakeImagePlotNoPlot<-function(panel,fromDoubleButton=T){


if(is.null(panel$PredInterval)){panel$PredInterval="Predicted"}

panel$PlumeLimEntry<-PlumeLimEntry #fix for R-3.0.0 tkrplot bug
panel$Porosity<-Porosity #fix for R-3.0.0 tkrplot bug


if(fromDoubleButton){
  my.jjj<-panel$shadow.jjj%%length(panel$All.Data$All.Agg.Dates)
  if(my.jjj==0){my.jjj=length(panel$All.Data$All.Agg.Dates)}
  panel$jjj=my.jjj

}


if(panel$ScaleCols["Plume Diagnostics?"]){
	op<-par(mar=c(3,4.1,2,2.1))
}else{
	op<-par(mar=c(2,4.1,2,2.1))
}

gc()
Col.Option<-panel$ScaleCols["Scale colours to Data?"]
Show.Values<-panel$ScaleCols["Show Conc. Values?"]
Show.GW.Contour<-panel$ScaleCols["Show GW Contour?"]

if(is.null(Show.GW.Contour) || length(Show.GW.Contour)==0 || is.na(Show.GW.Contour)){Show.GW.Contour<-FALSE}

Show.Well.Labels<-panel$ScaleCols["Show Well Labels?"]
Show.ShapeFile<-panel$ScaleCols["Overlay ShapeFiles?"]
if(is.null(Show.ShapeFile) || length(Show.ShapeFile)==0 || is.na(Show.ShapeFile)){Show.ShapeFile<-FALSE}

Well.Coords<-panel$All.Data$Well.Coords
jjj<-panel$jjj
Cont<-panel$Cont.rg


temp.time.eval<-panel$Fitted.Data[[Cont]]$Time.Eval[jjj]
temp.time.frac<-as.numeric(temp.time.eval-min(panel$Fitted.Data[[Cont]]$Time.Eval))/as.numeric(diff(range(panel$Fitted.Data[[Cont]]$Time.Eval)))

try(if(temp.time.frac==1){temp.time.frac=.999}) # to avoid plot issue with wmf format!
try(if(temp.time.frac==0){temp.time.frac=.001})
try(if(as.numeric(diff(range(panel$Fitted.Data[[Cont]]$Time.Eval)))==0 || is.nan(temp.time.frac)){temp.time.frac=.999}) # Handle case when only one time point.



############# Date Interval Printing #########################################################

date.to.print <-  format(as.Date(panel$Fitted.Data[[1]]$Time.Eval[jjj]),"%d-%b-%Y")

if(panel$GWSDAT_Options$Aggby %in% c("Monthly","Quarterly")){

if(panel$GWSDAT_Options$Aggby=="Monthly"){

	date.range.to.print<-seq.Date(as.Date(as.Date(panel$Fitted.Data[[1]]$Time.Eval[jjj])),by="-1 month",length=2)

}else{

	date.range.to.print<-seq.Date(as.Date(as.Date(panel$Fitted.Data[[1]]$Time.Eval[jjj])),by="-3 month",length=2)

}	

	date.range.to.print[2]<- date.range.to.print[2]+1
	date.range.to.print<-format(date.range.to.print,"%d-%b-%Y")[c(2,1)]
	date.to.print<-paste(date.range.to.print,collapse=" to ")

}
#---------------------------------------------------------------------------------------------#




model.tune<-panel$Fitted.Data[[Cont]][["Model.tune"]]
temp.Cont.Data<-panel$Fitted.Data[[panel$Cont.rg]]$Cont.Data
temp.Cont.Data<-temp.Cont.Data[temp.Cont.Data$AggDate==temp.time.eval,]
temp.Cont.Data$log.Resid<-log(temp.Cont.Data$Result.Corr.ND)-log(temp.Cont.Data$ModelPred)

if(panel$rgUnits=="mg/l"){

	temp.Cont.Data$Result.Corr.ND<-temp.Cont.Data$Result.Corr.ND/1000
	temp.res<-as.character(temp.Cont.Data$Result)
	temp.res<-gsub("ND<","",temp.res)
	temp.res[tolower(temp.res)!="napl"]<-as.character(as.numeric(temp.res[tolower(temp.res)!="napl"])/1000)
	temp.res[temp.Cont.Data$ND]<-paste("ND<",temp.res[temp.Cont.Data$ND],sep="")
	temp.Cont.Data$Result<-temp.res
	rm(temp.res)
}
if(panel$rgUnits=="ng/l"){

	temp.Cont.Data$Result.Corr.ND<-temp.Cont.Data$Result.Corr.ND*1000
	temp.res<-as.character(temp.Cont.Data$Result)
	temp.res<-gsub("ND<","",temp.res)
	temp.res[tolower(temp.res)!="napl"]<-as.character(as.numeric(temp.res[tolower(temp.res)!="napl"])*1000)
	temp.res[temp.Cont.Data$ND]<-paste("ND<",temp.res[temp.Cont.Data$ND],sep="")
	temp.Cont.Data$Result<-temp.res
	rm(temp.res)
}



Bad.Wells<-as.character(temp.Cont.Data$WellName[which(temp.Cont.Data$log.Resid>1.75)])
Bad.Wells<- Well.Coords[Well.Coords$WellName %in% Bad.Wells,]
if(nrow(Bad.Wells)>0){Bad.Wells$WellName<-paste("<",Bad.Wells$WellName,">",sep="")}


diffrangeX<-0.06*(range(Well.Coords$XCoord)[2]-range(Well.Coords$XCoord)[1])
diffrangeY<-0.06*(range(Well.Coords$YCoord)[2]-range(Well.Coords$YCoord)[1])

if((diffrangeX/diffrangeY)>1.4){diffrangeY=0}
if((diffrangeY/diffrangeX)>1.4){diffrangeX=0}
Contour.xlim=c(range(Well.Coords$XCoord)[1]-diffrangeX,range(Well.Coords$XCoord)[2]+diffrangeX)
Contour.ylim=c(range(Well.Coords$YCoord)[1]-diffrangeY,range(Well.Coords$YCoord)[2]+diffrangeY)



if(panel$PredInterval!="% sd"){

lev.cut<-panel$lev.cut
if(panel$rgUnits=="mg/l"){lev.cut<-lev.cut/10}
if(panel$rgUnits=="ng/l"){lev.cut<-lev.cut*10}

}else{

lev.cut<-panel$sd.lev.cut

}
n.col <- length(lev.cut)-1 #should be n.col-1



Good.Wells<-as.character(unique(panel$Fitted.Data[[Cont]]$Cont.Data[as.numeric(panel$Fitted.Data[[Cont]]$Cont.Data$AggDate)<=temp.time.eval,]$WellName))
Good.Wells<-intersect(Good.Wells,as.character(unique(panel$Fitted.Data[[Cont]]$Cont.Data[as.numeric(panel$Fitted.Data[[Cont]]$Cont.Data$AggDate)>=temp.time.eval,]$WellName)))

Do.Image=TRUE

if(length(Good.Wells)<3){

	Do.Image=FALSE;
	my.area<-as.matrix(Well.Coords[,c("XCoord","YCoord")])
}else{

	my.area<-as.matrix(Well.Coords[as.character(Well.Coords$WellName) %in% as.character(Good.Wells),c("XCoord","YCoord")])
	
}

if((areapl(my.area[chull(my.area),])/panel$All.Data$All.Well.Area)<0.01){
Do.Image=FALSE
my.area<-as.matrix(Well.Coords[,c("XCoord","YCoord")])
}



if(!Do.Image){ # If total number of wells is less than 3

	my.area<-cbind(
	c(Contour.xlim[1],Contour.xlim[1],Contour.xlim[2],Contour.xlim[2]),
	c(Contour.ylim[1],Contour.ylim[2],Contour.ylim[1],Contour.ylim[2])
	)

	colnames(my.area)<-c("XCoord","YCoord")

}

#############################################
expandpoly<-function (mypol, fact) {

    m1 <- mean(mypol[, 1])
    m2 <- mean(mypol[, 2])
    cbind((mypol[, 1] - m1) * fact + m1, (mypol[, 2] - m2) * fact + m2)

}
#-------------------------------------------#

#############################################


my.area<-my.area[chull(my.area),,drop=F]
my.exp.area<-expandpoly(my.area,fac=1.05)
eval.df<-gridpts(my.exp.area,350)
eval.df<-rbind(eval.df,my.exp.area)
colnames(eval.df)[1:2]<-c("XCoord","YCoord")
try(rownames(eval.df)<-NULL)
eval.df<-as.data.frame(eval.df)
eval.df$AggDate=rep(temp.time.eval,nrow(eval.df))


if(!inherits(model.tune,"try-error")){

	#interp.pred<-GWSDAT.Interp(model.tune$best.mod,AggDate=eval.df$AggDate[1],eval.df,type=if(is.null(panel$PredInterval)){"predict"}else{as.character(panel$PredInterval)})
	interp.pred<-try(GWSDAT.Bary.Interp(model.tune$best.mod,AggDate=eval.df$AggDate[1],my.area=my.area,type=as.character(panel$PredInterval)))
	if(inherits(interp.pred,"try-error")){interp.pred<-GWSDAT.Interp(NULL,AggDate=eval.df$AggDate[1],eval.df)}
	

}else{

	interp.pred<-GWSDAT.Interp(NULL,AggDate=eval.df$AggDate[1],eval.df)
	Do.Image<-FALSE

}

if(Do.Image){

if(panel$PredInterval %in% c("Lower 95% CI","Predicted","Upper 95% CI","IQR/2")){

if(panel$PredInterval!="IQR/2"){interp.pred$z<-exp(interp.pred$z)}

if(panel$rgUnits=="mg/l"){interp.pred$z<-interp.pred$z/1000}
if(panel$rgUnits=="ng/l"){interp.pred$z<-interp.pred$z*1000}

}

if(max(interp.pred$z,na.rm=T)>lev.cut[length(lev.cut)] && !Col.Option){

	interp.pred$z[which(interp.pred$z>lev.cut[length(lev.cut)],arr.ind=T)]<-lev.cut[length(lev.cut)]
}

}else{

interp.pred$z[,]<-NA


}
#----------------------------------------------------------------------------#








####################### Plume Quantification #################################

if(panel$ScaleCols["Plume Diagnostics?"]){


checkPlumeClosure<-function(cl){

cl$x[1]==cl$x[length(cl$x)] & cl$y[1]==cl$y[length(cl$y)]

}

VolIndTri<-function(l){

x<-l$x
y<-l$y
z<-l$z
 
0.5*(x[1]*(y[2] - y[3]) + x[2]*(y[3]- y[1]) + x[3]*(y[1]- y[2]))*(z[1] + z[2] + z[3]) / 3
}

xVolIndTri<-function(l){

x<-l$x
y<-l$y
z<-l$z
 
z=z*x

0.5*(x[1]*(y[2] - y[3]) + x[2]*(y[3]- y[1]) + x[3]*(y[1]- y[2]))*(z[1] + z[2] + z[3]) / 3
}


yVolIndTri<-function(l){

x<-l$x
y<-l$y
z<-l$z
 
z=z*y

0.5*(x[1]*(y[2] - y[3]) + x[2]*(y[3]- y[1]) + x[3]*(y[1]- y[2]))*(z[1] + z[2] + z[3]) / 3
}











CalcPlumeStats<-function(model,AggDate,cL,PLumeCutoff,type,units){

	temppts<-cbind(cL$x,cL$y)
	temppts<-gridpts(temppts,100)
	Plume.Tri.Points<-data.frame(XCoord=temppts[,1],YCoord=temppts[,2])
	Plume.Tri.Points$AggDate=as.numeric(AggDate)
	
	
	temppred<-predict(model,newdata=Plume.Tri.Points,se=type!="Predicted")
	if(type=="Lower 95% CI"){temppred$predicted<-temppred$predicted-temppred$predicted.sd*1.96}
	if(type=="Upper 95% CI"){temppred$predicted<-temppred$predicted+temppred$predicted.sd*1.96}
	Plume.Tri.Points$z<-exp(temppred$predicted)
	
	
	if(units=="mg/l"){Plume.Tri.Points$z<-Plume.Tri.Points$z/1000}
	if(units=="ng/l"){Plume.Tri.Points$z<-Plume.Tri.Points$z*1000}



	cL.Tri.Points<-data.frame(XCoord=cL$x,YCoord=cL$y,z=rep(PLumeCutoff,length(cL$x)))
	Vol.Tri.Points<-unique(rbind(Plume.Tri.Points[,c("XCoord","YCoord","z")],cL.Tri.Points))

	mydeldir<-deldir(x=Vol.Tri.Points$XCoord,y=Vol.Tri.Points$YCoord,z=Vol.Tri.Points$z)
	mytriangs<-triang.list(mydeldir)
	PlumeVol<-sum(unlist(lapply(mytriangs,VolIndTri)))
	xPlumeVol<-sum(unlist(lapply(mytriangs,xVolIndTri)))
	yPlumeVol<-sum(unlist(lapply(mytriangs,yVolIndTri)))
	PlumeCentreofMass<-c(xPlumeVol,yPlumeVol)/PlumeVol


return(list(PlumeVol=PlumeVol,PlumeCentreofMass=PlumeCentreofMass))

}



PlumeDetails=list()



PLumeCutoff<-as.numeric(panel$PlumeLimEntry[match(panel$Cont.rg,panel$Cont.Names)])  #Defined in ug/L
if(panel$rgUnits=="mg/l"){PLumeCutoff<-PLumeCutoff/1000}
if(panel$rgUnits=="ng/l"){PLumeCutoff<-PLumeCutoff*1000}
cL<-contourLines(interp.pred,levels=PLumeCutoff)



if(length(cL)>0){

	for(i in 1:length(cL)){
	
	cL[[i]]$Closed<-checkPlumeClosure(cL[[i]])

		if(cL[[i]]$Closed){

			cL[[i]]$area<-areapl(cbind(cL[[i]]$x,cL[[i]]$y))
			tempPlumeQuant<-CalcPlumeStats(model.tune$best.mod,AggDate=temp.time.eval,cL[[i]],PLumeCutoff=PLumeCutoff,type=as.character(panel$PredInterval),units=panel$rgUnits)
			cL[[i]]$Volume<-tempPlumeQuant$PlumeVol
			cL[[i]]$PlumeCentreofMass<-tempPlumeQuant$PlumeCentreofMass

		}else{

			cL[[i]]$area<-NA
			cL[[i]]$Volume<-NA
			cL[[i]]$PlumeCentreofMass<-NA

		}
	
	}

}


PlumeDetails$PLumeCutoff=PLumeCutoff
if(length(cL)>0){PlumeDetails$cL=cL}else{PlumeDetails$cL=NULL}

TotalPlume<-list()

if(length(PlumeDetails$cL)>0){

TotalPlume$area<-sum(unlist(lapply(PlumeDetails$cL,function(l){l$area})))
TotalPlume$volume<-sum(unlist(lapply(PlumeDetails$cL,function(l){l$Volume})))
TotalPlume$Mass<-TotalPlume$volume*as.numeric(panel$Porosity)

COMWeights<-unlist(lapply(PlumeDetails$cL,function(l){l$Volume}))/sum(unlist(lapply(PlumeDetails$cL,function(l){l$Volume})))
TotalPlume$PlumeCentreofMass<-rep(NA,2)
TotalPlume$PlumeCentreofMass[1]<-sum(COMWeights*unlist(lapply(PlumeDetails$cL,function(l){l$PlumeCentreofMass[1]})))
TotalPlume$PlumeCentreofMass[2]<-sum(COMWeights*unlist(lapply(PlumeDetails$cL,function(l){l$PlumeCentreofMass[2]})))


}else{

TotalPlume$area<-NA
TotalPlume$volume<-NA
TotalPlume$Mass<-NA
TotalPlume$PlumeCentreofMass<-c(NA,NA)

}

panel$tempTotalPlumeDetails<-TotalPlume


}

#----------------------------------------------------------------------------#




















############################## Plot GW Flows #######################################

GWFlows<-attr(panel$Fitted.Data,"GWFlows")
if(!inherits(GWFlows, "try-error")){


temp.GW.Flows<-GWFlows[as.numeric(GWFlows$AggDate)==temp.time.eval,]

if(!is.null(panel$GW.disp) && panel$GW.disp!="None"){

L<-0.05*sqrt(diff(Contour.xlim)^2+diff(Contour.ylim)^2)


GWFlows<-attr(panel$Fitted.Data,"GWFlows")


	if(!is.null(GWFlows)){

	
		if(nrow(temp.GW.Flows)>0){


			x0=temp.GW.Flows$XCoord
			y0=temp.GW.Flows$YCoord

			if(panel$GW.disp!="Same Length"){

				x1=temp.GW.Flows$XCoord+L*temp.GW.Flows$R*cos(temp.GW.Flows$RAD)
				y1=temp.GW.Flows$YCoord+L*temp.GW.Flows$R*sin(temp.GW.Flows$RAD)

			}
			else{

				x1=temp.GW.Flows$XCoord+.65*L*cos(temp.GW.Flows$RAD)
				y1=temp.GW.Flows$YCoord+.65*L*sin(temp.GW.Flows$RAD)

			}

			
			

		  }else{
		
			Show.GW.Contour<-FALSE
			
		}


	}
}

}#if not try error!
#----------------------------------------------------------------------------#



GWSDAT.GrayScale<-function(n){

rev(grey(seq(0,1,length=n+3)))[c(-1,-2,-(n+3))]

}

GWSDAT.Terrain<-function(n){

terrain.colors(n+1)[1:n] # to avoid white in extreme conc.
}


if(panel$Color.type=="Conc-Terrain" & Col.Option){
col.palette <- terrain.colors
}
if(panel$Color.type=="Conc-Terrain" & !Col.Option){
col.palette <- GWSDAT.Terrain(n.col)
}
if(panel$Color.type=="Conc-Topo" & Col.Option){
col.palette <- topo.colors
}
if(panel$Color.type=="Conc-Topo" & !Col.Option){
col.palette <- topo.colors(n.col)
}
if(panel$Color.type=="Conc-GreyScale" & Col.Option){
col.palette <- GWSDAT.GrayScale
}
if(panel$Color.type=="Conc-GreyScale" & !Col.Option){
col.palette <- GWSDAT.GrayScale(n.col)
}




if(panel$Color.type=="Conc-Terrain-Circles"){

	col.palette <- GWSDAT.Terrain(n.col)
	Do.Image<-FALSE
	interp.pred$z[,]<-NA
	my.palette<-col.palette[(as.numeric(cut(temp.Cont.Data$Result.Corr.ND,breaks=lev.cut)))]
	#my.cex<-.5*as.numeric(cut(temp.Cont.Data$Result.Corr.ND,breaks=my.lev.cut))
	my.cex<-.8*log(temp.Cont.Data$Result.Corr.ND)
	if(panel$rgUnits=="mg/l"){my.cex<-.5*log(1000*temp.Cont.Data$Result.Corr.ND)}
	if(panel$rgUnits=="ng/l"){my.cex<-.5*log(0.001*temp.Cont.Data$Result.Corr.ND)}


	my.cex[my.cex<1.5]<-1.5

}


if(panel$Color.type=="Conc-Topo-Circles"){

	col.palette <- topo.colors(n.col)
	Do.Image<-FALSE
	interp.pred$z[,]<-NA
	my.palette<-col.palette[(as.numeric(cut(temp.Cont.Data$Result.Corr.ND,breaks=my.lev.cut)))]
	#my.cex<-.5*as.numeric(cut(temp.Cont.Data$Result.Corr.ND,breaks=my.lev.cut))
	#my.cex<-if(panel$rgUnits=="mg/l"){.5*log(1000*temp.Cont.Data$Result.Corr.ND)}else{.8*log(temp.Cont.Data$Result.Corr.ND)}

	my.cex<-.8*log(temp.Cont.Data$Result.Corr.ND)
	if(panel$rgUnits=="mg/l"){my.cex<-.5*log(1000*temp.Cont.Data$Result.Corr.ND)}
	if(panel$rgUnits=="ng/l"){my.cex<-.5*log(0.001*temp.Cont.Data$Result.Corr.ND)}


	my.cex[my.cex<1.5]<-1.5

}

if(panel$Color.type=="Conc-GreyScale-Circles"){

	col.palette <- GWSDAT.GrayScale(n.col)
	Do.Image<-FALSE
	interp.pred$z[,]<-NA
	my.palette<-col.palette[(as.numeric(cut(temp.Cont.Data$Result.Corr.ND,breaks=lev.cut)))]
	#my.cex<-.5*as.numeric(cut(temp.Cont.Data$Result.Corr.ND,breaks=my.lev.cut))
	#my.cex<-if(panel$rgUnits=="mg/l"){.5*log(1000*temp.Cont.Data$Result.Corr.ND)}else{.8*log(temp.Cont.Data$Result.Corr.ND)}

	my.cex<-.8*log(temp.Cont.Data$Result.Corr.ND)
	if(panel$rgUnits=="mg/l"){my.cex<-.5*log(1000*temp.Cont.Data$Result.Corr.ND)}
	if(panel$rgUnits=="ng/l"){my.cex<-.5*log(0.001*temp.Cont.Data$Result.Corr.ND)}

	my.cex[my.cex<1.5]<-1.5

}

if(panel$Color.type=="NAPL-Circles"){

	Do.Image<-FALSE
	interp.pred$z[,]<-NA
	NAPL.Thickness.Data<-panel$All.Data$NAPL.Thickness.Data
	temp.NAPL.Data<-NAPL.Thickness.Data[NAPL.Thickness.Data$AggDate==temp.time.eval,]

	lev.cut<-attributes(NAPL.Thickness.Data)$lev.cuts
	NAPL.Wells<-attributes(NAPL.Thickness.Data)$NAPL.Wells
	col.palette <- heat.colors(length(lev.cut)-1)


	my.palette<-col.palette[(as.numeric(cut(temp.NAPL.Data$Result.Corr.ND,breaks=attributes(NAPL.Thickness.Data)$lev.cuts)))]
	my.palette<-col.palette[(as.numeric(cut(temp.NAPL.Data$Result.Corr.ND,breaks=attributes(NAPL.Thickness.Data)$lev.cuts)))]
	my.cex<-1.5+(as.numeric(cut(temp.NAPL.Data$Result.Corr.ND,breaks=attributes(NAPL.Thickness.Data)$lev.cuts)))/2
	
}


if(!Col.Option || !Do.Image){

  
	GWSDAT.filled.contour(interp.pred,asp=1,ShapeFiles=if(Show.ShapeFile){panel$All.Data$ShapeFiles}else{NULL},fixedConcScale=if(panel$Color.type=="NAPL-Circles"){FALSE}else{TRUE},
	xlim=Contour.xlim,
	ylim=Contour.ylim,
	levels=lev.cut,col=col.palette,
	plot.title = title(main = paste(Cont,if(panel$Color.type=="NAPL-Circles" & Cont!=" "){paste("(",panel$rgUnits,")",sep="")}else{""},if(Cont!=" "){":"}else{""},date.to.print,if(panel$All.Data$Aq.sel!=""){paste(": Aquifer-",panel$All.Data$Aq.sel,sep="")}else{""}),xlab = "", ylab = "",cex.main=.95),key.title = if(panel$Color.type=="NAPL-Circles"){title(main=paste("NAPL \nThickness \n(",panel$All.Data$NAPL.Units,")",sep=""),cex.main=0.7)}else{title(main=panel$rgUnits)},

		plot.axes={ axis(1); axis(2,las=3); axis(3,at=par("usr")[1]+temp.time.frac*(diff(range(par("usr")[1:2]))),labels="",col="red",lwd=3,tck=-0.02); 
		if(panel$Color.type=="Conc-Terrain-Circles" || panel$Color.type=="Conc-Topo-Circles" || panel$Color.type=="Conc-GreyScale-Circles"){points(temp.Cont.Data$XCoord[order(my.cex,decreasing=T)],temp.Cont.Data$YCoord[order(my.cex,decreasing=T)],pch=19,col=my.palette[order(my.cex,decreasing=T)],cex=my.cex[order(my.cex,decreasing=T)])}
		if(panel$Color.type=="Conc-Terrain-Circles" || panel$Color.type=="Conc-Topo-Circles" || panel$Color.type=="Conc-GreyScale-Circles"){points(temp.Cont.Data$XCoord[order(my.cex,decreasing=T)],temp.Cont.Data$YCoord[order(my.cex,decreasing=T)],pch=1,col=1,cex=my.cex[order(my.cex,decreasing=T)])}
		
		###### NAPL Circles Plot ############### 
		if(panel$Color.type=="NAPL-Circles"){points(temp.NAPL.Data$XCoord[order(my.cex,decreasing=T)],temp.NAPL.Data$YCoord[order(my.cex,decreasing=T)],pch=19,col=my.palette[order(my.cex,decreasing=T)],cex=my.cex[order(my.cex,decreasing=T)])}
		if(panel$Color.type=="NAPL-Circles"){points(temp.NAPL.Data$XCoord,temp.NAPL.Data$YCoord,pch=1,col=1,cex=my.cex)}
		
		#--------------------------------------#


		points(Well.Coords$XCoord,Well.Coords$YCoord,pch=19,cex=.7);
		if(panel$Color.type=="NAPL-Circles"){points(Well.Coords[as.character(Well.Coords$WellName) %in% attributes(NAPL.Thickness.Data)$NAPL.Wells,c("XCoord","YCoord")],col="red",pch=19,cex=0.7)}
		
		if(Show.Well.Labels)text(Well.Coords$XCoord,Well.Coords$YCoord,Well.Coords$WellName,cex=0.75,pos=1)
		if(Show.GW.Contour)try(contour(GWSDAT.GW.Contour(temp.GW.Flows),add=T,labcex=.8),silent=T)
		if(Show.Values & length(as.character(temp.Cont.Data$Result))>0)try(text(temp.Cont.Data$XCoord,temp.Cont.Data$YCoord,as.character(temp.Cont.Data$Result),
		cex=0.75,col=c("red","black")[as.numeric(temp.Cont.Data$ND)+1],pos=3),silent=T)
		
		###### Plume Details Plot ############### 		
		if(exists("PlumeDetails") & panel$ScaleCols["Plume Diagnostics?"]){

		try(contour(interp.pred,levels=PlumeDetails$PLumeCutoff,add=T,col="red",lwd=2,labcex =.8))
		
		#for(i in 1:length(PlumeDetails$cL)){try(points(PlumeDetails$cL[[i]]$PlumeCentreofMass[1],PlumeDetails$cL[[i]]$PlumeCentreofMass[2],cex=1.3,pch=3,lwd=2,col="red"))}	
		try(points(TotalPlume$PlumeCentreofMass[1],TotalPlume$PlumeCentreofMass[2],cex=1.3,pch=3,lwd=2,col="red"))
		}
		#--------------------------------------#

		
		if(nrow(Bad.Wells)>0 & Show.Well.Labels & Do.Image){text(Bad.Wells$XCoord,Bad.Wells$YCoord,Bad.Wells$WellName,cex=0.75,col="red",pos=1)}
		try(arrows(x0, y0, x1, y1, length = 0.1,lwd=2,col="blue"), silent = TRUE)
		
		}

	)
	
	if(panel$ScaleCols["Plume Diagnostics?"]){
	tempUnitHandle<-PlumeUnitHandlingFunc(GWSDAT_Options$WellCoordsLengthUnits,panel$rgUnits,TotalPlume$Mass[1],TotalPlume$area[1])
	#tp<-paste("Estimated Plume Mass=",round(TotalPlume$Mass,2),";   Estimated Plume Area=",round(TotalPlume$area,2),sep="")
	tp<-paste("Plume Mass=",signif(tempUnitHandle$PlumeMass,5),tempUnitHandle$PlumeMassUnits,";  Plume Area=",signif(tempUnitHandle$PlumeArea,5),tempUnitHandle$PlumeAreaUnits,sep="")
	mtext(tp,side=1,adj=-0.1,line = 2,cex=0.85)

	}


}else{


GWSDAT.filled.contour(interp.pred,asp=1,ShapeFiles=if(Show.ShapeFile){panel$All.Data$ShapeFiles}else{NULL},

	xlim=Contour.xlim,
	ylim=Contour.ylim,
	color.palette=col.palette,
	plot.title = title(main = paste(Cont,":",date.to.print,if(panel$All.Data$Aq.sel!=""){paste(": Aquifer-",panel$All.Data$Aq.sel,sep="")}else{""}),xlab = "", ylab = "",cex.main=.95),key.title = title(main=panel$rgUnits),
	plot.axes={ axis(1); axis(2,las=3);axis(3,at=par("usr")[1]+temp.time.frac*(diff(range(par("usr")[1:2]))),labels="",col="red",lwd=3,tck=-0.02);  
	points(Well.Coords$XCoord,Well.Coords$YCoord,pch=19,cex=1.0);
	if(Show.Well.Labels)text(Well.Coords$XCoord,Well.Coords$YCoord,Well.Coords$WellName,cex=0.75,pos=1)
	if(Show.GW.Contour)try(contour(GWSDAT.GW.Contour(temp.GW.Flows),add=T,labcex=.8),silent=T)
	if(Show.Values & length(as.character(temp.Cont.Data$Result))>0)try(text(temp.Cont.Data$XCoord,temp.Cont.Data$YCoord,as.character(temp.Cont.Data$Result),
	cex=0.75,col=c("red","black")[as.numeric(temp.Cont.Data$ND)+1],pos=3),silent=T)

		###### Plume Details Plot ############### 		
		if(exists("PlumeDetails") & panel$ScaleCols["Plume Diagnostics?"]){

		contour(interp.pred,levels=PlumeDetails$PLumeCutoff,add=T,col="red",lwd=2,labcex =.8)
		try(contour(interp.pred,levels=PlumeDetails$PLumeCutoff,add=T,col="red",lwd=2,labcex =.8))
		#for(i in 1:length(PlumeDetails$cL)){try(points(PlumeDetails$cL[[i]]$PlumeCentreofMass[1],PlumeDetails$cL[[i]]$PlumeCentreofMass[2],cex=1.3,pch=3,lwd=2,col="red"))}	
		try(points(TotalPlume$PlumeCentreofMass[1],TotalPlume$PlumeCentreofMass[2],cex=1.3,pch=3,lwd=2,col="red"))
		}
		#--------------------------------------#

	if(nrow(Bad.Wells)>0 & Show.Well.Labels & Do.Image){text(Bad.Wells$XCoord,Bad.Wells$YCoord,Bad.Wells$WellName,cex=0.75,col="red",pos=1)}
	try(arrows(x0, y0, x1, y1, length = 0.1,lwd=2,col="blue"), silent = TRUE)

	}

	)

	if(panel$ScaleCols["Plume Diagnostics?"]){
	
	tempUnitHandle<-PlumeUnitHandlingFunc(GWSDAT_Options$WellCoordsLengthUnits,panel$rgUnits,TotalPlume$Mass[1],TotalPlume$area[1])
	#tp<-paste("Estimated Plume Mass=",round(TotalPlume$Mass,2),";   Estimated Plume Area=",round(TotalPlume$area,2),sep="")
	tp<-paste("Plume Mass=",signif(tempUnitHandle$PlumeMass,5),tempUnitHandle$PlumeMassUnits,";  Plume Area=",signif(tempUnitHandle$PlumeArea,5),tempUnitHandle$PlumeAreaUnits,sep="")
	mtext(tp,side=1,adj=-0.1,line = 2,cex=0.85)

	}


}


par(op)
return(panel)


}
#-------------------------------------- End GWSDAT Spatial Plot Function------------------------------------------------------#



































































#################################### Smooth Time Series Plot functions ########################################################

click.MakeSmoothPlot<-function(panel,x,y){
  
	graphics.off();
	windows(record=T,width =11, height = 9)
	MakeSmoothPlot(panel,showvline=FALSE)
	try(bringToTop())
	return(panel)

}





MakeSmoothPlot<-function(panel,showvline=TRUE,fromDoubleButton=T){
#browser()
panel$ContLimEntry<-ContLimEntry #fix for R-3.0.0 tkrplot bug

if(fromDoubleButton){
my.jjj<-panel$shadow.jjj%%length(panel$All.Data$All.Agg.Dates)
if(my.jjj==0){my.jjj=length(panel$All.Data$All.Agg.Dates)}
panel$jjj=my.jjj

}

Use.LogScale=panel$dlines["Log Conc. Scale"]

Well.Data<-panel$Cont.Data[as.character(panel$Cont.Data$WellName)==panel$Well & panel$Cont.Data$Constituent==panel$Cont.rg,]


if(panel$rgUnits=="mg/l"){Well.Data$Result.Corr.ND<-Well.Data$Result.Corr.ND/1000}
if(panel$rgUnits=="ng/l"){Well.Data$Result.Corr.ND<-Well.Data$Result.Corr.ND*1000}

Num.Data.Pts<-nrow(Well.Data)
smThreshSe<-panel$GWSDAT_Options$smThreshSe
Det.Pts<-Well.Data$ND==FALSE
ND.Pts<-Well.Data$ND==TRUE
NAPL.Present<-any("napl" %in% tolower(as.character(Well.Data$Result))) ||   nrow(panel$All.Data$NAPL.Thickness.Data[as.character(panel$All.Data$NAPL.Thickness.Data$WellName)==panel$Well,])>0
if(is.na( NAPL.Present)){NAPL.Present<-FALSE}


Stat.Lim<-as.numeric(panel$ContLimEntry[match(panel$Cont.rg,panel$Cont.Names)])
if(panel$rgUnits=="mg/l"){Stat.Lim=Stat.Lim/1000}
if(panel$rgUnits=="ng/l"){Stat.Lim=Stat.Lim*1000}
if(panel$rg1=="Trend"){Stat.Lim=NA}



GWAxis<-panel$dlines["Overlay GW levels"] && "GWFlows" %in% names(attributes(panel$Fitted.Data)) && any(as.character(panel$All.Data$GW.Data$WellName)==panel$Well)
NAPLAxis<-(panel$dlines["Overlay NAPL Thickness"] && NAPL.Present)

tempinc<-0.4
if(NAPLAxis | GWAxis){

	if(NAPLAxis && GWAxis){

		op<-par(mar=c(5.1,4.1,4.1,5.1+tempinc))

	}else{

		op<-par(mar=c(5.1,4.1,4.1,3.1+tempinc))
	}


}else{


	op<-par(mar=c(5.1,4.1,4.1,2.15+tempinc))

}





if(panel$dlines["Scale to Conc. Data"] & nrow(Well.Data)>0){

	my.ylim<-range(Well.Data$Result.Corr.ND,na.rm=T)
	if(!is.finite(my.ylim[2])){my.ylim[2]<-100000}
	my.xlim<-range(Well.Data$SampleDate)

}else{

	if(nrow(Well.Data)>0){my.ylim<-c(min(Well.Data$Result.Corr.ND,Stat.Lim,na.rm=T),max(Well.Data$Result.Corr.ND,Stat.Lim,na.rm=T))}
	else{my.ylim=c(0.01,100)}
	my.xlim<-range(c(panel$Cont.Data$SampleDate,panel$All.Data$GW.Data$SampleDate),na.rm=T) #maybe change to AggDate!
}


sm.fit<-NULL
sm.h<-panel$Traffic.Lights$h[panel$Well,panel$Cont.rg]


if(panel$dlines["Conc. Trend Smoother"] & !is.na(sm.h)){


my.eval.points<-seq(range(Well.Data$SampleDate)[1],range(Well.Data$SampleDate)[2],length=40)
sm.fit <- try(sm.regression(Well.Data$SampleDate, log(Well.Data$Result.Corr.ND), display = "none",h=sm.h,eval.points = my.eval.points))

	if(!inherits(sm.fit, "try-error")){

	sm.est.keep  <- sm.fit$estimate;
	sm.95up.keep <- exp(sm.est.keep+2*sm.fit$se)
	sm.95low.keep<- exp(sm.est.keep-2*sm.fit$se)
	sm.95up.keep[!sm.fit$se>smThreshSe]<-NA
	sm.95low.keep[!sm.fit$se>smThreshSe]<-NA
	sm.est.keep  <- exp(sm.est.keep)

	sm.fit$estimate[sm.fit$se>smThreshSe]<-NA
	sm.est	     <-exp(sm.fit$estimate)
	sm.95up      <-exp(sm.fit$estimate+2*sm.fit$se)
	sm.95low     <-exp(sm.fit$estimate-2*sm.fit$se)


		if(!panel$dlines["Scale to Conc. Data"]){

			my.ylim<-c(min(my.ylim[1],sm.95low,na.rm=T),max(my.ylim[2],sm.95up,na.rm=T))
			if(!is.finite(my.ylim[2])){my.ylim[2]<-100000}
		}

	}

}


if(panel$dlines["Conc. Linear Trend Fit"] & sum(Det.Pts)>0 & panel$Cont.rg!=" "){


	lm.fit<-try(lm(log(Result.Corr.ND)~SampleDate,Well.Data))
	lm.eval.points<-data.frame(SampleDate=as.Date(seq(range(Well.Data$SampleDate)[1],range(Well.Data$SampleDate)[2],length=100)))
	lm.pred<-predict(lm.fit,newdata=lm.eval.points,interval="confidence")
	lm.eval.points$fit<-exp(lm.pred[,"fit"])
	lm.eval.points$lwr<-exp(lm.pred[,"lwr"])
	lm.eval.points$upr<-exp(lm.pred[,"upr"])

	if(!panel$dlines["Scale to Conc. Data"]){

			my.ylim<-c(min(my.ylim[1],lm.eval.points$lwr,na.rm=T),max(my.ylim[2],lm.eval.points$upr,na.rm=T))
			if(!is.finite(my.ylim[2])){my.ylim[2]<-100000}
	}

	Mann.test<-try(Kendall(Well.Data$SampleDate,log(Well.Data$Result.Corr.ND)))

}


if(any(ND.Pts)){#Segment plotting


seg.lim<-Well.Data$Result.Corr.ND[ND.Pts]
if(length(grep("half",tolower(panel$GWSDAT_Options$NDMethod)))>0){seg.lim=2*seg.lim}	
try(if(my.ylim[2]<max(seg.lim)){my.ylim[2]<-max(seg.lim)})

}

if(any(!is.finite(my.ylim))){my.ylim=c(1,100)}

if(Use.LogScale){
	
	if(panel$dlines["Show Legend"]){my.ylim[2]<-10^(log(my.ylim[1],base=10)+1.15*log(my.ylim[2]/my.ylim[1],base=10))}# make space for Key!
	plot(Result.Corr.ND~SampleDate,Well.Data,xlab="Date",
	ylab=if(panel$Cont.rg!=" "){paste(panel$Cont.rg," (",panel$rgUnits,")",sep="")}else{""},
	ylim=my.ylim,xlim=my.xlim,log="y",cex.lab=1,cex.main=1,axes=FALSE)
	rect(par("usr")[1], 10^par("usr")[3], par("usr")[2], 10^par("usr")[4], col = "white") 
	
}else{

	if(panel$dlines["Show Legend"]){my.ylim[2]<-my.ylim[2]+diff(range(my.ylim))*.15}# make space for Key!
	plot(Result.Corr.ND~SampleDate,Well.Data,xlab="Date",
	ylab=if(panel$Cont.rg!=" "){paste(panel$Cont.rg," (",panel$rgUnits,")",sep="")}else{""},
	ylim=my.ylim,xlim=my.xlim,cex.lab=1,cex.main=1,axes=FALSE)
	rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "white") 
}


#axis.Date(1, my.xlim)
axis.Date(1, seq(my.xlim[1],my.xlim[2],l=10))
#,format="%b %d")


if(nrow(panel$Cont.Data[as.character(panel$Cont.Data$Result)!="NAPL" & !is.na(panel$Cont.Data$Result),])!=0){axis(2)} #if no Conc Data suppress Y-axis
box()	
title(main = paste(panel$Cont.rg, if(panel$Cont.rg!=" "){"in"}else{""}, panel$Well,if(panel$All.Data$Aq.sel!=""){paste(": Aquifer-",panel$All.Data$Aq.sel,sep="")}else{""}), font.main = 4, cex.main = 1)


grid(NA,NULL,lwd = 1,lty=1,equilogs = FALSE)

abline(v=as.Date(c(paste(1990:2030,c("-01-01"),sep=""),paste(1990:2030,c("-06-30"),sep=""))),lwd=1,lty=1,col = "lightgray")
if(length(grep("Threshold",panel$rg1))>0){if(!is.na(Stat.Lim)){abline(h=Stat.Lim,col="red",lty=2,lwd=3)}}

if(panel$dlines["Show Legend"]){

choose.vec=c(TRUE,TRUE,NAPL.Present,panel$dlines["Conc. Linear Trend Fit"],panel$dlines["Conc. Trend Smoother"],length(grep("Threshold",panel$rg1))>0,panel$dlines["Overlay GW levels"],if(is.na(panel$dlines["Overlay NAPL Thickness"])){FALSE}else{panel$dlines["Overlay NAPL Thickness"]})

if(!all(choose.vec[4:8]==FALSE)){
	try(legend("top",
	c("Detectable Data","Non-Detect Data","NAPL Substituted Data","Linear Conc. Trend","Conc. Smoother","Threshold Limit","GW Elevation","NAPL Thickness")[choose.vec],
	pch=c(19,19,19,-1,-1,-1,1,1)[choose.vec],
	lty=c(-1,-1,-1,1,1,2,1,1)[choose.vec],
	lwd=c(-1,-1,-1,2,2,3,1,1)[choose.vec],
	pt.cex=c(1.2,1.2,1.2,1.2,1.2,1.2,1,1)[choose.vec],
	col=c("black","orange","red","green","blue","red","black","red")[choose.vec],horiz = F,cex=.5,ncol=min(3,ceiling(sum(choose.vec)/2)))
	)


}else{
	
	try(legend("top",
	c("Detectable Data","Non-Detect Data","NAPL Substituted Data")[choose.vec[1:3]],
	pch=c(19,19,19)[choose.vec[1:3]],
	pt.cex=c(1.2,1.2,1.2)[choose.vec[1:3]],
	col=c("black","orange","red")[choose.vec[1:3]],
	horiz = F,cex=.5,ncol=min(3,ceiling(sum(choose.vec)/2))))

}

}





if(showvline){abline(v=panel$All.Agg.Dates[panel$jjj],col="grey",lwd=3)}
points(Result.Corr.ND~SampleDate,Well.Data[Det.Pts,],cex=1.5,pch=19,col="black")
points(Result.Corr.ND~SampleDate,Well.Data[ND.Pts, ],cex=1.5,pch=19,col="orange")
if(NAPL.Present){points(Result.Corr.ND~SampleDate,Well.Data[tolower(as.character(Well.Data$Result))=="napl", ],cex=1.5,pch=19,col="red")}


if(panel$dlines["Conc. Trend Smoother"] & !inherits(sm.fit, "try-error") & !is.null(sm.fit)){

	try(lines(my.eval.points,sm.est.keep,col="grey",lwd=2))#15Sep
	try(lines(my.eval.points,sm.95up.keep,col="grey",lwd=2,lty=2))
	try(lines(my.eval.points,sm.95low.keep,col="grey",lwd=2,lty=2))
	
	try(lines(my.eval.points,sm.est,col="blue",lwd=2))
	try(lines(my.eval.points,sm.95up,col="blue",lwd=2,lty=2))
	try(lines(my.eval.points,sm.95low,col="blue",lwd=2,lty=2))

}


if(panel$dlines["Conc. Linear Trend Fit"] & sum(Det.Pts)>1 & !inherits(lm.fit, "try-error") & panel$Cont.rg!=" "){


try(lines(lm.eval.points$SampleDate,lm.eval.points$fit,lwd=2,col="green"))
try(lines(lm.eval.points$SampleDate,lm.eval.points$lwr,lwd=2,col="green",lty=2))
try(lines(lm.eval.points$SampleDate,lm.eval.points$upr,lwd=2,col="green",lty=2))
 
temp.tex1<-paste("Mann-Kendall P.Value=",format.pval(Mann.test$sl,digits=3,eps=0.01))
try(half.life<- -round(log(2)/as.numeric(lm.fit$coeff[2])))
try(temp.tex2<-paste("Half-Life=",as.character(half.life),"days"))
try(if(abs(half.life)>0.5*3650)   {temp.tex2<-paste("Half-Life> 5 Years") })
try(if(half.life<(-0.5*3650)){temp.tex2<-paste("Half-Life> -5 Years")})
browser()
try(mtext(paste(temp.tex1,temp.tex2,sep="; "),side=3,line=0.4,cex=0.75,col=if(Mann.test$sl<0.05){"darkgreen"}else{"red"}))
}

kp<-par()

GWInc<-FALSE

if(panel$dlines["Overlay GW levels"]){

Well.GW.Data<-panel$All.Data$GW.Data[as.character(panel$All.Data$GW.Data$WellName)==panel$Well,]
Well.GW.Data<-Well.GW.Data[order(Well.GW.Data$SampleDate),]

	if(nrow(Well.GW.Data)>0){

		par(new=T)
		GW.ylim=range(Well.GW.Data$Result,na.rm=T)
		if(panel$dlines["Show Legend"]){GW.ylim[2]<-GW.ylim[2]+diff(range(GW.ylim,na.rm=T))*.15}
		plot(Result~SampleDate,Well.GW.Data,yaxt='n',xaxt='n',xlab="",ylab="",xlim=my.xlim,ylim=GW.ylim,type="b",col="black")
		axis(4,axTicks(4),cex.axis=.7,padj=-2.2,tcl=-0.3)
		mtext(paste("Groundwater Elevation (",panel$All.Data$GW.Units,")",sep=""), side=4,line=0.75,cex=.7,col="black")
		GWInc<-TRUE
	}

}



if(panel$dlines["Overlay NAPL Thickness"] & !is.null(panel$All.Data$NAPL.Thickness.Data)){

Well.NAPL.Thickness.Data<-panel$All.Data$NAPL.Thickness.Data[as.character(panel$All.Data$NAPL.Thickness.Data$WellName)==panel$Well,]
Well.NAPL.Thickness.Data<-Well.NAPL.Thickness.Data[order(Well.NAPL.Thickness.Data$SampleDate),]

	if(nrow(Well.NAPL.Thickness.Data)>0 && GWInc==FALSE){

		par(new=T)
		NAPL.ylim=range(Well.NAPL.Thickness.Data$Result.Corr.ND,na.rm=T)
		if(panel$dlines["Show Legend"]){NAPL.ylim[2]<-NAPL.ylim[2]+diff(range(NAPL.ylim,na.rm=T))*.15}

		plot(Result.Corr.ND~SampleDate,Well.NAPL.Thickness.Data,yaxt='n',xaxt='n',xlab="",ylab="",xlim=my.xlim,ylim=NAPL.ylim,type="b",col="red")
		axis(4,axTicks(4),cex.axis=.7,padj=-2.2,tcl=-0.3)
		mtext(paste("NAPL Thickness (",panel$All.Data$NAPL.Units,")",sep=""), side=4,line=0.75,cex=.7,col="black")
		
	}

	if(nrow(Well.NAPL.Thickness.Data)>0 && GWInc==TRUE){

		par(new=T)
		NAPL.ylim=range(Well.NAPL.Thickness.Data$Result.Corr.ND,na.rm=T)
		NAPL.ylim[1]=0
		if(panel$dlines["Show Legend"]){NAPL.ylim[2]<-NAPL.ylim[2]+diff(range(NAPL.ylim,na.rm=T))*.15}
		plot(Result.Corr.ND~SampleDate,Well.NAPL.Thickness.Data,yaxt='n',xaxt='n',xlab="",ylab="",xlim=my.xlim,ylim=NAPL.ylim,type="b",col="red")
		axis(4,axTicks(4),cex.axis=.7,col="red",line=2,padj=-2.2,tcl=-0.3)
		mtext(paste("NAPL Thickness (",panel$All.Data$NAPL.Units,")",sep=""), side=4,line=2.75,cex=.7,col="black")
		
	}


}

par(op)
return(panel)
}

############################################### End Time Series Plot ###########################################################

























































############################################### Traffic Light Plotting  #######################################################


click.Make.Traffic.Plot<-function(panel,x,y){
graphics.off();
windows(record=T,width =11, height = 9)
Make.Traffic.Plot(panel)
try(bringToTop())
return(panel)

}


Edit.Threshold.Lims<-function(panel){

my.df<-data.frame(Constituent=panel$Cont.Names,Threshold.Limit=as.numeric(as.character(panel$ContLimEntry)))
names(my.df)[2]<-"Threshold(ug/l)"
temp.df<-edit(my.df,edit.row.names=F)[,"Threshold(ug/l)"]

if(any(temp.df<=0)){
tkmessageBox(title="Data Input Error!",message="All threshold limits must be positive.",icon="error",type="ok")
return(panel)
}

panel$ContLimEntry<-as.character(temp.df)[1:length(panel$ContLimEntry)]

names(panel$ContLimEntry)<-panel$Cont.Names
ContLimEntry<<-panel$ContLimEntry #fix for R-3.0.0 tkrplot bug
panel<-replot.Make.Traffic.Plot(panel)
panel<-replot.SmoothPlot(panel)

print(panel$ContLimEntry)
return(panel)

}


Edit.Plume.Lims<-function(panel){

my.df<-data.frame(Constituent=panel$Cont.Names,Threshold.Limit=as.numeric(as.character(panel$PlumeLim)))
names(my.df)[2]<-"Threshold(ug/l)"


temp.df<-edit(my.df,edit.row.names=F)[,"Threshold(ug/l)"]

if(any(temp.df<=0)){
tkmessageBox(title="Data Input Error!",message="All threshold limits must be positive.",icon="error",type="ok")
return(panel)
}

panel$PlumeLimEntry<-as.character(temp.df)[1:length(panel$PlumeLim)]
names(panel$PlumeLimEntry)<-panel$Cont.Names

PlumeLimEntry<<-panel$PlumeLimEntry #fix for R-3.0.0 tkrplot bug

panel<-replot.MakeImagePlotNoPlot(panel)
return(panel)

}


Edit.Porosity<-function(panel){

keepPorosity<-as.numeric(as.character(panel$Porosity))
my.df<-data.frame(Porosity=as.numeric(as.character(panel$Porosity)))

my.df<-edit(my.df,edit.row.names=F)


if(as.numeric(my.df$Porosity)<=0 | as.numeric(my.df$Porosity)>1){

tkmessageBox(title="Data Input Error!",message="Porosity must be between 0 and 1",icon="error",type="ok")

}else{

panel$Porosity<-as.character(my.df$Porosity)
Porosity<<-panel$Porosity #fix for R-3.0.0 tkrplot bug

}


panel<-replot.MakeImagePlotNoPlot(panel)
return(panel)

}



Subset.Traffic.Plot<-function(panel){


graphics.off();
windows(record=T,width =11, height = 9)
Make.Traffic.Plot(panel,subset=TRUE)
try(bringToTop())
return(panel)

}



TrafficLightKey<-function(panel){
x11()
par(mfrow=c(1,2))
Num.Conts=1
my.cols<-c("grey","blue","#1A9641","#A6D96A","White","#FDAE61","#D7191C")
Num.Wells=length(my.cols)
Trend.labs<-c("Not Available","Non-Detect Data","Strong\nDownward\nTrend","Downward\nTrend","Stable","Upward\nTrend","Strong\nUpward\nTrend")
op<-par(xaxs="i", yaxs="i",mai=c(0.3,1.3,1,0.39375),mgp = c(1.5, 0.35,0)) 
plot(0:1,0:1,type="n",axes=F,xlab="",ylab="",main="Trend Key",cex.main=1)
x1<-seq(0,1,l=(Num.Conts+1))[1:Num.Conts]
y1=seq(0,1,l=(Num.Wells+1))[1:Num.Wells]
xleft<-expand.grid(x1,y1)[,1]
yleft=expand.grid(x1,y1)[,2]
xright=xleft+1/Num.Conts
yright=yleft+1/Num.Wells
rect(xleft,yleft,xright,yright,col=rev(my.cols))



try(axis(side=2,tick=FALSE,at=if(Num.Wells==1){0.5}
else{rollmean(seq(0,1,length=(Num.Wells+1)),2)}, labels=rev(Trend.labs),las=1,cex.axis = .85,font=2))
bringToTop()
cen.labs<-c("Doubling-Time<1 Year","1 Year<Doubling-Time<2 Years",
"Doubling-Time>2 Years\nHalf-Life>2 Years","1 Year<Half-Life<2 Years","Half-Life<1 Year","","")
text(0.5,seq(1/15,14/15,l=7),labels=cen.labs,cex=0.75)

Thresh.Cols<-rev(c("grey","blue","#1A9641","#D7191C"))
Thresh.Labs<-rev(c("Not Available","Non-Detect Data","Below\n(Significantly below)\nThreshold limit","Above\n(Not Significantly below)\n Threshold limit"))

par(op)

op<-par(xaxs="i", yaxs="i",mai=c(0.3,1.3,1,0.39375),mgp = c(1.5, 0.25,0)) 
plot(0:1,0:1,type="n",axes=F,xlab="",ylab="",main="Absolute (and Statistical) \n Threshold Key",cex.main=1)
xleft=xleft[4:7]
xright=xright[4:7]
yleft=yleft[4:7]
yright=yright[4:7]
rect(xleft,yleft,xright,yright,col=Thresh.Cols)
try(axis(side=2,tick=FALSE,at=if(Num.Wells==1){0.5}
else{rollmean(seq(0,1,length=(Num.Wells+1)),2)[4:7]}, labels=Thresh.Labs,las=1,adj=0,cex.axis = .85,font=2))



################ Time Series Plot Legend ###########################################




	try(legend("bottom",
	c("Detectable Data","Non-Detect Data","NAPL Substituted Data","Linear Conc. Trend","Conc. Smoother","Threshold Limit","GW Elevation","NAPL Thickness"),
	pch=c(19,19,19,-1,-1,-1,1,1),
	lty=c(-1,-1,-1,1,1,2,1,1),
	lwd=c(-1,-1,-1,2,2,3,1,1),
	pt.cex=c(1.2,1.2,1.2,1.2,1.2,1.2,1,1),
	col=c("black","orange","red","green","blue","red","black","red"),horiz = F,cex=.85)
	)

text(0.5,.3,"Time Series Plot Key",font=2)
#-----------------------------------------------------------------------------------#

bringToTop()
par(op)
par(mfrow=c(1,1))
return(panel)

}















Make.Traffic.Plot<-function(panel,subset=FALSE,fromDoubleButton=TRUE){


panel$ContLimEntry<-ContLimEntry #fix for R-3.0.0 tkrplot bug

if(fromDoubleButton){
my.jjj<-panel$shadow.jjj%%length(panel$All.Data$All.Agg.Dates)
if(my.jjj==0){my.jjj=length(panel$All.Data$All.Agg.Dates)}
panel$jjj=my.jjj

}

jjj=panel$jjj
date.to.print <- format(as.Date(panel$Fitted.Data[[1]]$Time.Eval[jjj]),"%d-%b-%Y")


if(panel$rg1=="Trend"){

	temp.traffic.Beta.ND.Check<-panel$Traffic.Lights$Beta.ND.Check[,,jjj,drop=F]
	temp.traffic.Betas<-panel$Traffic.Lights$Betas[,,jjj,drop=F]
	Well.Names<-dimnames(temp.traffic.Betas)[[1]]
	Cont.Names<-dimnames(temp.traffic.Betas)[[2]]
}

if(panel$rg1=="Threshold - Statistical"){
	
	temp.traffic.Beta.ND.Check<-panel$Traffic.Lights$Beta.ND.Check[,,jjj,drop=F]
	temp.traffic.Ulims<-panel$Traffic.Lights$Smooth.Upper.lims[,,jjj,drop=F]
	Well.Names<-dimnames(temp.traffic.Ulims)[[1]]
	Cont.Names<-dimnames(temp.traffic.Ulims)[[2]]

}

if(panel$rg1=="Threshold - Absolute"){

	temp.traffic.Abs.Thresh.Check<-panel$Traffic.Lights$Abs.Thresh.Check[,,jjj,drop=F]
	Well.Names<-dimnames(temp.traffic.Abs.Thresh.Check)[[1]]
	Cont.Names<-dimnames(temp.traffic.Abs.Thresh.Check)[[2]]
}


if(subset==TRUE){

	if(length(Cont.Names)==1){Cont.Names<-Cont.Names[1]}else{Cont.Names<-GWSDAT.select.list(Cont.Names,multiple =T,title = "Select Solutes to Plot",preselect=Cont.Names)}
	if(length(Cont.Names)==0){stop("No Solutes selected!")}

	Well.Names<-GWSDAT.select.list(Well.Names,multiple =T,title = "Select Wells to Plot",preselect=Well.Names)
	if(length(Well.Names)==0){stop("No Wells selected!")}

}

if(panel$rg1=="Trend"){

	temp.traffic.Beta.ND.Check<-panel$Traffic.Lights$Beta.ND.Check[Well.Names,Cont.Names,jjj,drop=F]
	temp.traffic.Betas<-panel$Traffic.Lights$Betas[Well.Names,Cont.Names,jjj,drop=F]
	temp.traffic.Betas[!is.finite(temp.traffic.Betas)]<-NA
	temp.traffic.Betas<-zapsmall(temp.traffic.Betas)

	Well.Names<-dimnames(temp.traffic.Betas)[[1]]
	Cont.Names<-dimnames(temp.traffic.Betas)[[2]]
	Num.Conts<-length(Cont.Names)
	Num.Wells<-length(Well.Names)

	my.palette<-c("#1A9641","#A6D96A","White","#FDAE61","#D7191C")
	my.breaks<-c(-Inf, -0.0019,  -0.000949, 0.000949,0.0019 ,Inf) # 
	temp.traffic.light<-rep("grey",length(temp.traffic.Betas))
	tr<-my.palette[as.numeric(cut(as.numeric((temp.traffic.Betas)),breaks=my.breaks, include.lowest = T))]

	##ND filter!
	tr[is.na(tr)]<-"grey"
	tr[as.numeric(temp.traffic.Beta.ND.Check)==1]<-"blue"
	temp.traffic.light=tr

}

if(panel$rg1=="Threshold - Statistical"){

	temp.traffic.Beta.ND.Check<-panel$Traffic.Lights$Beta.ND.Check[Well.Names,Cont.Names,jjj,drop=F]
	temp.traffic.Ulims<-panel$Traffic.Lights$Smooth.Upper.lims[Well.Names,Cont.Names,jjj,drop=F]
	Well.Names<-dimnames(temp.traffic.Ulims)[[1]]
	Cont.Names<-dimnames(temp.traffic.Ulims)[[2]]
	Num.Conts<-length(Cont.Names)
	Num.Wells<-length(Well.Names)
	temp.traffic.Ulims<-as.numeric(temp.traffic.Ulims)


	Stat.Lim<-as.numeric(panel$ContLimEntry[Cont.Names])
	Stat.Lim<-rep(Stat.Lim,each=length(Well.Names))
	temp.traffic.light<-rep("grey",length(Stat.Lim))

	temp.traffic.light[temp.traffic.Ulims<=Stat.Lim]<-"#1A9641"#"#1A9850"#"green"
	temp.traffic.light[temp.traffic.Ulims>Stat.Lim]<-"#D7191C"#"#D73027"#"red"
	temp.traffic.light[as.numeric(temp.traffic.Beta.ND.Check)==1]<-"blue"

}


if(panel$rg1=="Threshold - Absolute"){


	temp.traffic.Abs.Thresh.Check<-panel$Traffic.Lights$Abs.Thresh.Check[Well.Names,Cont.Names,jjj,drop=F]
	Well.Names<-dimnames(temp.traffic.Abs.Thresh.Check)[[1]]
	Cont.Names<-dimnames(temp.traffic.Abs.Thresh.Check)[[2]]
	Num.Conts<-length(Cont.Names)
	Num.Wells<-length(Well.Names)
	temp.traffic.Abs.Thresh.Check<-as.numeric(temp.traffic.Abs.Thresh.Check)
	
	
	Stat.Lim<-as.numeric(panel$ContLimEntry[Cont.Names])
	Stat.Lim<-rep(Stat.Lim,each=length(Well.Names))
	

	temp.traffic.light<-rep("grey",length(Stat.Lim))

	temp.traffic.light[temp.traffic.Abs.Thresh.Check<=Stat.Lim]<-"#1A9641"#"#1A9850"#"green"
	
	temp.traffic.light[temp.traffic.Abs.Thresh.Check>Stat.Lim]<-"#D7191C"#"#D73027"#"red"
	temp.traffic.light[temp.traffic.Abs.Thresh.Check==-1]<-"blue"

}



if(!is.null(panel$ColTrafficListbox)){

if(panel$ColTrafficListbox!="All" & subset==FALSE){

if(panel$ColTrafficListbox=="White"){my.temp.col<-"White"}
if(panel$ColTrafficListbox=="Reds"){my.temp.col<-c("#FDAE61","#D7191C")}
if(panel$ColTrafficListbox=="Greens"){my.temp.col<-c("#1A9641","#A6D96A")}
if(panel$ColTrafficListbox=="Non-Detects"){my.temp.col<-"blue"}
if(panel$ColTrafficListbox=="Greys"){my.temp.col<-"grey"}

	Traffic.col.mat<-matrix(temp.traffic.light,ncol=Num.Conts,nrow=Num.Wells)
	rownames(Traffic.col.mat)<-Well.Names
	colnames(Traffic.col.mat)<-Cont.Names
	Traffic.col.mat<-Traffic.col.mat[apply(Traffic.col.mat,1,function(x){any(my.temp.col %in% x)}),apply(Traffic.col.mat,2,function(x){any(my.temp.col %in% x)}),drop=FALSE]
	Well.Names<-rownames(Traffic.col.mat)
	Cont.Names<-colnames(Traffic.col.mat)
	Num.Wells<-length(Well.Names)
	Num.Conts<-length(Cont.Names)
	temp.traffic.light<-as.character(Traffic.col.mat)

	
	if(length(temp.traffic.light)==0){Well.Names<-"Not Available";Cont.Names<-"Not Available";Num.Wells<-Num.Conts<-1; temp.traffic.light="grey"}
}
}



#op<-par(xaxs="i", yaxs="i",mai=c(0.3,1,1,0.39375),mgp = c(1.5, 0.5,0)) 
op<-par(xaxs="i", yaxs="i",mai=c(0.3,1,1,0.3),mgp = c(1.5, 0.5,0)) 

plot(0:1,0:1,type="n",axes=F,xlab="",ylab="")
x1<-seq(0,1,l=(Num.Conts+1))[1:Num.Conts]
y1=seq(0,1,l=(Num.Wells+1))[1:Num.Wells]
xleft<-expand.grid(x1,y1)[,1]
yleft=expand.grid(x1,y1)[,2]
xright=xleft+1/Num.Conts
yright=yleft+1/Num.Wells

rect(xleft,yleft,xright,yright,col=t(matrix(temp.traffic.light,ncol=Num.Conts)))


try(axis(side=2,tick=FALSE,at=if(Num.Wells==1){0.5}else{rollmean(seq(0,1,length=(Num.Wells+1)),2)}, labels=Well.Names,las=1,cex.axis = .85,font=2))
text(if(Num.Conts==1){0.5}else{rollmean(seq(0,1,length=(Num.Conts+1)),2)}, par("usr")[4], Cont.Names,srt=35,adj = c(0, 0),xpd=T,cex=.95,font=2) #.95 

#Threshxlabs<-if(panel$rgUnits=="mg/l"){paste("<",as.numeric(panel$ContLimEntry[Cont.Names])/1000," mg/l",sep="")}else{paste("<",panel$ContLimEntry[Cont.Names]," ug/l",sep="")}
if(panel$rgUnits=="mg/l"){Threshxlabs<-paste("<",as.numeric(panel$ContLimEntry[Cont.Names])/1000," mg/l",sep="")}
if(panel$rgUnits=="ug/l"){Threshxlabs<-paste("<",panel$ContLimEntry[Cont.Names]," ug/l",sep="")}
if(panel$rgUnits=="ng/l"){Threshxlabs<-paste("<",as.numeric(panel$ContLimEntry[Cont.Names])*1000," ng/l",sep="")}


if(panel$rg1=="Threshold - Statistical"){axis(side=1,tick=FALSE,at=if(Num.Conts==1){0.5}else{rollmean(seq(0,1,length=(Num.Conts+1)),2)},las=2, labels=Threshxlabs,las=1,cex.axis = 0.85,padj=-0.1)}
if(panel$rg1=="Threshold - Absolute")   {axis(side=1,tick=FALSE,at=if(Num.Conts==1){0.5}else{rollmean(seq(0,1,length=(Num.Conts+1)),2)},las=2, labels=Threshxlabs,las=1,cex.axis = 0.85,padj=-0.1)}
if(panel$rg1=="Trend")                  {axis(side=1,tick=FALSE,at=if(Num.Conts==1){0.5}else{rollmean(seq(0,1,length=(Num.Conts+1)),2)},       labels=rep("Trend",Num.Conts),     las=1,cex.axis = 1)}


mtext(paste(panel$rg1, ": ", date.to.print,if(panel$All.Data$Aq.sel!=""){paste(": Aquifer-",panel$All.Data$Aq.sel,sep="")}else{""},sep=""),padj=-5,font=2,cex=1.1)

par(op)

return(panel)
}



#--------------------------------------------------- End Traffic Lights ---------------------------------------------------------#



































############################################### Report Generation  Functions #####################################################

setupPPV2<-function(){

require(RDCOMClient)
ppt<<-COMCreate("PowerPoint.Application")
ppt[["Visible"]]<<-TRUE
myPres<<-ppt[["Presentations"]]$add()
mySlides<<-myPres[["Slides"]]


}


setupWdV2<-function(){

require(RDCOMClient)
GWSDATR2wd<<-COMCreate("Word.Application")
GWSDATR2wd[['Documents']]$Add()
GWSDATR2wd[['visible']]<-TRUE

}




AddPlotPPV2<-function(panel,asp=FALSE){
require(RDCOMClient)


calledsetupPP <- FALSE
CheckforNoPlot <- is.na(match(dev.cur(), dev.list(), NA))
if(CheckforNoPlot){rp.messagebox("No active plot to export.", title = "Error"); return(panel)}
CheckPPT<-exists('ppt',envir=globalenv())

if(CheckPPT == FALSE){setupPPV2(); calledsetupPP = TRUE; CheckPPT = TRUE}


CheckPPT2 <- try(as.logical(ppt[['Visible']]),silent=TRUE)
if(inherits(CheckPPT2, "try-error")){CheckPPT2<-FALSE}


CheckPPT3 <- try(!is.null(myPres[['Name']]),silent=TRUE)
if(inherits(CheckPPT3, "try-error")){CheckPPT3<-FALSE}


if((CheckPPT & CheckPPT2 & CheckPPT3) == FALSE){setupPPV2(); calledsetupPP = TRUE}





OutputGraphics <- panel$GWSDAT_Options$OutputGraphics
if(is.null(OutputGraphics)){OutputGraphics="wmf"}

mytemp<-tempfile()
mytemp<-paste(mytemp,OutputGraphics ,sep='.')
mytemp<- gsub("/", "\\\\", as.character(mytemp))

savePlot(mytemp,type=OutputGraphics,restoreConsole = FALSE)
if(file.exists(paste(mytemp,OutputGraphics,sep="."))){file.rename(paste(mytemp,OutputGraphics,sep="."),mytemp)}

mySlide<-myPres[["Slides"]]$add(as.integer(max(1,myPres[["Slides"]]$Count()+1)),as.integer(12))
myShapes<-mySlide$Shapes()

if(asp){### Maintain aspect ratio in PowerPoint Plots!
my.din<-par()$din

	if((my.din[1]/my.din[2])>=1.4){

		my.size<-c(10,(540-700*my.din[2]/my.din[1])/2,700,700*my.din[2]/my.din[1])

	}else{

		my.size<-c((720-500*my.din[1]/my.din[2])/2,20,500*my.din[1]/my.din[2],500)

	}
}else{

my.size<-c(10,10,700,500)

}


myShapes$AddPicture(mytemp,0,-1,my.size[1],my.size[2],my.size[3],my.size[4]) 
mySlide$Select()


try(file.remove(mytemp))
try(rm(mytemp))
return(panel)
}




AddPlotWORDV2<-function(panel){

require(RDCOMClient)


##### Check if live plot exists ###################################
CheckforNoPlot <- is.na(match(dev.cur(), dev.list(), NA))

if(CheckforNoPlot){

	rp.messagebox("No active plot to export.", title = "Error"); 
	return(panel)

}

#### Check if Word Document open, if not initiate one ###############
calledsetupWORD <- FALSE
CheckWd<-exists('GWSDATR2wd',envir=globalenv())
if(CheckWd == FALSE){setupWdV2(); calledsetupWORD = TRUE; CheckWd = TRUE}

CheckWd2 <- try(!is.null(GWSDATR2wd[['visible']]) && GWSDATR2wd[['visible']],silent=TRUE)
if(inherits(CheckWd2, "try-error")){CheckWd2<-FALSE}else{CheckWd2<-TRUE}

CheckWd3<-try(!is.null(GWSDATR2wd[['ActiveDocument']][['Name']]),silent=TRUE)
if(inherits(CheckWd3, "try-error")){CheckWd3<-FALSE}else{CheckWd3<-TRUE}


if((CheckWd & CheckWd2 & CheckWd3) == FALSE){setupWdV2(); calledsetupWORD = TRUE}


OutputGraphics <- panel$GWSDAT_Options$OutputGraphics
if(is.null(OutputGraphics)){OutputGraphics="wmf"}

mytemp<-tempfile()
mytemp<-paste(mytemp,OutputGraphics ,sep='.')
mytemp<- gsub("/", "\\\\", as.character(mytemp))

savePlot(mytemp,type=OutputGraphics,restoreConsole = FALSE)
if(file.exists(paste(mytemp,OutputGraphics,sep="."))){file.rename(paste(mytemp,OutputGraphics,sep="."),mytemp)}


GWSDATR2wd[['Selection']]$TypeParagraph()
GWSDATR2wd[['Selection']][['InlineShapes']]$AddPicture(mytemp)


try(file.remove(mytemp))
try(rm(mytemp))
return(panel)
}












AddPlotPPfromFilesV2<-function(filenames){
require(RDCOMClient)

filenames<- gsub("/", "\\\\", as.character(filenames))

calledsetupPP <- FALSE
CheckPPT<-exists('ppt',envir=globalenv())

if(CheckPPT == FALSE){setupPPV2(); calledsetupPP = TRUE; CheckPPT = TRUE}

CheckPPT2 <- try(as.logical(ppt[['Visible']]),silent=TRUE)
if(inherits(CheckPPT2, "try-error")){CheckPPT2<-FALSE}

CheckPPT3 <- try(!is.null(myPres[['Name']]),silent=TRUE)
if(inherits(CheckPPT3, "try-error")){CheckPPT3<-FALSE}

if((CheckPPT & CheckPPT2 & CheckPPT3) == FALSE){setupPPV2(); calledsetupPP = TRUE}



for(i in filenames){

mySlide<-myPres[["Slides"]]$add(as.integer(max(1,myPres[["Slides"]]$Count()+1)),as.integer(12))


myShapes<-mySlide$Shapes()

myShapes$AddPicture(as.character(i),0,-1,10,10,700,500) 

mySlide$Select()


}

}





GWSDAT.xyplotWells<-function(myrpanel,Cont.Data,SiteName="",sm.fit=TRUE,UseLogScale=FALSE){

require(lattice)

NAPL.Present<-any(tolower(as.character(na.omit(Cont.Data$Result)))=="napl")


Cont<-unique(Cont.Data$Constituent)
my.xlim<-range(c(myrpanel$Cont.Data$SampleDate,myrpanel$All.Data$GW.Data$SampleDate)) 

my.xlim.orig=my.xlim
my.xlim[1]<-my.xlim.orig[1]-0.025*as.numeric(diff(my.xlim.orig))
my.xlim[2]<-my.xlim.orig[2]+0.025*as.numeric(diff(my.xlim.orig))


my.key <- list( 
        space = "top", 
        border = FALSE, 
	columns=2,
        points = list( 
                pch = rep(19,2), 
                cex = rep(1.4,2), 
                col = c("black","orange")
        ), 
        text = list( 
                lab=c("Detectable Data","Non-Detect Data")
        ) 
) 

if(NAPL.Present){

my.key <- list( 
        space = "top", 
        border = FALSE, 
	columns=3,
        points = list( 
                pch = rep(19,3), 
                cex = rep(1.4,3), 
                col = c("black","orange","red")
        ), 
        text = list( 
                lab=c("Detectable Data","Non-Detect Data","NAPL Substituted Data")
        ) 
) 

Cont.Data$ND<-as.character(Cont.Data$ND)
Cont.Data$ND[tolower(as.character(Cont.Data$Result))=="napl"]<-"NAPL"
}


my.plot<-xyplot(Result.Corr.ND~as.Date(SampleDate)|WellName,data=Cont.Data,groups=as.character(Cont.Data$ND),

panel = function(x, y,groups,subscripts) {
try(panel.grid(h=-1, v= 2))
groupNDx<-x[groups[subscripts]=="TRUE"]
groupNDy<-y[groups[subscripts]=="TRUE"]
panel.xyplot(groupNDx,groupNDy,col="orange",pch=19,cex=1.0)



groupx<-x[groups[subscripts]=="FALSE"]
groupy<-y[groups[subscripts]=="FALSE"]
panel.xyplot(groupx,groupy,col="black",pch=19,cex=1.0)


groupNAPLx<-x[groups[subscripts]=="NAPL"]
groupNAPLy<-y[groups[subscripts]=="NAPL"]

if(length(groupNAPLx)>0){panel.xyplot(groupNAPLx,groupNAPLy,col="red",pch=19,cex=1.0)}


                
		if(sm.fit && length(x)>1){
		
		
		h=try(myrpanel$Traffic.Lights$h[as.character(unique(Cont.Data[subscripts,"WellName"])),as.character(Cont)])
		try(eval.points<-seq(min(x,na.rm=T),max(x,na.rm=T),l=40))
		try(if(UseLogScale){y=log(10^y)}else{y=log(y)})
		try(sr<-sm.regression(x,y,h=h,display="none",eval.points=eval.points))
		
			try(sr.keep<-sr$estimate)
			#try(sm.est.keep  <- sm.fit$estimate)
			try(sr.keep      <- if(UseLogScale){log(exp(sr$estimate),base=10)}else{exp(sr$estimate)})
			try(sr.95up.keep <- if(UseLogScale){log(exp(sr$estimate+2*sr$se),base=10)}else{exp(sr$estimate+2*sr$se)})
			try(sr.95low.keep<- if(UseLogScale){log(exp(sr$estimate-2*sr$se),base=10)}else{exp(sr$estimate-2*sr$se)})
			try(panel.xyplot(as.Date(sr$eval.points), sr.keep,type="l",col="grey"))
			try(panel.xyplot(as.Date(sr$eval.points), sr.95up.keep,type="l",col="grey"))
			try(panel.xyplot(as.Date(sr$eval.points), sr.95low.keep,type="l",col="grey"))


		try(sr$estimate[sr$se>myrpanel$GWSDAT_Options$smThreshSe]<-NA)
		try(sr.fit<-  if(UseLogScale){log(exp(sr$estimate),base=10)}else{exp(sr$estimate)})
		try(sr.95up<- if(UseLogScale){log(exp(sr$estimate+2*sr$se),base=10)}else{exp(sr$estimate+2*sr$se)})
		try(sr.95low<-if(UseLogScale){log(exp(sr$estimate-2*sr$se),base=10)}else{exp(sr$estimate-2*sr$se)})
                try(panel.xyplot(as.Date(sr$eval.points), sr.fit,type="l",col="blue"))
		try(panel.xyplot(as.Date(sr$eval.points), sr.95up,type="l",col="blue"))
		try(panel.xyplot(as.Date(sr$eval.points), sr.95low,type="l",col="blue"))

		}
		
            },

scales=list(y=list(log=UseLogScale)),
xlab=list("Sampling Date",cex=1.5),ylab=list(paste("Solute concentration"," (",myrpanel$rgUnits,")",sep=""),cex=1.5),
layout=if(length(levels(Cont.Data$Well))>30){c(4,4)}else{NULL},xlim=my.xlim,

main=if(myrpanel$All.Data$Aq.sel==""){paste(Cont,"at",SiteName)}else{paste(Cont," at ",SiteName,": Aquifer-",myrpanel$All.Data$Aq.sel,sep="")},
drop.unused.levels=FALSE,key=my.key) 

return(my.plot)



}

#-------------------------------------------------------------------------------#







################### All Wells all Conts #########################################


GWSDAT.xyplotAllContbyWells<-function(panel,Cont.Data,SiteName="",UseLogScale=FALSE){


my.xlim=range(panel$Cont.Data$SampleDate,na.rm=T)
my.xlim=c(min(panel$Cont.Data$SampleDate,na.rm=T),max(panel$Cont.Data$AggDate,na.rm=T)) #Global xlim range!
my.xlim<-range(c(my.xlim,panel$Cont.Data$SampleDate,panel$All.Data$GW.Data$SampleDate),na.rm=T) 

my.xlim.orig=my.xlim
my.xlim[1]<-my.xlim.orig[1]-0.025*as.numeric(diff(my.xlim.orig))
my.xlim[2]<-my.xlim.orig[2]+0.025*as.numeric(diff(my.xlim.orig))


palette("default")
palette(c(palette(),"purple","orange","deeppink4","springgreen","indianred"))
Num.Conts<-nlevels(Cont.Data$Constituent)
if(Num.Conts>length(palette())){palette(rainbow(Num.Conts))}

my.key <- list( 
        space = "top", 
        border = FALSE, 
	columns=min(Num.Conts,4),
        points = list( 
                pch = rep(19,Num.Conts), 
                cex = rep(1.2,Num.Conts), 
                col = 1:Num.Conts
        ), 
        text = list( 
                lab=as.character(levels(Cont.Data$Constituent)) 
        ) 
) 


browser()
myplot<-xyplot(Result.Corr.ND~SampleDate|WellName,groups=Constituent,data=Cont.Data,

scales=list(y=list(log=UseLogScale)),layout=if(length(unique(Cont.Data$WellName))>30){c(4,4)}else{NULL},type=c("b"),
pch=19,cex=0.75,col=1:Num.Conts,lwd=1,
key=my.key,xlab=list("Sampling Date",cex=1.5),ylab=list(paste("Solute concentration"," (",panel$rgUnits,")",sep=""),cex=1.5),
main=if(panel$All.Data$Aq.sel==""){SiteName}else{paste(SiteName,": Aquifer-",panel$All.Data$Aq.sel,sep="")},
drop.unused.levels=FALSE,xlim=my.xlim
)


return(myplot)

}



############## Well Report ###############################################################################

WellReport<-function(panel,toPPT=FALSE){

on.exit(palette("default"))
require(lattice)
Cont.Data<-panel$All.Data$Cont.Data
All.Conts<-panel$All.Data$All.Conts
All.Wells<-sort(as.character(panel$All.Data$All.Wells))

SiteName<-panel$GWSDAT_Options$SiteName


if(length(All.Conts)==1){Conts.to.plot<-All.Conts[1]}else{Conts.to.plot<-GWSDAT.select.list(All.Conts,multiple =T,title = "Select Solutes to Plot",preselect=All.Conts)}
	
if(length(Conts.to.plot)==0){stop("No Solutes selected!")}
Cont.Data<-Cont.Data[as.character(Cont.Data$Constituent) %in% Conts.to.plot,]
Cont.Data$Constituent<-factor(as.character(Cont.Data$Constituent))


Wells.to.Plot<-GWSDAT.select.list(All.Wells,multiple =T,title = "Select Wells to Plot",preselect=All.Wells)
if(length(Wells.to.Plot)==0){stop("No Wells selected!")}
Cont.Data<-Cont.Data[as.character(Cont.Data$WellName) %in% Wells.to.Plot,]

if(nrow(Cont.Data)==0){rp.messagebox("No Data to Plot!", title = "Error"); stop("No Data to Plot")} #need messagebox here!

Cont.Data$WellName<-factor(as.character(Cont.Data$WellName),levels=sort(Wells.to.Plot))
Cont.Data<-Cont.Data[order(Cont.Data$SampleDate),]

UseLogScale<-GWSDAT.select.list(c("No","Yes"),multiple =F,title = "Use Log Scale?",preselect="Yes")
if(length(UseLogScale)==0){stop("No Choice of LogScale selected!")}
if(UseLogScale=="No"){UseLogScale=FALSE}else{UseLogScale=TRUE}


if(panel$rgUnits=="mg/l"){Cont.Data$Result.Corr.ND<-Cont.Data$Result.Corr.ND/1000}
if(panel$rgUnits=="ng/l"){Cont.Data$Result.Corr.ND<-Cont.Data$Result.Corr.ND*1000}

if(length(Conts.to.plot)==1){

myplot<-GWSDAT.xyplotWells(panel,Cont.Data,SiteName=SiteName,sm.fit=panel$dlines["Conc. Trend Smoother"],UseLogScale=UseLogScale)

}else{

myplot<-GWSDAT.xyplotAllContbyWells(panel,Cont.Data,SiteName=SiteName,UseLogScale=UseLogScale)

}

	if(!toPPT){

		.SavedPlots<<-NULL
		graphics.off()
		windows(record=T,width =11, height = 9)
		print(myplot)

	}else{
		try(file.remove(list.files(dirname(tempfile()),pattern="Wellgraph",full.names=T)))
		OutputGraphics <- panel$GWSDAT_Options$OutputGraphics
		if(is.null(OutputGraphics)){OutputGraphics="wmf"}
		if(OutputGraphics=="jpeg"){jpeg(file=paste(dirname(tempfile()),"/","Wellgraph%03d.jpeg",sep=""), width=960, height=960, quality=100)}
		if(OutputGraphics=="png") {png(file=paste(dirname(tempfile()),"/","Wellgraph%03d.png",sep=""), width=960, height=960)}
		if(OutputGraphics=="wmf") {win.metafile(file=paste(dirname(tempfile()),"/","Wellgraph%03d.emf",sep=""),width = 9, height =7) }
		
		print(myplot)
		dev.off()
		AddPlotPPfromFilesV2(sort(list.files(dirname(tempfile()),pattern="Wellgraph",full.names=T)))
		try(file.remove(list.files(dirname(tempfile()),pattern="Wellgraph",full.names=T)))

	}


return(panel)
}

#--------------------------------------------------- End Report Generation  -----------------------------------------------------#




############## GW Well Report ###############################################################################

GWWellReport<-function(panel,toPPT=FALSE){

Keep.Well<-panel$Well
Keep.Cont<-panel$Cont.rg
Keep.DispGW<-panel$dlines["Overlay GW levels"]

ReturnSavedVars<-function(panel,Keep.Well,Keep.Cont,Keep.DispGW){
	panel$Well<-Keep.Well
	panel$Cont.rg<-Keep.Cont
	panel$dlines["Overlay GW levels"]<-Keep.DispGW
	return(panel)
}


on.exit(return(panel<-ReturnSavedVars(panel,Keep.Well,Keep.Cont,Keep.DispGW)))


Cont.Data<-panel$All.Data$Cont.Data
All.Conts<-panel$All.Data$All.Conts
All.Wells<-sort(as.character(panel$All.Data$All.Wells))
SiteName<-panel$GWSDAT_Options$SiteName


if(length(All.Conts)==1){Cont.to.plot<-All.Conts[1]}else{Cont.to.plot<-GWSDAT.select.list(All.Conts,multiple =FALSE,title = "Select Solute to Plot")}



if(length(Cont.to.plot)==0 | Cont.to.plot==""){stop("No Solute selected!")}

Wells.to.Plot<-GWSDAT.select.list(All.Wells,multiple =T,title = "Select Wells to Plot",preselect=All.Wells)
if(length(Wells.to.Plot)==0){stop("No Wells selected!")}


panel$Cont.rg<-Cont.to.plot

if(!toPPT){

	graphics.off();
	.SavedPlots<<-NULL
	windows(record=T,width =11, height = 9)
	if(length(Wells.to.Plot)==1){par(mfrow=c(1,1))}
	if(length(Wells.to.Plot)==2){par(mfrow=c(2,1))}
	if(length(Wells.to.Plot) >2){par(mfrow=c(2,2))}


		for(i in 1:length(Wells.to.Plot)){

			try(panel$Well<-Wells.to.Plot[i])
			try(MakeSmoothPlot(panel,showvline=FALSE))
			try(bringToTop())
		}



}else{

	graphics.off();
	try(file.remove(list.files(dirname(tempfile()),pattern="GWWellgraph",full.names=T)))
	OutputGraphics <- panel$GWSDAT_Options$OutputGraphics
	if(is.null(OutputGraphics)){OutputGraphics="wmf"}
	if(OutputGraphics=="jpeg"){jpeg(file=paste(dirname(tempfile()),"/","GWWellgraph%03d.jpeg",sep=""), width=960, height=960, quality=100)}
	if(OutputGraphics=="png") {png(file=paste(dirname(tempfile()),"/","GWWellgraph%03d.jpeg",sep=""), width=960, height=960)}
	if(OutputGraphics=="wmf") {win.metafile(file=paste(dirname(tempfile()),"/","GWWellgraph%03d.wmf",sep=""),width = 9*1.4, height = 7*1.1) }

	if(length(Wells.to.Plot)==1){par(mfrow=c(1,1))}
	if(length(Wells.to.Plot)==2){par(mfrow=c(2,1))}
	if(length(Wells.to.Plot) >2){par(mfrow=c(2,2))}

		for(i in 1:length(Wells.to.Plot)){

			try(panel$Well<-Wells.to.Plot[i])
			try(MakeSmoothPlot(panel,showvline=FALSE))
			
		}

	dev.off()
	AddPlotPPfromFilesV2(sort(list.files(dirname(tempfile()),pattern="GWWellgraph",full.names=T)))
	try(file.remove(list.files(dirname(tempfile()),pattern="GWWellgraph",full.names=T)))
}


return(panel)
}

#-------------------------- End GW Well Report  ---------------------------------------------------------#





##################### Output Model Diagnostic Table ######################################################

Model.Diag<-function(panel){

	Conts.select<-names(panel$Fitted.Data)
	if(length(Conts.select)==1){Model.Diag.Sel<-Conts.select[1]}else{Model.Diag.Sel<-GWSDAT.select.list(Conts.select,preselect =Conts.select,multiple=TRUE,title="Select Constituents")}

	temp.dat<-NULL


  if(length(Model.Diag.Sel)>0){


	for(i in Model.Diag.Sel){

		temp.dat<-try(rbind(temp.dat,panel$Fitted.Data[[i]]$Cont.Data[,c("WellName","Constituent","SampleDate","Result","Lower95","ModelPred","Upper95")]))

	}

  if(panel$rgUnits=="mg/l"){

	temp.dat$ModelPred<-temp.dat$ModelPred/1000
	temp.dat$Upper95<-temp.dat$Upper95/1000
	temp.dat$Lower95<-temp.dat$Lower95/1000
	
	temp.res<-as.character(temp.dat$Result)
	temp.res<-gsub("ND<","",temp.res)
	temp.res[tolower(temp.res)!="napl"]<-as.character(as.numeric(temp.res[tolower(temp.res)!="napl"])/1000)
	temp.res[grep("ND<",temp.dat$Result)]<-paste("ND<",temp.res[grep("ND<",temp.dat$Result)],sep="")
	temp.dat$Result<-temp.res
	try(temp.dat$ModelPred<-round(temp.dat$ModelPred,4))
	try(temp.dat$Upper95<-round(temp.dat$Upper95,4))
  	try(temp.dat$Lower95<-round(temp.dat$Lower95,4))	
  
	temp.dat$Units<-rep("mg/l",nrow(temp.dat))
	rm(temp.res)

  } else if(panel$rgUnits=="ng/l"){

	temp.dat$ModelPred<-temp.dat$ModelPred*1000
	temp.dat$Upper95<-temp.dat$Upper95*1000
	temp.dat$Lower95<-temp.dat$Lower95*1000

	temp.res<-as.character(temp.dat$Result)
	temp.res<-gsub("ND<","",temp.res)
	temp.res[tolower(temp.res)!="napl"]<-as.character(as.numeric(temp.res[tolower(temp.res)!="napl"])*1000)
	temp.res[grep("ND<",temp.dat$Result)]<-paste("ND<",temp.res[grep("ND<",temp.dat$Result)],sep="")
	temp.dat$Result<-temp.res
	try(temp.dat$ModelPred<-round(temp.dat$ModelPred,0))
	try(temp.dat$Upper95<-round(temp.dat$Upper95,0))
  	try(temp.dat$Lower95<-round(temp.dat$Lower95,0))	
  
	temp.dat$Units<-rep("ng/l",nrow(temp.dat))
	rm(temp.res)


  }else{

  try(temp.dat$ModelPred<-round(temp.dat$ModelPred,1))
  try(temp.dat$Upper95<-round(temp.dat$Upper95,1))
  try(temp.dat$Lower95<-round(temp.dat$Lower95,1))	
  temp.dat$Units<-rep("ug/l",nrow(temp.dat))
  
  }
  names(temp.dat)[names(temp.dat)=="ModelPred"]<-"Model_Pred"
  #Alternative with SEs
  temp.dat<-temp.dat[,setdiff(names(temp.dat),c("Lower95","Upper95"))]

  }



  if(!is.null(temp.dat) &&  nrow(temp.dat)>0){

	mytmpfile<-NULL
	mytmpfile<-dirname(tempfile())
	mytmpfile<-paste(mytmpfile,paste("Model Diagnostic Table ",format(Sys.time(), " %Y-%b-%d %H-%M-%S"),".csv",sep=""),sep="/")
	try(write.csv(temp.dat,file=mytmpfile,row.names=FALSE))
	try(shell.exec(mytmpfile))

  }

return(panel)
}
#------------- End Model Diagnostic Table -----------------------------------------------------#




####################### GWSDAT Model Diagnostic main plot function #############################

GWSDAT.Model.Diag.Plot<-function(myrpanel,Cont.Data,SiteName="",se.fit=FALSE,UseLogScale=FALSE){

if(myrpanel$rgUnits=="mg/l"){Cont.Data$Result.Corr.ND<-Cont.Data$Result.Corr.ND/1000}
if(myrpanel$rgUnits=="ng/l"){Cont.Data$Result.Corr.ND<-Cont.Data$Result.Corr.ND*1000}


require(lattice)
NAPL.Present<-any(tolower(as.character(na.omit(Cont.Data$Result)))=="napl")
Cont<-as.character(unique(Cont.Data$Constituent))


my.xlim<-range(c(myrpanel$Cont.Data$SampleDate,myrpanel$All.Data$GW.Data$SampleDate)) 
my.xlim.orig=my.xlim
my.xlim[1]<-my.xlim.orig[1]-0.025*as.numeric(diff(my.xlim.orig))
my.xlim[2]<-my.xlim.orig[2]+0.025*as.numeric(diff(my.xlim.orig))


my.ylim<-range(myrpanel$Fitted.Data[[Cont]]$Cont.Data[,c("ModelPred","Upper95","Lower95")],na.rm=T)
my.ylim.orig=my.ylim
#my.ylim[1]<-my.ylim.orig[1]-0.025*as.numeric(diff(my.ylim.orig))
#my.ylim[2]<-my.ylim.orig[2]+0.025*as.numeric(diff(my.ylim.orig))
if(myrpanel$rgUnits=="mg/l"){my.ylim<-my.ylim/1000}
if(myrpanel$rgUnits=="ng/l"){my.ylim<-my.ylim*1000}

my.key <- list( 
        space = "top", 
        border = FALSE, 
	columns=3,
	lines=list(
	pch=c(0,19,19),lty=c(1,1,1),cex = rep(1.4,3), lwd=c(3),col = c("grey","black","orange"), type = c("l","p", "p")
	),
        text = list( 
                lab=c("Spatiotemporal Prediction","Detectable Data","Non-Detect Data")
        ) 
) 

if(NAPL.Present){

my.key <- list( 
        space = "top", 
        border = FALSE, 
	columns=3,
	lines=list(
	pch=c(0,19,19,19),lty=c(1,1,1,1),cex = rep(1.4,4), lwd=c(3),col = c("grey","black","orange","red"), type = c("l","p", "p","p")
	),
        text = list( 
                lab=c("Spatiotemporal Prediction","Detectable Data","Non-Detect Data","NAPL Substituted Data")
        ) 
) 

Cont.Data$ND<-as.character(Cont.Data$ND)
Cont.Data$ND[tolower(as.character(Cont.Data$Result))=="napl"]<-"NAPL"
}


my.plot<-xyplot(Result.Corr.ND~as.Date(SampleDate)|WellName,data=Cont.Data,groups=as.character(Cont.Data$ND),

panel = function(x, y,groups,subscripts) {
try(panel.grid(h=-1, v= 2))
groupNDx<-x[groups[subscripts]=="TRUE"]
groupNDy<-y[groups[subscripts]=="TRUE"]
panel.xyplot(groupNDx,groupNDy,col="orange",pch=19,cex=1.0)


groupx<-x[groups[subscripts]=="FALSE"]
groupy<-y[groups[subscripts]=="FALSE"]

panel.xyplot(groupx,groupy,col="black",pch=19,cex=1.0)


groupNAPLx<-x[groups[subscripts]=="NAPL"]
groupNAPLy<-y[groups[subscripts]=="NAPL"]

if(length(groupNAPLx)>0){panel.xyplot(groupNAPLx,groupNAPLy,col="red",pch=19,cex=1.0)}


		#if(sm.fit && length(x)>1){

		if(length(x)>1){

		Model<-myrpanel$Fitted.Data[[as.character(Cont)]]$Model.tune

		if(!inherits(Model,"try-error")){
		
				
		Model<-Model$best.model
		eval.df<-data.frame(AggDate=seq(min(x,na.rm=T),max(x,na.rm=T),l=50),XCoord=rep(Cont.Data[subscripts,"XCoord"][1],50),YCoord=rep(Cont.Data[subscripts,"YCoord"][1],50))

		pred<-predict(Model,eval.df,se=se.fit) ###
		eval.df$pred<-pred$predicted
		
		if(se.fit){
		eval.df$upper<-pred$predicted+pred$predicted.sd*1.96
		eval.df$lower<-pred$predicted-pred$predicted.sd*1.96
		}

		if(myrpanel$rgUnits=="mg/l"){

		eval.df$pred <-log(exp(eval.df$pred)/1000)
		
		 if(se.fit){
		 eval.df$upper<-log(exp(eval.df$upper)/1000)
		 eval.df$lower<-log(exp(eval.df$lower)/1000)
		 }

		}

		if(myrpanel$rgUnits=="ng/l"){

		eval.df$pred <-log(exp(eval.df$pred)*1000)

		 if(se.fit){
		 eval.df$upper<-log(exp(eval.df$upper)*1000)
		 eval.df$lower<-log(exp(eval.df$lower)*1000)
		 }

		}


		if(UseLogScale){
		
		eval.df$pred <-log(exp(eval.df$pred),base=10)
		 if(se.fit){
		 eval.df$upper<-log(exp(eval.df$upper),base=10)
		 eval.df$lower<-log(exp(eval.df$lower),base=10)
		 }

		}else{

		eval.df$pred <-exp(eval.df$pred)
		
		 if(se.fit){
		 eval.df$upper<-exp(eval.df$upper)
		 eval.df$lower<-exp(eval.df$lower)
		 }

		}
		
		panel.xyplot(as.Date(eval.df$AggDate),eval.df$pred,type="l",col="grey",lwd=3)	
 		 if(se.fit){
		 panel.xyplot(as.Date(eval.df$AggDate),eval.df$upper,type="l",lty=2,col="grey",lwd=2)	
		 panel.xyplot(as.Date(eval.df$AggDate),eval.df$lower,type="l",lty=2,col="grey",lwd=2)	
		 }

		}
		}
		
            },


scales=list(y=list(log=UseLogScale)),
xlab=list("Sampling Date",cex=1.5),ylab=list(paste("Solute concentration"," (",myrpanel$rgUnits,")",sep=""),cex=1.5),
layout=if(length(levels(Cont.Data$Well))>30){c(4,4)}else{NULL},xlim=my.xlim,#ylim=my.ylim,
main=if(myrpanel$All.Data$Aq.sel==""){paste("Spatiotemporal Predictions for ",Cont,"at",SiteName)}else{paste("Spatiotemporal Predictions for ",Cont," at ",SiteName,": Aquifer-",myrpanel$All.Data$Aq.sel,sep="")},
drop.unused.levels=FALSE,key=my.key) 

return(my.plot)

}
#--------------------------------------------------------------------------------------------------------------------------#














###################### Model Diagnostic PLot ###############################################################################

Plot.Model.Diag<-function(panel,toPPT=FALSE){

on.exit(palette("default"))
require(lattice)
Cont.Data<-panel$All.Data$Cont.Data

Cont.Data<-Cont.Data[!is.na(Cont.Data$XCoord),] #No model prediction possible without well coords!
Cont.Data<-Cont.Data[!is.na(Cont.Data$YCoord),]

All.Conts<-panel$All.Data$All.Conts


All.Wells<-sort(as.character(unique(Cont.Data$WellName)))

SiteName<-panel$GWSDAT_Options$SiteName


if(length(All.Conts)==1){Conts.to.plot<-All.Conts[1]}else{Conts.to.plot<-GWSDAT.select.list(All.Conts,multiple =FALSE,title = "Select Solute to Plot")}


if(length(Conts.to.plot)==0){stop("No Solutes selected!")}
Cont.Data<-Cont.Data[as.character(Cont.Data$Constituent) %in% Conts.to.plot,]
Cont.Data$Constituent<-factor(as.character(Cont.Data$Constituent))


Wells.to.Plot<-GWSDAT.select.list(All.Wells,multiple =T,title = "Select Wells to Plot",preselect=All.Wells)
if(length(Wells.to.Plot)==0){stop("No Wells selected!")}
Cont.Data<-Cont.Data[as.character(Cont.Data$WellName) %in% Wells.to.Plot,]

if(nrow(Cont.Data)==0){rp.messagebox("No Data to Plot!", title = "Error"); stop("No Data to Plot")}

Cont.Data$WellName<-factor(as.character(Cont.Data$WellName),levels=sort(Wells.to.Plot))
Cont.Data<-Cont.Data[order(Cont.Data$SampleDate),]

UseLogScale<-GWSDAT.select.list(c("No","Yes"),multiple =F,title = "Use Log Scale?",preselect="Yes")
if(length(UseLogScale)==0){stop("No Choice of LogScale selected!")}
if(UseLogScale=="No"){UseLogScale=FALSE}else{UseLogScale=TRUE}


myplot<-GWSDAT.Model.Diag.Plot(panel,Cont.Data,SiteName=SiteName,se.fit=FALSE & panel$dlines["Conc. Trend Smoother"],UseLogScale=UseLogScale)


	if(!toPPT){

		.SavedPlots<<-NULL
		graphics.off()
		windows(record=T,width =11, height = 9)
		print(myplot)

	}else{
		try(file.remove(list.files(dirname(tempfile()),pattern="Wellgraph",full.names=T)))
		OutputGraphics <- panel$GWSDAT_Options$OutputGraphics
		if(is.null(OutputGraphics)){OutputGraphics="wmf"}
		if(OutputGraphics=="jpeg"){jpeg(file=paste(dirname(tempfile()),"/","Wellgraph%03d.jpeg",sep=""), width=960, height=960, quality=100)}
		if(OutputGraphics=="png") {png(file=paste(dirname(tempfile()),"/","Wellgraph%03d.png",sep=""), width=960, height=960)}
		if(OutputGraphics=="wmf") {win.metafile(file=paste(dirname(tempfile()),"/","Wellgraph%03d.emf",sep=""),width = 9, height =7) }
		
		print(myplot)
		dev.off()
		AddPlotPPfromFilesV2(sort(list.files(dirname(tempfile()),pattern="Wellgraph",full.names=T)))
		try(file.remove(list.files(dirname(tempfile()),pattern="Wellgraph",full.names=T)))

	}


return(panel)
}
#------------------------------- End Model Diagnostic Plot -----------------------------------------------------#

























################################################### Save Session ####################################################################
SaveGWSDATSession<-function(panel){


fileName<-tclvalue(tkgetSaveFile(initialfile=GWSDAT_Options$SiteName,filetypes="{{GWSDATData Files} {.GWSDATData}}",defaultextension=".GWSDATData"))

if(!nchar(fileName)){return(panel)}

attr(Curr.Site.Data,'Default.panel.Values')<-list(Porosity=panel$Porosity,ContLimEntry=panel$ContLimEntry,PlumeLimEntry=panel$PlumeLimEntry,jjj=panel$jjj,shadow.jjj=panel$shadow.jjj,
Well=panel$Well,Cont.rg=panel$Cont.rg,dlines=panel$dlines,Color.type=panel$Color.type,GW.disp=panel$GW.disp,ScaleCols=panel$ScaleCols,rg1=panel$rg1,rgUnits=panel$rgUnits)



for(i in names(Curr.Site.Data$Fitted.Data)){

if(!inherits(Curr.Site.Data$Fitted.Data[[i]]$Model.tune,"try-error")){Curr.Site.Data$Fitted.Data[[i]]$Model.tune$best.model$Xtinv<-NULL}

}

save(list="Curr.Site.Data",file=fileName)

return(panel)
}
#--------------------------------------------------- End Save Session -------------------------------------------------------------#







































################################################### Animations ####################################################################
Make.Moviedoublebutton<-function(panel){

	#my.jjj<-panel$shadow.jjj%%length(panel$All.Data$All.Agg.Dates)
	#if(my.jjj==0){my.jjj=length(panel$All.Data$All.Agg.Dates)}
	#panel$jjj=my.jjj
	redraw(panel)
	return(panel)
}



Make.Movie<-function(panel,addtoPPt=FALSE){

keep.jjj<-panel$jjj
graphics.off()
.SavedPlots<<-NULL
windows(record=T,width =11, height = 8)
try(bringToTop())

for(i in 1:length(panel$All.Data$All.Agg.Dates)){
	panel$jjj=i
	try(MakeImagePlotNoPlot(panel,FALSE))
	if(addtoPPt){AddPlotPPV2(panel,asp=TRUE)}

}

panel$jjj=keep.jjj
return(panel)

}






####### Plume Unit Handling Function #####################################

PlumeUnitHandlingFunc<-function(LengthUnit,rgUnits,PlumeMass,PlumeArea){

if(is.null(LengthUnit)){

PlumeMass<-1000*PlumeMass
if(rgUnits=="ng/l"){PlumeMass<-PlumeMass*10^-12}
if(rgUnits=="ug/l"){PlumeMass<-PlumeMass*10^-9}
if(rgUnits=="mg/l"){PlumeMass<-PlumeMass*10^-6}
PlumeMassUnits<-paste(" (Mass/Unit Depth)",sep="")
PlumeAreaUnits<-paste(" (Unit Area)",sep="")
#PlumeAverageUnits<-paste(" (Mass/Unit Volume)",sep="")
PlumeAverageUnits<-paste("(",rgUnits,")",sep="")


}else{

 if(LengthUnit=="metres"){


  PlumeMass<-1000*PlumeMass
  if(rgUnits=="ng/l"){PlumeMass<-PlumeMass*10^-12}
  if(rgUnits=="ug/l"){PlumeMass<-PlumeMass*10^-9}
  if(rgUnits=="mg/l"){PlumeMass<-PlumeMass*10^-6}
  PlumeMassUnits<-paste(" (kg/m)",sep="")
  PlumeAreaUnits<-paste(" (m^2)",sep="")
  #PlumeAverageUnits<-paste(" (kg/m^3)",sep="")
  PlumeAverageUnits<-paste("(",rgUnits,")",sep="")

 }


 if(LengthUnit=="feet"){

  PlumeMass<-1000*PlumeMass/35.315 #per cubic ft
  if(rgUnits=="ng/l"){PlumeMass<-PlumeMass*10^-12}
  if(rgUnits=="ug/l"){PlumeMass<-PlumeMass*10^-9}
  if(rgUnits=="mg/l"){PlumeMass<-PlumeMass*10^-6}
  PlumeMassUnits<-paste(" (kg/ft)",sep="")
  PlumeAreaUnits<-paste(" (ft^2)",sep="")
  #PlumeAverageUnits<-paste(" (kg/ft^3)",sep="")
  PlumeAverageUnits<-paste("(",rgUnits,")",sep="")

 }

}

return(list(PlumeMass=PlumeMass,PlumeArea=PlumeArea,PlumeMassUnits=PlumeMassUnits,PlumeAreaUnits=PlumeAreaUnits,PlumeAverageUnits=PlumeAverageUnits))

}

#------------------------------------------------------------------------#





Make.Plume.Movie<-function(panel,addtoPPt=FALSE){



keep.jjj<-panel$jjj
keep.Plume.Details<-panel$ScaleCols["Plume Diagnostics?"]
panel$ScaleCols["Plume Diagnostics?"]<-TRUE
#PlumeLengthUnit<-GWSDAT.select.list(c("dimensionless","metres","feet"),title = "Select Plume units:")
#if(PlumeLengthUnit==""){return(panel)}

graphics.off()
.SavedPlots<<-NULL
windows(record=T,width =11, height = 8)
try(bringToTop())


PlumeStats.df<-data.frame(Agg.Date=panel$All.Data$All.Agg.Dates)
PlumeStats.df$PlumeMass<-PlumeStats.df$PlumeArea<-PlumeStats.df$COMx<-PlumeStats.df$COMy<-PlumeStats.df$PlumeAverageConc<-rep(NA,nrow(PlumeStats.df))

for(i in 1:length(panel$All.Data$All.Agg.Dates)){

	panel$jjj=i
	try(panel<-MakeImagePlotNoPlot(panel,FALSE))
	PlumeStats.df$PlumeArea[i]<-panel$tempTotalPlumeDetails$area
	PlumeStats.df$PlumeMass[i]<-panel$tempTotalPlumeDetails$Mass
	PlumeStats.df$COMx[i]<-panel$tempTotalPlumeDetails$PlumeCentreofMass[1]
	PlumeStats.df$COMy[i]<-panel$tempTotalPlumeDetails$PlumeCentreofMass[2]
	PlumeStats.df$PlumeAverageConc[i]<-panel$tempTotalPlumeDetails$volume/panel$tempTotalPlumeDetails$area


	if(addtoPPt){AddPlotPPV2(panel,asp=TRUE)}

}

if(!all(is.na(PlumeStats.df$PlumeMass))){



tempUnitHandle<-PlumeUnitHandlingFunc(GWSDAT_Options$WellCoordsLengthUnits,panel$rgUnits,PlumeStats.df$PlumeMass,PlumeStats.df$PlumeArea)
PlumeStats.df$PlumeMass<-tempUnitHandle$PlumeMass
PlumeStats.df$PlumeArea<-tempUnitHandle$PlumeArea
#PlumeStats.df$PlumeAverageConc<-tempUnitHandle$PlumeMass/tempUnitHandle$PlumeArea

par(mfrow=c(1,3),oma=c(1.5,0,0,0))


####################### Plume Mass #######################################################################################################

my.ylim<-range(PlumeStats.df$PlumeMass,na.rm=T)

lmPlumeMass<-try(lm(PlumeMass~Agg.Date,PlumeStats.df,na.action=na.omit))

if(!inherits(lmPlumeMass,"try-error")){

lm.eval.points<-na.omit(PlumeStats.df[,c("Agg.Date","PlumeMass")])
lm.eval.points<-data.frame(Agg.Date=as.Date(seq(range(lm.eval.points$Agg.Date)[1],range(lm.eval.points$Agg.Date)[2],length=100)))
lm.pred<-predict(lmPlumeMass,newdata=lm.eval.points,interval="confidence")

lm.eval.points$fit<-lm.pred[,"fit"]
lm.eval.points$lwr<-lm.pred[,"lwr"]
lm.eval.points$upr<-lm.pred[,"upr"]


my.ylim<-c(min(c(lm.eval.points$lwr,my.ylim),na.rm=T),max(c(lm.eval.points$upr,my.ylim),na.rm=T))
my.ylim[1]<-max(my.ylim[1],0)

}

#my.ylab<-GWSDAT_Options$WellCoordsLengthUnits #"Mass"
my.ylab<-paste("Plume Mass",tempUnitHandle$PlumeMassUnits,sep="")

try(plot(PlumeMass~Agg.Date,PlumeStats.df,ylim=my.ylim,cex.main=1.3,type="b",
main= paste(panel$GWSDAT_Options$SiteName,"\n Plume Mass: ",panel$Cont.rg,sep=""),xlab="Date",cex.lab=1.4,pch=19,cex=1.5,ylab=my.ylab))
try(mtext(paste("Plume Threshold Conc =",as.numeric(panel$PlumeLimEntry[panel$Cont.rg]),"ug/l",sep=""),side=3,line=-1,cex=0.75))

if(!inherits(lmPlumeMass,"try-error")){

#try(lines(lm.eval.points$Agg.Date,lm.eval.points$fit,lwd=2,col="green"))
#try(lines(lm.eval.points$Agg.Date,lm.eval.points$lwr,lwd=2,col="green",lty=2))
#try(lines(lm.eval.points$Agg.Date,lm.eval.points$upr,lwd=2,col="green",lty=2))


}

Mann.testPlumeMass<-try(Kendall(PlumeStats.df$Agg.Date,PlumeStats.df$PlumeMass))

if(!inherits(Mann.testPlumeMass,"try-error")){

temp.tex1<-paste("(Mann-Kendall P.Value=",format.pval(Mann.testPlumeMass$sl,digits=3,eps=0.01),")",sep="")
#try(mtext(temp.tex1,side=3,line=0.4,cex=0.75,col=if(Mann.testPlumeMass$sl<0.05 & Mann.testPlumeMass$tau<0.0){"darkgreen"}else{"red"}))
#try(mtext(temp.tex1,side=3,line=0.4,cex=0.75,col=if(Mann.testPlumeMass$sl<0.05){"darkgreen"}else{"red"}))

}

#----------------------------------------------------------------------------------------------------------------------------------------#


####################### Plume Area #######################################################################################################

my.ylim<-range(PlumeStats.df$PlumeArea,na.rm=T)

lmPlumeArea<-try(lm(PlumeArea~Agg.Date,PlumeStats.df,na.action=na.omit))

if(!inherits(lmPlumeArea,"try-error")){

lm.eval.points<-na.omit(PlumeStats.df[,c("Agg.Date","PlumeArea")])
lm.eval.points<-data.frame(Agg.Date=as.Date(seq(range(lm.eval.points$Agg.Date)[1],range(lm.eval.points$Agg.Date)[2],length=100)))
lm.pred<-predict(lmPlumeArea,newdata=lm.eval.points,interval="confidence")

lm.eval.points$fit<-lm.pred[,"fit"]
lm.eval.points$lwr<-lm.pred[,"lwr"]
lm.eval.points$upr<-lm.pred[,"upr"]


my.ylim<-c(min(c(lm.eval.points$lwr,my.ylim),na.rm=T),max(c(lm.eval.points$upr,my.ylim),na.rm=T))
my.ylim[1]<-max(my.ylim[1],0)

}
my.ylab<-paste("Plume Area",tempUnitHandle$PlumeAreaUnits,sep="")



try(plot(PlumeArea~Agg.Date,PlumeStats.df,ylim=my.ylim,cex.main=1.3,type="b",
main= paste(panel$GWSDAT_Options$SiteName,"\n Plume Area: ",panel$Cont.rg,sep=""),xlab="Date",cex.lab=1.4,pch=19,cex=1.5,ylab=my.ylab))
try(mtext(paste("Plume Threshold Conc =",as.numeric(panel$PlumeLimEntry[panel$Cont.rg]),"ug/l",sep=""),side=3,line=-1,cex=0.75))

if(!inherits(lmPlumeArea,"try-error")){

#try(lines(lm.eval.points$Agg.Date,lm.eval.points$fit,lwd=2,col="green"))
#try(lines(lm.eval.points$Agg.Date,lm.eval.points$lwr,lwd=2,col="green",lty=2))
#try(lines(lm.eval.points$Agg.Date,lm.eval.points$upr,lwd=2,col="green",lty=2))


}

Mann.testPlumeArea<-try(Kendall(PlumeStats.df$Agg.Date,PlumeStats.df$PlumeArea))

if(!inherits(Mann.testPlumeArea,"try-error")){

temp.tex1<-paste("(Mann-Kendall P.Value=",format.pval(Mann.testPlumeArea$sl,digits=3,eps=0.01),")",sep="")
#try(mtext(temp.tex1,side=3,line=0.4,cex=0.75,col=if(Mann.testPlumeArea$sl<0.05 & Mann.testPlumeArea$tau<0.0){"darkgreen"}else{"red"}))
#try(mtext(temp.tex1,side=3,line=0.4,cex=0.75,col=if(Mann.testPlumeArea$sl<0.05){"darkgreen"}else{"red"}))

}


#----------------------------------------------------------------------------------------------------------------------------------------#



####################### Plume Average ####################################################################################################

my.ylim<-range(PlumeStats.df$PlumeAverageConc,na.rm=T)

lmPlumeAverageConc<-try(lm(PlumeAverageConc~Agg.Date,PlumeStats.df,na.action=na.omit))

if(!inherits(lmPlumeAverageConc,"try-error")){

lm.eval.points<-na.omit(PlumeStats.df[,c("Agg.Date","PlumeAverageConc")])
lm.eval.points<-data.frame(Agg.Date=as.Date(seq(range(lm.eval.points$Agg.Date)[1],range(lm.eval.points$Agg.Date)[2],length=100)))
lm.pred<-predict(lmPlumeAverageConc,newdata=lm.eval.points,interval="confidence")

lm.eval.points$fit<-lm.pred[,"fit"]
lm.eval.points$lwr<-lm.pred[,"lwr"]
lm.eval.points$upr<-lm.pred[,"upr"]


my.ylim<-c(min(c(lm.eval.points$lwr,my.ylim),na.rm=T),max(c(lm.eval.points$upr,my.ylim),na.rm=T))
my.ylim[1]<-max(my.ylim[1],0)

}
my.ylab<-paste("Concentration",tempUnitHandle$PlumeAverageUnits,sep="")

try(plot(PlumeAverageConc~Agg.Date,PlumeStats.df,ylim=my.ylim,cex.main=1.3,type="b",
main= paste(panel$GWSDAT_Options$SiteName,"\n Average Plume Concentration: ",panel$Cont.rg,sep=""),xlab="Date",cex.lab=1.4,pch=19,
cex=1.5,ylab=my.ylab))

try(mtext(paste("Plume Threshold Conc =",as.numeric(panel$PlumeLimEntry[panel$Cont.rg]),"ug/l",sep=""),side=3,line=-1,cex=0.75))

if(!inherits(lmPlumeAverageConc,"try-error")){

#try(lines(lm.eval.points$Agg.Date,lm.eval.points$fit,lwd=2,col="green"))
#try(lines(lm.eval.points$Agg.Date,lm.eval.points$lwr,lwd=2,col="green",lty=2))
#try(lines(lm.eval.points$Agg.Date,lm.eval.points$upr,lwd=2,col="green",lty=2))


}

Mann.testPlumeAverageConc<-try(Kendall(PlumeStats.df$Agg.Date,PlumeStats.df$PlumeAverageConc))

if(!inherits(Mann.testPlumeAverageConc,"try-error")){

temp.tex1<-paste("(Mann-Kendall P.Value=",format.pval(Mann.testPlumeAverageConc$sl,digits=3,eps=0.01),")",sep="")
#try(mtext(temp.tex1,side=3,line=0.4,cex=0.75,col=if(Mann.testPlumeAverageConc$sl<0.05 & Mann.testPlumeAverageConc$tau<0.0){"darkgreen"}else{"red"}))
#try(mtext(temp.tex1,side=3,line=0.4,cex=0.75,col=if(Mann.testPlumeAverageConc$sl<0.05){"darkgreen"}else{"red"}))

}


#----------------------------------------------------------------------------------------------------------------------------------------#


if(addtoPPt){AddPlotPPV2(panel,asp=TRUE)}




if( tclvalue(tkmessageBox(message = "Do you want to output Plume statistics to Excel csv file?",icon = "question", type = "yesno", default = "yes"))=="yes"){

names(PlumeStats.df)[names(PlumeStats.df)=="PlumeAverageConc"]<-paste("PlumeAverageConc",tempUnitHandle$PlumeAverageUnits,sep="")
names(PlumeStats.df)[names(PlumeStats.df)=="PlumeArea"]<-paste("PlumeArea",tempUnitHandle$PlumeAreaUnits,sep="")
names(PlumeStats.df)[names(PlumeStats.df)=="PlumeMass"]<-paste("PlumeMass",tempUnitHandle$PlumeMassUnits,sep="")
PlumeStats.df[,"Plume Threshold Conc (ug/l)"]<-rep("",nrow=PlumeStats.df)
PlumeStats.df[1,"Plume Threshold Conc (ug/l)"]<-as.numeric(panel$PlumeLimEntry[panel$Cont.rg])

	mytmpfile<-NULL
	mytmpfile<-dirname(tempfile())
	mytmpfile<-paste(mytmpfile,paste(panel$GWSDAT_Options$SiteName,"-",panel$Cont.rg,"-Plume Diagnostic Table ",format(Sys.time(), " %Y-%b-%d %H-%M-%S"),".csv",sep=""),sep="/")
	try(write.csv(PlumeStats.df,file=mytmpfile,row.names=FALSE))
	try(shell.exec(mytmpfile))

}

}else{

tkmessageBox(title="Error!",message=paste("Unable to calculate plume statistics for threshold value=",as.numeric(panel$PlumeLimEntry[panel$Cont.rg]),"ug/l.","\nUse the 'Estimate Plume Boundary' function for assistance in selecting a suitable plume threshold concentration value."),icon="error",type="ok")

}







panel$jjj=keep.jjj
panel$ScaleCols["Plume Diagnostics?"]<-keep.Plume.Details
return(panel)

}






Make.Animation<-function(panel){

keep.jjj=panel$jjj

oopt = ani.options(interval = 0.5, nmax = length(panel$All.Data$All.Agg.Dates), 
ani.dev = png, ani.type = "png",ani.width=680,ani.height=680,
title="Plume Animation",description="",autobrowse=TRUE)

ani.start()
for(i in 1:length(panel$All.Data$All.Agg.Dates)){

	panel$jjj=i
	MakeImagePlotNoPlot(panel,FALSE)
}

ani.stop()

ani.options(oopt)

panel$jjj=keep.jjj

return(panel)
}



Latest.Snapshot<-function(panel,addtoPPt=FALSE){

Cont.Names<-panel$Cont.Names
keep.Cont<-panel$Cont.rg
keep.jjj<-panel$jjj
keep.rg1<-panel$rg1
keep.ColTrafficListbox<-panel$ColTrafficListbox

graphics.off()
.SavedPlots<<-NULL
windows(record=T,width =11, height = 8)
try(bringToTop())


panel$jjj=length(panel$All.Data$All.Agg.Dates)
if(!is.null(panel$ColTrafficListbox)){panel$ColTrafficListbox<-"All"}


for(i in Cont.Names){

	panel$Cont.rg=i
	try(MakeImagePlotNoPlot(panel,FALSE))
	if(addtoPPt){AddPlotPPV2(panel,asp=TRUE)}

}

	panel$rg1="Trend"
	try(Make.Traffic.Plot(panel,fromDoubleButton=FALSE))
	if(addtoPPt){AddPlotPPV2(panel)}


	panel$rg1="Threshold - Absolute"
	try(Make.Traffic.Plot(panel,fromDoubleButton=FALSE))
	if(addtoPPt){AddPlotPPV2(panel)}

	panel$rg1="Threshold - Statistical"
	try(Make.Traffic.Plot(panel,fromDoubleButton=FALSE))
	if(addtoPPt){AddPlotPPV2(panel)}
	



panel$Cont.rg<-keep.Cont
panel$jjj<-keep.jjj
panel$rg1<-keep.rg1
panel$ColTrafficListbox<-keep.ColTrafficListbox

return(panel)

}

#--------------------------------------------------- End Make Movie ---------------------------------------------------------#

















































################################################### Build Panel ##############################################################
do.menu<-function(panel){

if(panel$menuchoice=="Save Session"){panel<-SaveGWSDATSession(panel)}
if(panel$menuchoice=="Current Plot -> PPT"){panel<-AddPlotPPV2(panel)}
if(panel$menuchoice=="Current Plot -> WORD"){panel<-AddPlotWORDV2(panel)}
if(panel$menuchoice=="Contour Animation"){panel<-Make.Movie(panel)}
if(panel$menuchoice=="Contour Animation -> PPT"){panel<-Make.Movie(panel,TRUE)}
if(panel$menuchoice=="Contour Animation -> HTML"){panel<-Make.Animation(panel)}
if(panel$menuchoice=="Latest Snapshot"){panel<-Latest.Snapshot(panel,FALSE)}
if(panel$menuchoice=="Latest Snapshot -> PPT"){panel<-Latest.Snapshot(panel,TRUE)}
if(panel$menuchoice=="Well Reporting"){panel<-WellReport(panel,toPPT=FALSE)}
if(panel$menuchoice=="Well Reporting -> PPT"){panel<-WellReport(panel,toPPT=TRUE)}
if(panel$menuchoice=="GW Well Reporting"){panel<-GWWellReport(panel,toPPT=FALSE)}
if(panel$menuchoice=="GW Well Reporting -> PPT"){panel<-GWWellReport(panel,toPPT=TRUE)}
if(panel$menuchoice=="Output Predictions to Table"){panel<-Model.Diag(panel)}
if(panel$menuchoice=="Spatiotemporal Diagnostic Plot"){panel<-Plot.Model.Diag(panel)}
if(panel$menuchoice=="Spatiotemporal Diagnostic Plot -> PPT"){panel<-Plot.Model.Diag(panel,TRUE)}
if(panel$menuchoice=="Edit Plume Thresholds"){panel<-Edit.Plume.Lims(panel)}
if(panel$menuchoice=="Estimate Plume Boundary"){panel<-TunePlumeQuant(panel)}
if(panel$menuchoice=="Set Ground Porosity"){panel<-Edit.Porosity(panel)}
if(panel$menuchoice=="Plume Animation"){panel<-Make.Plume.Movie(panel,addtoPPt=FALSE)}
if(panel$menuchoice=="Plume Animation -> PPT"){panel<-Make.Plume.Movie(panel,addtoPPt=TRUE)}

return(panel)
}

NAPLThickPresent<-!is.null(All.Data$NAPL.Thickness.Data)
GWFlowsPresent<-"GWFlows" %in% names(attributes(Fitted.Data))


if(!is.null(attributes(Curr.Site.Data)$Default.panel.Values)){
Use.Defaults<-TRUE
Default.Values<-attributes(Curr.Site.Data)$Default.panel.Values
}else{Use.Defaults<-FALSE}

my.lev.cut <- c(0,5,10,25,50,75,100,200, 400, 800, 1500, 3000, 5000, 5000000)
sd.lev.cut<-100*c(seq(0,3,by=0.25),10000000)
#sd.lev.cut<-my.lev.cut


if(Use.Defaults && !is.null(Default.Values$ContLimEntry)){ContLimEntry<-Default.Values$ContLimEntry}else{
ContLimEntry<-as.character(rep(GWSDAT_Options$DefContThresh,Num.Conts)); names(ContLimEntry)<-Cont.Names}

if(Use.Defaults && !is.null(Default.Values$PlumeLimEntry)){PlumeLimEntry<-Default.Values$PlumeLimEntry}else{
PlumeLimEntry<-as.character(rep(GWSDAT_Options$DefPlumeThresh,Num.Conts)); names(PlumeLimEntry)<-Cont.Names}

if(Use.Defaults && !is.null(Default.Values$Porosity)){
Porosity<-Default.Values$Porosity
}else{
Porosity<-as.character(GWSDAT_Options$DefPorosity)
}


Scale.panelImage=list(hscale=GWSDAT_Options$Scale.panelImage$hscale,vscale=GWSDAT_Options$Scale.panelImage$vscale)

GWSDAT_Options$SiteName<-paste("",GWSDAT_Options$SiteName,sep="")
GWSDATpnl <- rp.control(if(All.Data$Aq.sel==""){GWSDAT_Options$SiteName}else{paste(GWSDAT_Options$SiteName,": Aquifer-",All.Data$Aq.sel,sep="")},
jjj=if(Use.Defaults && !is.null(Default.Values$jjj)){Default.Values$jjj}else{length(All.Data$All.Agg.Dates)},
Cont.Data=All.Data$Cont.Data,Well=if(Use.Defaults && !is.null(Default.Values$Well)){Default.Values$Well}else{sort(All.Data$All.Wells)[1]},
All.Dates=All.Data$All.Dates,All.Agg.Dates=All.Data$All.Agg.Dates,Cont.Names=Cont.Names,All.Data=All.Data,Fitted.Data=Fitted.Data,
Scale.panelImage=Scale.panelImage,Traffic.Lights=attr(Fitted.Data,"TrafficLights"),lev.cut=my.lev.cut,sd.lev.cut=sd.lev.cut,GWSDAT_Options=GWSDAT_Options,
ContLimEntry=ContLimEntry,PlumeLimEntry=PlumeLimEntry,Porosity=Porosity,tempTotalPlumeDetails=NULL,panelname="GWSDATpnl")

UseCombo<-F

#rp.grid(GWSDATpnl,            "ControlsGrid", row = 0, column = 0, columnspan = 1, rowspan = 1,sticky="")
#rp.grid(GWSDATpnl,     "ContourControlsGrid", row = 0, column = 2, columnspan = 1, rowspan = 1,sticky="")
#rp.grid(GWSDATpnl,"TrafficLightControlsGrid", row = 1, column = 2, columnspan = 1, rowspan = 1,sticky="")
#rp.grid(GWSDATpnl,                "topright", row = 0, column = 1, columnspan = 1, rowspan = 1,sticky="")
 rp.grid(GWSDATpnl,            "ControlsGrid", row = 1, column = 1, columnspan = 1, rowspan = 1,sticky="")
 rp.grid(GWSDATpnl,     "ContourControlsGrid", row = 1, column = 3, columnspan = 1, rowspan = 1,sticky="")
 rp.grid(GWSDATpnl,"TrafficLightControlsGrid", row = 2, column = 3, columnspan = 1, rowspan = 1,sticky="")
 rp.grid(GWSDATpnl,                "topright", row = 1, column = 2, columnspan = 1, rowspan = 1,sticky="")


listAnimations<-list("Animations","Contour Animation","Contour Animation -> PPT","Contour Animation -> HTML")
listReportGeneration<-list("Report Generation","Current Plot -> PPT","Current Plot -> WORD","Latest Snapshot","Latest Snapshot -> PPT","Well Reporting","Well Reporting -> PPT","GW Well Reporting","GW Well Reporting -> PPT")
listSTDiagnostics<-list("Spatiotemporal Diagnostics","Output Predictions to Table","Spatiotemporal Diagnostic Plot","Spatiotemporal Diagnostic Plot -> PPT")
listPlumeiagnostics<-list("Plume Diagnostics","Edit Plume Thresholds","Set Ground Porosity","Estimate Plume Boundary","Plume Animation","Plume Animation -> PPT")

if(!require(RDCOMClient)){ ## Hide PowerPoint and Word Options if RDCOMClient unavailable. 

listAnimations<-listAnimations[-grep("PPT",listAnimations)]
listReportGeneration<-listReportGeneration[-grep("PPT",listReportGeneration)]
listReportGeneration<-listReportGeneration[-grep("WORD",listReportGeneration)]
listSTDiagnostics<-listSTDiagnostics[-grep("PPT",listSTDiagnostics)]

}

rp.menu(GWSDATpnl, menuchoice, 
labels=list(list("File","Save Session"),listAnimations,listReportGeneration,listSTDiagnostics,listPlumeiagnostics),action=do.menu)



rp.listbox(GWSDATpnl, Cont.rg, labels=sort(as.character(Cont.Names)),sleep=0.01,
vals=sort(as.character(Cont.Names)), 
action = redraw, title = "Solute",
initval=if(Use.Defaults && !is.null(Default.Values$Cont.rg)){Default.Values$Cont.rg}else{sort(as.character(Cont.Names))[1]},
grid="ControlsGrid" , row = 0, column = 0,rows =min(5,length(as.character(Cont.Names))))


rp.doublebutton(GWSDATpnl,var = shadow.jjj,initval=if(Use.Defaults  && !is.null(Default.Values$shadow.jjj)){Default.Values$shadow.jjj}else{10*length(All.Data$All.Agg.Dates)},step = 1, range=c(1,20*length(All.Data$All.Agg.Dates)),
action = Make.Moviedoublebutton, title = "Time Steps",grid = "ControlsGrid",row=0,column=2,repeatdelay = 1,repeatinterval=1)

rp.checkbox(GWSDATpnl, dlines, replot.SmoothPlot, labels = c("Conc. Trend Smoother","Conc. Linear Trend Fit","Show Legend","Scale to Conc. Data","Log Conc. Scale","Overlay GW levels","Overlay NAPL Thickness")[c(rep(TRUE,6),NAPLThickPresent)],
title = "Time Series Plot Options",initval=if(Use.Defaults  && !is.null(Default.Values$dlines)){Default.Values$dlines}else{c(TRUE,FALSE,FALSE,FALSE,TRUE,FALSE,FALSE)[c(rep(TRUE,6),NAPLThickPresent)]},
grid = "ControlsGrid", row = 1, column = 0)


if(UseCombo){

     rp.combo(GWSDATpnl, Color.type, prompt="Plot Type:", c("Conc-Terrain", "Conc-Topo","Conc-GreyScale","Conc-Terrain-Circles","Conc-Topo-Circles","Conc-GreyScale-Circles",if(!is.null(All.Data$NAPL.Thickness.Data)){"NAPL-Circles"}else{NULL}),
initval=if(Use.Defaults && !is.null(Default.Values$Color.type)){Default.Values$Color.type}else{"Conc-Terrain"},
action = redraw,grid="ContourControlsGrid",row=0,column=0)     

}else{

rp.radiogroup(GWSDATpnl, Color.type, c("Conc-Terrain", "Conc-Topo","Conc-GreyScale","Conc-Terrain-Circles","Conc-Topo-Circles","Conc-GreyScale-Circles",if(!is.null(All.Data$NAPL.Thickness.Data)){"NAPL-Circles"}else{NULL}), 
initval=if(Use.Defaults && !is.null(Default.Values$Color.type)){Default.Values$Color.type}else{"Conc-Terrain"},
action = redraw, title = "Plot Type",grid="ContourControlsGrid",row=0,column=0)     

}

if(F){
rp.radiogroup(GWSDATpnl, PredInterval, c(c("Upper 95% CI","Predicted","Lower 95% CI","% sd","IQR/2")), initval="Predicted",
action = redraw, title = "Model Prediction",grid="ContourControlsGrid",row=3,column=0)     
}


if(UseCombo){

rp.combo(GWSDATpnl, rgUnits, c("ng/l","ug/l","mg/l"),initval=if(Use.Defaults && !is.null(Default.Values$rgUnits)){Default.Values$rgUnits}else{"ug/l"},
action = redraw, prompt= "Solute Conc. Units",grid="ControlsGrid", row = 0, column = 1)


}else{

rp.radiogroup(GWSDATpnl, rgUnits, c("ng/l","ug/l","mg/l"),initval=if(Use.Defaults && !is.null(Default.Values$rgUnits)){Default.Values$rgUnits}else{"ug/l"},
action = redraw, title = "Solute Conc. Units",grid="ControlsGrid", row = 0, column = 1)

}

if(GWFlowsPresent){

	if(UseCombo){
	
	rp.combo(GWSDATpnl, prompt="GW Flows",GW.disp, c("None", "Same Length", "Weighted Length"), 
	initval = if(Use.Defaults && !is.null(Default.Values$GW.disp)){Default.Values$GW.disp}else{"Weighted Length"},
	action = replot.ImagePlot,grid="ContourControlsGrid",row=1,column=0) 

	}else{

	rp.radiogroup(GWSDATpnl, GW.disp, c("None", "Same Length", "Weighted Length"), 
	initval = if(Use.Defaults && !is.null(Default.Values$GW.disp)){Default.Values$GW.disp}else{"Weighted Length"},
	action = replot.ImagePlot, title = "Groundwater Flows",grid="ContourControlsGrid",row=1,column=0)     

	}


}


if(UseCombo){

rp.combo(GWSDATpnl, rg1, c("Trend","Threshold - Absolute","Threshold - Statistical"),initval=if(Use.Defaults && !is.null(Default.Values$rg1)){Default.Values$rg1}else{"Trend"},
action = replot.Make.Traffic.Plot, prompt = "Display table",grid="TrafficLightControlsGrid", row = 0, column = 0)

}else{

rp.radiogroup(GWSDATpnl, rg1, c("Trend","Threshold - Absolute","Threshold - Statistical"),initval=if(Use.Defaults && !is.null(Default.Values$rg1)){Default.Values$rg1}else{"Trend"},
action = replot.Make.Traffic.Plot, title = "Display table",grid="TrafficLightControlsGrid", row = 0, column = 0)

}


if(UseCombo){

rp.combo(GWSDATpnl, ColTrafficListbox,c("All","Reds","Greens","White","Non-Detects","Greys"),initval="All",
action = replot.Make.Traffic.Plot,prompt="Show:",grid="TrafficLightControlsGrid",row = 4, column = 0,rows=5)

}else{

rp.listbox(GWSDATpnl, ColTrafficListbox,labels=c("All","Reds","Greens","White","Non-Detects","Greys"), 
vals=c("All","Reds","Greens","White","Non-Detects","Greys"),
action = replot.Make.Traffic.Plot,title="Show:",initval="All",grid="TrafficLightControlsGrid",row = 4, column = 0,rows=5)

}




rp.button(GWSDATpnl,title="Edit Thresholds",action=Edit.Threshold.Lims,grid="TrafficLightControlsGrid", row = 1, column = 0)
rp.button(GWSDATpnl,title="   Subset Plot   ",action=Subset.Traffic.Plot,grid="TrafficLightControlsGrid", row = 2, column = 0)
rp.button(GWSDATpnl,title="   Colour Key    ",action=TrafficLightKey,grid="TrafficLightControlsGrid", row = 3, column = 0)


#rp.listbox(GWSDATpnl,Well.Select,labels=sort(as.character(All.Data$All.Wells)),
rp.listbox(GWSDATpnl,Well,labels=sort(as.character(All.Data$All.Wells)),
vals=sort(as.character(All.Data$All.Wells)),
grid="ControlsGrid" ,
initval=if(Use.Defaults && !is.null(Default.Values$Well)){Default.Values$Well}else{sort(as.character(All.Data$All.Wells))[1]},  
row = 1, column = 1,title="Select Monitoring Well",rows =min(10,length(as.character(All.Data$All.Wells))),action=listbox.Well.Select)


#check length of labels to help backward compatability!
rp.checkbox(GWSDATpnl,ScaleCols,initval = if(Use.Defaults && !is.null(Default.Values$ScaleCols)){Default.Values$ScaleCols}else{c(TRUE,FALSE,TRUE,FALSE,if(is.null(All.Data$ShapeFiles)){FALSE}else{TRUE},FALSE)},
labels=c("Show Well Labels?","Scale colours to Data?","Show Conc. Values?","Show GW Contour?","Overlay ShapeFiles?","Plume Diagnostics?"),title="Plot Options:",grid="ContourControlsGrid" ,row = 2, column = 0,
action=replot.MakeImagePlotNoPlot)

#rp.button(GWSDATpnl,title="Edit Plume Thresholds",action=Edit.Plume.Lims,grid="ContourControlsGrid" ,row = 3, column = 0)



##### Image Plot
rp.tkrplot(GWSDATpnl, ImagePlot,  MakeImagePlotNoPlot, 	,grid="topright",row=0,column=0,action=click.MakeSmoothPlotNoPlot,
hscale=1.6*Scale.panelImage$hscale,vscale=1.2*Scale.panelImage$vscale)


##### Time Series Plot
rp.tkrplot(GWSDATpnl, SmoothPlot,  MakeSmoothPlot,row=2,column=1,action= click.MakeSmoothPlot,
hscale=1.4*Scale.panelImage$hscale,vscale=1.25*Scale.panelImage$vscale)


##### Traffic Light Plot
rp.tkrplot(GWSDATpnl, TrafficLightPlot,  Make.Traffic.Plot,row=2,column=2,action= click.Make.Traffic.Plot,
hscale=1.5*Scale.panelImage$hscale,vscale=1.1*Scale.panelImage$vscale)

#--------------------------------------------------- End Build Panel ---------------------------------------------------------#

#--------------------------------------------------------------------------------------------------------------------------#
#browser()
#graphics.off();
#windows(record=T,width =11, height = 9)
#MakeSmoothPlot(GWSDATpnl,showvline=FALSE)
#try(bringToTop())




## Stop GWSDAT rpanel from exiting. 
if(!interactive()){
	rp.block(GWSDATpnl)
}


}



######################################## GWSDAT Select List ################################################################

GWSDAT.select.list<-function (list, preselect = NULL, multiple = FALSE, title = NULL){


################### Select List froma character vector in non-interactive mode #######################################
GWSDAT.select.listMajVer2<-function (choices, preselect = NULL, multiple = FALSE, title = NULL, 
    graphics = getOption("menu.graphics")) 
{
    
    if (!is.null(title) && (!is.character(title) || length(title) != 
        1)) 
        stop("'title' must be NULL or a length-1 character vector")
    if (isTRUE(graphics)) {
        if (.Platform$OS.type == "windows" || .Platform$GUI == 
            "AQUA") 
            return(.Internal(select.list(choices, preselect, 
                multiple, title)))
        else if (graphics && capabilities("tcltk") && capabilities("X11") && 
            suppressWarnings(tcltk:::.TkUp)) 
            return(tcltk::tk_select.list(choices, preselect, 
                multiple, title))
    }
    if (!multiple) {
        res <- menu(choices, FALSE, title)
        if (res < 1L || res > length(choices)) 
            return("")
        else return(choices[res])
    }
    else {
        nc <- length(choices)
        if (length(title) && nzchar(title[1L])) 
            cat(title, "\n", sep = "")
        def <- if (is.null(preselect)) 
            rep(FALSE, nc)
        else choices %in% preselect
        op <- paste(format(seq_len(nc)), ": ", ifelse(def, "+", 
            " "), " ", choices, sep = "")
        if (nc > 10L) {
            fop <- format(op)
            nw <- nchar(fop[1L], "w") + 2L
            ncol <- getOption("width")%/%nw
            if (ncol > 1L) 
                op <- paste(fop, c(rep("  ", ncol - 1L), "\n"), 
                  sep = "", collapse = "")
            cat("", op, sep = "\n")
        }
        else cat("", op, "", sep = "\n")
        cat(gettext("Enter one or more numbers separated by spaces, or an empty line to cancel\n"))
        repeat {
            res <- tryCatch(scan("", what = 0, quiet = TRUE, 
                nlines = 1), error = identity)
            if (!inherits(res, "error")) 
                break
            cat(gettext("Invalid input, please try again\n"))
        }
        if (!length(res) || (length(res) == 1L && !res[1L])) 
            return(character())
        res <- sort(res[1 <= res && res <= nc])
        return(choices[res])
    }
}


GWSDAT.select.listMajVer3<-function (choices, preselect = NULL, multiple = FALSE, title = NULL, 
    graphics = getOption("menu.graphics")) 
{
     if (!is.null(title) && (!is.character(title) || length(title) != 
        1)) 
        stop("'title' must be NULL or a length-1 character vector")
    if (isTRUE(graphics)) {
        if (.Platform$OS.type == "windows" || .Platform$GUI == 
            "AQUA") 
            return(.External2(utils:::C_selectlist, choices, preselect, 
                multiple, title))
        else if (graphics && capabilities("tcltk") && capabilities("X11") && 
            suppressWarnings(tcltk:::.TkUp)) 
            return(tcltk::tk_select.list(choices, preselect, 
                multiple, title))
    }
    if (!multiple) {
        res <- menu(choices, FALSE, title)
        if (res < 1L || res > length(choices)) 
            return("")
        else return(choices[res])
    }
    else {
        nc <- length(choices)
        if (length(title) && nzchar(title[1L])) 
            cat(title, "\n", sep = "")
        def <- if (is.null(preselect)) 
            rep(FALSE, nc)
        else choices %in% preselect
        op <- paste0(format(seq_len(nc)), ": ", ifelse(def, "+", 
            " "), " ", choices)
        if (nc > 10L) {
            fop <- format(op)
            nw <- nchar(fop[1L], "w") + 2L
            ncol <- getOption("width")%/%nw
            if (ncol > 1L) 
                op <- paste(fop, c(rep("  ", ncol - 1L), "\n"), 
                  sep = "", collapse = "")
            cat("", op, sep = "\n")
        }
        else cat("", op, "", sep = "\n")
        cat(gettext("Enter one or more numbers separated by spaces, or an empty line to cancel\n"))
        repeat {
            res <- tryCatch(scan("", what = 0, quiet = TRUE, 
                nlines = 1), error = identity)
            if (!inherits(res, "error")) 
                break
            cat(gettext("Invalid input, please try again\n"))
        }
        if (!length(res) || (length(res) == 1L && !res[1L])) 
            return(character())
        res <- sort(res[1 <= res && res <= nc])
        return(choices[res])
    }
}
#-------------------------------------------------------------------------------------------------------------#


if( R.Version()$major=="2"){
     		GWSDAT.select.listMajVer2(list, preselect , multiple, title)
 	}else{
     		GWSDAT.select.listMajVer3(list, preselect , multiple, title)
	 }


}

#-------------------------------------------------------------------------------------------------------------#

