


plotTrendTable <- function(panel, timestep = 1, subset = FALSE){
  
 
  date.to.print <- format(as.Date(panel$Fitted.Data[[1]]$Time.Eval[timestep]),"%d-%b-%Y")
  
  
  if (panel$rg1 == "Trend") {
    
    temp.traffic.Beta.ND.Check <- panel$Traffic.Lights$Beta.ND.Check[,,timestep,drop = F]
    temp.traffic.Betas <- panel$Traffic.Lights$Betas[,,timestep,drop = F]
    Well.Names <- dimnames(temp.traffic.Betas)[[1]]
    Cont.Names <- dimnames(temp.traffic.Betas)[[2]]
  }
  
  if (panel$rg1 == "Threshold - Statistical") {
    
    temp.traffic.Beta.ND.Check <- panel$Traffic.Lights$Beta.ND.Check[,,timestep, drop = F]
    temp.traffic.Ulims <- panel$Traffic.Lights$Smooth.Upper.lims[,,timestep,drop=F]
    Well.Names <- dimnames(temp.traffic.Ulims)[[1]]
    Cont.Names <- dimnames(temp.traffic.Ulims)[[2]]
    
  }
  
  if (panel$rg1 == "Threshold - Absolute") {
    
    temp.traffic.Abs.Thresh.Check <- panel$Traffic.Lights$Abs.Thresh.Check[,,timestep,drop=F]
    Well.Names <- dimnames(temp.traffic.Abs.Thresh.Check)[[1]]
    Cont.Names <- dimnames(temp.traffic.Abs.Thresh.Check)[[2]]
  }
  
  
  if (subset == TRUE) {
    
    if(length(Cont.Names)==1){Cont.Names<-Cont.Names[1]}else{Cont.Names<-GWSDAT.select.list(Cont.Names,multiple =T,title = "Select Solutes to Plot",preselect=Cont.Names)}
    if(length(Cont.Names)==0){stop("No Solutes selected!")}
    
    Well.Names<-GWSDAT.select.list(Well.Names,multiple =T,title = "Select Wells to Plot",preselect=Well.Names)
    if(length(Well.Names)==0){stop("No Wells selected!")}
    
  }
  
  if(panel$rg1=="Trend"){
    
    temp.traffic.Beta.ND.Check<-panel$Traffic.Lights$Beta.ND.Check[Well.Names,Cont.Names,timestep,drop=F]
    temp.traffic.Betas<-panel$Traffic.Lights$Betas[Well.Names,Cont.Names,timestep,drop=F]
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
    
    temp.traffic.Beta.ND.Check<-panel$Traffic.Lights$Beta.ND.Check[Well.Names,Cont.Names,timestep,drop=F]
    temp.traffic.Ulims<-panel$Traffic.Lights$Smooth.Upper.lims[Well.Names,Cont.Names,timestep,drop=F]
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
    
    
    temp.traffic.Abs.Thresh.Check<-panel$Traffic.Lights$Abs.Thresh.Check[Well.Names,Cont.Names,timestep,drop=F]
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


plotTrendTablePPT <- function(panel, timestep, subset = FALSE, width = 7, height = 5){
  
  # Create temporary wmf file. 
  mytemp <- tempfile(fileext = ".wmf")
  
  win.metafile(mytemp, width = width, height = height) 
  plotTrendTable(panel, timestep)
  dev.off()
  
  # Put into powerpoint slide.
  AddPlotPPV2(mytemp, width, height) 
  
  try(file.remove(mytemp))
  
  
}

makeTrendTableAnimation <- function(panel, width = 7, height = 5){
  
  
  for (i in panel$timestep_range[1]:panel$timestep_range[2]) {
    
    # Create temporary wmf file. 
    mytemp <- tempfile(fileext = ".wmf")
    
    win.metafile(mytemp, width = width, height = height) 
    plotTrendTable(panel, timestep = i)
    dev.off()
    
    AddPlotPPV2(mytemp, width, height) 
    
  }
  
}