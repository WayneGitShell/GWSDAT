


Plot_Legend_TrafficLights <- function() {    #panel){

  #x11()
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
  #bringToTop()
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
  
  
  
 
  
  
  
  #-----------------------------------------------------------------------------------#
  
  #bringToTop()
  par(op)
  par(mfrow=c(1,1))
  #return(panel)
  
}


#
#
# Not used - was formerly inside the Plot_Legend_TrafficLights().
#  The Time Series plots provides already a legend inside the plot windows through a option tickbox.
#
Plot_Legend_TimeSeries <- function() {
  
  
  plot(1, type="n", axes=FALSE, xlab="", ylab="")
  try(legend("top",
             c("Detectable Data","Non-Detect Data","NAPL Substituted Data","Linear Conc. Trend","Conc. Smoother","Threshold Limit","GW Elevation","NAPL Thickness"),
             pch=c(19,19,19,-1,-1,-1,1,1),
             lty=c(-1,-1,-1,1,1,2,1,1),
             lwd=c(-1,-1,-1,2,2,3,1,1),
             pt.cex=c(1.2,1.2,1.2,1.2,1.2,1.2,1,1),
             col=c("black","orange","red","green","blue","red","black","red"),horiz = F,cex=.85)
  )
  
  #text(0.5,.3,"Time Series Plot Key",font=2)
  
}
