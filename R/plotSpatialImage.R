




plotSpatialImage <- function(panel, substance = " ", timestep = 1) { 
  
  
  if (panel$ScaleCols["Plume Diagnostics"]) {
    op <- par(mar = c(3,4.1,2,2.1))
  }else{
    op <- par(mar = c(2,4.1,2,2.1))
  }

  
  Col.Option <- panel$ScaleCols["Scale colours to Data"]
  Show.Values <- panel$ScaleCols["Show Conc. Values"]
  Show.GW.Contour <- panel$ScaleCols["Show GW Contour"]
  
  if (is.null(Show.GW.Contour) || length(Show.GW.Contour) == 0 || is.na(Show.GW.Contour)) {
    Show.GW.Contour <- FALSE
  }
  
  Show.Well.Labels <- panel$ScaleCols["Show Well Labels"]
  
  Show.ShapeFile <- FALSE
  if ("Overlay ShapeFiles" %in% names(panel$ScaleCols))
    Show.ShapeFile <- panel$ScaleCols["Overlay ShapeFiles"]
 
  Well.Coords <- panel$All.Data$Well.Coords
  
  
  temp.time.eval <- panel$Fitted.Data[[substance]]$Time.Eval[timestep]
  temp.time.frac <- as.numeric(temp.time.eval - min(panel$Fitted.Data[[substance]]$Time.Eval))/as.numeric(diff(range(panel$Fitted.Data[[substance]]$Time.Eval)))
  
  try(if (temp.time.frac == 1) {temp.time.frac=.999}) # to avoid plot issue with wmf format!
  try(if (temp.time.frac == 0) {temp.time.frac=.001})
  try(if (as.numeric(diff(range(panel$Fitted.Data[[substance]]$Time.Eval))) == 0 || is.nan(temp.time.frac)){temp.time.frac=.999}) # Handle case when only one time point.
  
  
  
  
  date.to.print <-  format(as.Date(panel$Fitted.Data[[1]]$Time.Eval[timestep]),"%d-%b-%Y")
  
  if (panel$GWSDAT_Options$Aggby %in% c("Monthly","Quarterly")) {
    
    if (panel$GWSDAT_Options$Aggby == "Monthly") {
      
      date.range.to.print <- seq.Date(as.Date(as.Date(panel$Fitted.Data[[1]]$Time.Eval[timestep])), by = "-1 month", length=2)
      
    } else {
      
      date.range.to.print <- seq.Date(as.Date(as.Date(panel$Fitted.Data[[1]]$Time.Eval[timestep])), by = "-3 month", length=2)
      
    }	
    
    date.range.to.print[2] <- date.range.to.print[2] + 1
    date.range.to.print <- format(date.range.to.print,"%d-%b-%Y")[c(2,1)]
    date.to.print <- paste(date.range.to.print,collapse = " to ")
    
  }

  
  
  
  model.tune <- panel$Fitted.Data[[substance]][["Model.tune"]]
  temp.Cont.Data<-panel$Fitted.Data[[substance]]$Cont.Data
  temp.Cont.Data<-temp.Cont.Data[temp.Cont.Data$AggDate == temp.time.eval,]
  temp.Cont.Data$log.Resid<-log(temp.Cont.Data$Result.Corr.ND) - log(temp.Cont.Data$ModelPred)
  
  if(panel$rgUnits=="mg/l"){
    
    temp.Cont.Data$Result.Corr.ND<-temp.Cont.Data$Result.Corr.ND/1000
    temp.res<-as.character(temp.Cont.Data$Result)
    temp.res<-gsub("ND<","",temp.res)
    temp.res[tolower(temp.res)!="napl"]<-as.character(as.numeric(temp.res[tolower(temp.res)!="napl"])/1000)
    temp.res[temp.Cont.Data$ND]<-paste("ND<",temp.res[temp.Cont.Data$ND],sep="")
    temp.Cont.Data$Result<-temp.res
    rm(temp.res)
  }
  if(panel$rgUnits == "ng/l"){
    
    temp.Cont.Data$Result.Corr.ND<-temp.Cont.Data$Result.Corr.ND*1000
    temp.res<-as.character(temp.Cont.Data$Result)
    temp.res<-gsub("ND<","",temp.res)
    temp.res[tolower(temp.res)!="napl"]<-as.character(as.numeric(temp.res[tolower(temp.res)!="napl"])*1000)
    temp.res[temp.Cont.Data$ND]<-paste("ND<",temp.res[temp.Cont.Data$ND],sep="")
    temp.Cont.Data$Result<-temp.res
    rm(temp.res)
  }
  
  
  
  Bad.Wells <- as.character(temp.Cont.Data$WellName[which(temp.Cont.Data$log.Resid>1.75)])
  Bad.Wells <- Well.Coords[Well.Coords$WellName %in% Bad.Wells,]
  if(nrow(Bad.Wells) > 0){Bad.Wells$WellName<-paste("<",Bad.Wells$WellName,">",sep="")}
  
  
  diffrangeX <- 0.06*(range(Well.Coords$XCoord)[2]-range(Well.Coords$XCoord)[1])
  diffrangeY <- 0.06*(range(Well.Coords$YCoord)[2]-range(Well.Coords$YCoord)[1])
  
  if ((diffrangeX/diffrangeY) > 1.4) {diffrangeY = 0}
  if ((diffrangeY/diffrangeX) > 1.4) {diffrangeX = 0}
  Contour.xlim = c(range(Well.Coords$XCoord)[1] - diffrangeX,range(Well.Coords$XCoord)[2] + diffrangeX)
  Contour.ylim = c(range(Well.Coords$YCoord)[1] - diffrangeY,range(Well.Coords$YCoord)[2] + diffrangeY)
  
  
  
  if (panel$PredInterval != "% sd") {
    
    lev.cut <- panel$lev.cut
    if (panel$rgUnits == "mg/l") {lev.cut <- lev.cut/10}
    if (panel$rgUnits == "ng/l") {lev.cut <- lev.cut*10}
    
  } else {
    
    lev.cut <- panel$sd.lev.cut
    
  }
  n.col <- length(lev.cut) - 1 #should be n.col-1
  
  
  tmp_cont <- panel$Fitted.Data[[substance]]$Cont.Data
  
  tmp_wells_earlier <- unique(tmp_cont[as.numeric(tmp_cont$AggDate) <= temp.time.eval,]$WellName)
  tmp_wells_later   <- unique(tmp_cont[as.numeric(tmp_cont$AggDate) >= temp.time.eval,]$WellName)
  
  Good.Wells <- intersect(as.character(tmp_wells_earlier), as.character(tmp_wells_later))
  
  #Good.Wells <- as.character(unique(panel$Fitted.Data[[substance]]$Cont.Data[as.numeric(panel$Fitted.Data[[substance]]$Cont.Data$AggDate) <= temp.time.eval,]$WellName))
  #Good.Wells <- intersect(Good.Wells, as.character(unique(panel$Fitted.Data[[substance]]$Cont.Data[as.numeric(panel$Fitted.Data[[substance]]$Cont.Data$AggDate) >= temp.time.eval,]$WellName)))
  
  
  Do.Image <-  TRUE
  
  if (length(Good.Wells) < 3) {
    Do.Image <- FALSE;
    my.area <- as.matrix(Well.Coords[,c("XCoord","YCoord")])
  } else {
    my.area <- as.matrix(Well.Coords[as.character(Well.Coords$WellName) %in% as.character(Good.Wells),c("XCoord","YCoord")])
  }
  
  if ((areapl(my.area[chull(my.area),]) / panel$All.Data$All.Well.Area) < 0.01) {
    Do.Image = FALSE
    my.area <- as.matrix(Well.Coords[,c("XCoord","YCoord")])
  }
  
  
  
  if (!Do.Image) { # If total number of wells is less than 3
    
    my.area <- cbind(
      c(Contour.xlim[1],Contour.xlim[1],Contour.xlim[2],Contour.xlim[2]),
      c(Contour.ylim[1],Contour.ylim[2],Contour.ylim[1],Contour.ylim[2])
    )
    
    colnames(my.area) <- c("XCoord","YCoord")
    
  }
  
 
  
  
  my.area <- my.area[chull(my.area),, drop = F]
  my.exp.area <- expandpoly(my.area,fac=1.05)
  eval.df <- gridpts(my.exp.area,350)
  eval.df <- rbind(eval.df,my.exp.area)
  colnames(eval.df)[1:2] <- c("XCoord","YCoord")
  try(rownames(eval.df) <- NULL)
  eval.df <- as.data.frame(eval.df)
  eval.df$AggDate = rep(temp.time.eval,nrow(eval.df))
  
  
  if (!inherits(model.tune,"try-error")) {
    
    #interp.pred<-GWSDAT.Interp(model.tune$best.mod,AggDate=eval.df$AggDate[1],eval.df,type=if(is.null(panel$PredInterval)){"predict"}else{as.character(panel$PredInterval)})
    interp.pred <- try(GWSDAT.Bary.Interp(model.tune$best.mod,AggDate=eval.df$AggDate[1],my.area=my.area,type=as.character(panel$PredInterval)))
    if (inherits(interp.pred,"try-error")){interp.pred<-GWSDAT.Interp(NULL,AggDate=eval.df$AggDate[1],eval.df)}
    
  } else {
    
    interp.pred <- GWSDAT.Interp(NULL,AggDate=eval.df$AggDate[1],eval.df)
    Do.Image <- FALSE
    
  }
  
  if (Do.Image) {
    
    if (panel$PredInterval %in% c("Lower 95% CI","Predicted","Upper 95% CI","IQR/2")) {
      
      if (panel$PredInterval != "IQR/2") {interp.pred$z <- exp(interp.pred$z)}
      
      if (panel$rgUnits == "mg/l") {interp.pred$z <- interp.pred$z/1000}
      if (panel$rgUnits == "ng/l") {interp.pred$z <- interp.pred$z*1000}
      
    }
    
    if (max(interp.pred$z,na.rm = T) > lev.cut[length(lev.cut)] && !Col.Option) {
      
      interp.pred$z[which(interp.pred$z > lev.cut[length(lev.cut)],arr.ind = T)] <- lev.cut[length(lev.cut)]
    }
    
  }else{
    
    interp.pred$z[,] <- NA
    
    
  }
  
  # This calculates the area, mass, volume and center of the plume.
  # It will be used to include a status text at the bottom of the plot, and
  # to plot the plume contour. 
  if (panel$ScaleCols["Plume Diagnostics"]) {
    
    TotalPlume <- getPlumeDiagnostics(panel, substance, timestep, interp.pred)
    
  }
    
    
  
  GWFlows <- attr(panel$Fitted.Data,"GWFlows")
  if (!inherits(GWFlows, "try-error")) {
    
    
    temp.GW.Flows <- GWFlows[as.numeric(GWFlows$AggDate) == temp.time.eval,]
    
    if (!is.null(panel$GW.disp) && panel$GW.disp != "None") {
      
      L <- 0.05 * sqrt(diff(Contour.xlim)^2 + diff(Contour.ylim)^2)
      
      
      GWFlows <- attr(panel$Fitted.Data,"GWFlows")
      
      
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
  
  
 
  
  GWSDAT.Terrain <- function(n){
    
    terrain.colors(n + 1)[1:n] # to avoid white in extreme conc.
  }
  
  
  if(panel$Color.type == "Conc-Terrain" & Col.Option){
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
  
  
  
  
  if(panel$Color.type == "Conc-Terrain-Circles"){
    
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
    my.palette<-col.palette[(as.numeric(cut(temp.Cont.Data$Result.Corr.ND,breaks=panel$lev.cut)))]
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
  
  
  if (!Col.Option || !Do.Image) {
    
    tmp_main <- paste(substance,
                    if (panel$Color.type == "NAPL-Circles" & substance != " ") {paste("(",panel$rgUnits,")", sep = "")} else {""},
                    if (substance != " ") {":"} else {""},
                    date.to.print,
                    if (panel$All.Data$Aq.sel != "") {paste(": Aquifer-",panel$All.Data$Aq.sel,sep = "")} else {""}
              )
    
    
    
    GWSDAT.filled.contour(interp.pred, asp = 1, 
                          ShapeFiles = if (Show.ShapeFile) {panel$All.Data$ShapeFiles} else {NULL},
                          fixedConcScale = if (panel$Color.type == "NAPL-Circles") {FALSE} else {TRUE},
                          xlim   = Contour.xlim,
                          ylim   = Contour.ylim,
                          levels = lev.cut,
                          col    = col.palette,
                          plot.title = title(main = tmp_main, xlab = "", ylab = "", cex.main = .95),
                          key.title  = if (panel$Color.type == "NAPL-Circles") {title(main = paste("NAPL \nThickness \n(", panel$All.Data$NAPL.Units, ")", sep = ""), 
                                                                                      cex.main = 0.7)} else {title(main = panel$rgUnits)},
                          
                          plot.axes  = {axis(1); axis(2, las = 3); axis(3, at = par("usr")[1]+temp.time.frac*(diff(range(par("usr")[1:2]))),labels="",col="red",lwd=3,tck=-0.02); 
                            if (panel$Color.type == "Conc-Terrain-Circles" || panel$Color.type=="Conc-Topo-Circles" || panel$Color.type=="Conc-GreyScale-Circles"){points(temp.Cont.Data$XCoord[order(my.cex,decreasing=T)],temp.Cont.Data$YCoord[order(my.cex,decreasing=T)],pch=19,col=my.palette[order(my.cex,decreasing=T)],cex=my.cex[order(my.cex,decreasing=T)])}
                            if (panel$Color.type == "Conc-Terrain-Circles" || panel$Color.type=="Conc-Topo-Circles" || panel$Color.type=="Conc-GreyScale-Circles"){points(temp.Cont.Data$XCoord[order(my.cex,decreasing=T)],temp.Cont.Data$YCoord[order(my.cex,decreasing=T)],pch=1,col=1,cex=my.cex[order(my.cex,decreasing=T)])}
                            
                            
                            if (panel$Color.type == "NAPL-Circles") {points(temp.NAPL.Data$XCoord[order(my.cex,decreasing=T)],temp.NAPL.Data$YCoord[order(my.cex,decreasing=T)],pch=19,col=my.palette[order(my.cex,decreasing=T)],cex=my.cex[order(my.cex,decreasing=T)])}
                            if (panel$Color.type == "NAPL-Circles") {points(temp.NAPL.Data$XCoord,temp.NAPL.Data$YCoord,pch=1,col=1,cex=my.cex)}
                            
                            
                            
                            
                            points(Well.Coords$XCoord,Well.Coords$YCoord,pch=19,cex=.7);
                            
                            if(panel$Color.type == "NAPL-Circles") {
                              points(Well.Coords[as.character(Well.Coords$WellName) %in% attributes(NAPL.Thickness.Data)$NAPL.Wells,c("XCoord","YCoord")],col="red",pch=19,cex=0.7)
                            }
                            
                            if(Show.Well.Labels)text(Well.Coords$XCoord,Well.Coords$YCoord,Well.Coords$WellName,cex=0.75,pos=1)
                            
                            if(Show.GW.Contour) {
                              contour(GWSDAT.GW.Contour(temp.GW.Flows),add=T,labcex=.8)
                            }
                            if(Show.Values & length(as.character(temp.Cont.Data$Result))>0)try(text(temp.Cont.Data$XCoord,temp.Cont.Data$YCoord,as.character(temp.Cont.Data$Result),
                                                                                                    cex=0.75,col=c("red","black")[as.numeric(temp.Cont.Data$ND)+1],pos=3),silent=T)
                            

                            if(exists("TotalPlume") & panel$ScaleCols["Plume Diagnostics"]){
                              
                              try(contour(interp.pred, levels = TotalPlume$PLumeCutoff, add = T, col = "red", lwd = 2, labcex = .8))
                              
                              try(points(TotalPlume$PlumeCentreofMass[1], TotalPlume$PlumeCentreofMass[2], cex = 1.3, pch = 3, lwd = 2, col = "red"))
                            }
                        
                            
                            
                            if(nrow(Bad.Wells)>0 & Show.Well.Labels & Do.Image){text(Bad.Wells$XCoord,Bad.Wells$YCoord,Bad.Wells$WellName,cex=0.75,col="red",pos=1)}
                            try(arrows(x0, y0, x1, y1, length = 0.1,lwd=2,col="blue"), silent = TRUE)
                            
                          }
                          
    )
    
    if (panel$ScaleCols["Plume Diagnostics"]) {

      tempUnitHandle <- PlumeUnitHandlingFunc(panel$GWSDAT_Options$WellCoordsLengthUnits,panel$rgUnits,TotalPlume$Mass[1],TotalPlume$area[1])
      
      tp <- paste("Plume Mass=", signif(tempUnitHandle$PlumeMass,5),tempUnitHandle$PlumeMassUnits,";  Plume Area=",signif(tempUnitHandle$PlumeArea,5),tempUnitHandle$PlumeAreaUnits,sep = "")
      mtext(tp,side = 1,adj = -0.1, line = 2,cex = 0.85)
      
    }
    
    
  } else {
    
    
    GWSDAT.filled.contour(interp.pred,asp=1,ShapeFiles=if(Show.ShapeFile){panel$All.Data$ShapeFiles}else{NULL},
                          
                          xlim=Contour.xlim,
                          ylim=Contour.ylim,
                          color.palette=col.palette,
                          plot.title = title(main = paste(substance,":",date.to.print,if(panel$All.Data$Aq.sel!=""){paste(": Aquifer-",panel$All.Data$Aq.sel,sep="")}else{""}),xlab = "", ylab = "",cex.main=.95),key.title = title(main=panel$rgUnits),
                          plot.axes={ axis(1); axis(2,las=3);axis(3,at=par("usr")[1]+temp.time.frac*(diff(range(par("usr")[1:2]))),labels="",col="red",lwd=3,tck=-0.02);  
                            points(Well.Coords$XCoord,Well.Coords$YCoord,pch=19,cex=1.0);
                            if(Show.Well.Labels)text(Well.Coords$XCoord,Well.Coords$YCoord,Well.Coords$WellName,cex=0.75,pos=1)
                            
                            if(Show.GW.Contour)try(contour(GWSDAT.GW.Contour(temp.GW.Flows),add=T,labcex=.8),silent=T)
                            if(Show.Values & length(as.character(temp.Cont.Data$Result))>0)try(text(temp.Cont.Data$XCoord,temp.Cont.Data$YCoord,as.character(temp.Cont.Data$Result),
                                                                                                    cex=0.75,col=c("red","black")[as.numeric(temp.Cont.Data$ND)+1],pos=3),silent=T)
                            
                            
                            if (exists("TotalPlume") & panel$ScaleCols["Plume Diagnostics"]) {
                              
                              contour(interp.pred, levels = TotalPlume$PLumeCutoff, add = T, col = "red", lwd = 2, labcex = .8)
                              # Fixme: why plot twice?
                              try(contour(interp.pred, levels = TotalPlume$PLumeCutoff, add = T, col = "red", lwd = 2, labcex = .8))
                            
                                  
                              try(points(TotalPlume$PlumeCentreofMass[1],TotalPlume$PlumeCentreofMass[2],cex=1.3,pch=3,lwd=2,col="red"))
                            }
                            #--------------------------------------#
                            
                            if(nrow(Bad.Wells)>0 & Show.Well.Labels & Do.Image){text(Bad.Wells$XCoord,Bad.Wells$YCoord,Bad.Wells$WellName,cex=0.75,col="red",pos=1)}
                            try(arrows(x0, y0, x1, y1, length = 0.1,lwd=2,col="blue"), silent = TRUE)
                            
                          }
                          
    )
    
    if (panel$ScaleCols["Plume Diagnostics"]) {
      
      tempUnitHandle <- PlumeUnitHandlingFunc(panel$GWSDAT_Options$WellCoordsLengthUnits,panel$rgUnits,TotalPlume$Mass[1],TotalPlume$area[1])
      
      #tp<-paste("Estimated Plume Mass=",round(TotalPlume$Mass,2),";   Estimated Plume Area=",round(TotalPlume$area,2),sep="")
      
      tp <- paste("Plume Mass=",signif(tempUnitHandle$PlumeMass,5),tempUnitHandle$PlumeMassUnits,";  Plume Area=",signif(tempUnitHandle$PlumeArea,5),tempUnitHandle$PlumeAreaUnits,sep = "")
      mtext(tp,side = 1,adj = -0.1,line = 2, cex = 0.85)
      
    }
    
    
  }
  
  
  par(op)
  return(panel)
  
  
}


makeSpatialPPT <- function(panel, substance, timestep){
 
  # Create temporary wmf file. 
  mytemp <- tempfile(fileext = ".wmf")
  
  win.metafile(mytemp) 
  plotSpatialImage(panel, substance, timestep = timestep)
  dev.off()
   
  # Put into powerpoint slide.
  AddPlotPPV2(mytemp, asp = TRUE) 
  
  try(file.remove(mytemp))
  
    
}


makeSpatialAnimation <- function(panel, substance){
  
  
  for (i in panel$timestep_range[1]:panel$timestep_range[2]) {
 
    # Create temporary wmf file. 
    mytemp <- tempfile(fileext = ".wmf")
    
    win.metafile(mytemp) 
    plotSpatialImage(panel, substance, timestep = i)
    dev.off()
    
    AddPlotPPV2(mytemp, asp = TRUE) 
    
  }
  
}






GWSDAT.GW.Contour <- function(temp.GW.Flows){
  
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

GWSDAT.GrayScale <- function(n){
  
  rev(grey(seq(0,1,length = n + 3)))[c(-1,-2,-(n+3))]
  
}

