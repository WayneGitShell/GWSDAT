

interpData <- function(csite, substance, timestep) {
  
  
  model.tune     <- csite$Fitted.Data[[substance]][["Model.tune"]]
  Well.Coords    <- csite$All.Data$sample_loc$data
  temp.time.eval <- csite$Fitted.Data[[substance]]$Time.Eval[timestep]
  Col.Option     <- csite$ui_attr$spatial_options["Scale colours to Data"]
  
  #
  # Extract useable wells for given substance and timestep.
  #
  tmp_cont <- csite$Fitted.Data[[substance]]$Cont.Data
  
  tmp_wells_earlier <- unique(tmp_cont[as.numeric(tmp_cont$AggDate) <= temp.time.eval,]$WellName)
  tmp_wells_later   <- unique(tmp_cont[as.numeric(tmp_cont$AggDate) >= temp.time.eval,]$WellName)
  
  Good.Wells <- intersect(as.character(tmp_wells_earlier), as.character(tmp_wells_later))
  
  #
  # Find the limits of the contour.
  #
  diffrangeX <- 0.06*(range(Well.Coords$XCoord)[2] - range(Well.Coords$XCoord)[1])
  diffrangeY <- 0.06*(range(Well.Coords$YCoord)[2] - range(Well.Coords$YCoord)[1])
  
  if ((diffrangeX/diffrangeY) > 1.4) {diffrangeY = 0}
  if ((diffrangeY/diffrangeX) > 1.4) {diffrangeX = 0}
  Contour.xlim = c(range(Well.Coords$XCoord)[1] - diffrangeX,range(Well.Coords$XCoord)[2] + diffrangeX)
  Contour.ylim = c(range(Well.Coords$YCoord)[1] - diffrangeY,range(Well.Coords$YCoord)[2] + diffrangeY)
  
  
  #
  # Define area with outer hull
  #
  
  Do.Image <-  TRUE
  
  if (length(Good.Wells) < 3) {
    
    # Not enough wells to form an area.
    Do.Image <- FALSE
    my.area <- as.matrix(Well.Coords[,c("XCoord","YCoord")])
  } else {
    my.area <- as.matrix(Well.Coords[as.character(Well.Coords$WellName) %in% as.character(Good.Wells),c("XCoord","YCoord")])
  }
  
  tmp_my.area <- my.area[chull(my.area),,drop = FALSE]
  
  if (nrow(tmp_my.area) < 3) {
    Do.Image = FALSE
    my.area <- as.matrix(Well.Coords[,c("XCoord","YCoord")])
  } else if ((areapl(tmp_my.area) / csite$All.Data$sample_loc$area) < 0.01) {
    Do.Image = FALSE
    my.area <- as.matrix(Well.Coords[,c("XCoord","YCoord")])
  }
  
  
  if (!Do.Image) { 
    
    # Not enough wells for the area.
    my.area <- cbind(
      c(Contour.xlim[1],Contour.xlim[1],Contour.xlim[2],Contour.xlim[2]),
      c(Contour.ylim[1],Contour.ylim[2],Contour.ylim[1],Contour.ylim[2])
    )
    
    colnames(my.area) <- c("XCoord","YCoord")
    
  }
  
  
  #
  # Prepare area for interpolation.
  #
  my.area <- my.area[chull(my.area),, drop = F]
  my.exp.area <- expandpoly(my.area,fac=1.05)
  eval.df <- gridpts(my.exp.area,350)
  eval.df <- rbind(eval.df,my.exp.area)
  colnames(eval.df)[1:2] <- c("XCoord","YCoord")
  try(rownames(eval.df) <- NULL)
  eval.df <- as.data.frame(eval.df)
  eval.df$AggDate = rep(temp.time.eval,nrow(eval.df))
  
  
  #
  # Interpolate values inside the area.
  #
  if (!inherits(model.tune,"try-error")) {
    
    #interp.pred<-GWSDAT.Interp(model.tune$best.mod,AggDate=eval.df$AggDate[1],eval.df,type=if(is.null(csite$ui_attr$pred_interval)){"predict"}else{as.character(csite$ui_attr$pred_interval)})
    interp.pred <- try( interpBary(model.tune$best.mod,
                                   AggDate = eval.df$AggDate[1],
                                   my.area = my.area,
                                   type = as.character(csite$ui_attr$pred_interval)
                                           )
                        )
    
    if (inherits(interp.pred,"try-error")) { 
      interp.pred <- interp(NULL, AggDate = eval.df$AggDate[1], eval.df)
    }
    
  } else {
    
    interp.pred <- interp(NULL, AggDate = eval.df$AggDate[1],eval.df)
    Do.Image <- FALSE
    
  }
  
  
  #  
  # Extract level cut (for interp.pred z dimension). 
  #
  if (csite$ui_attr$pred_interval != "% sd") {
    
    lev_cut <- csite$ui_attr$lev_cut
    if (csite$ui_attr$conc_unit_selected == "mg/l") {lev_cut <- lev_cut/10}
    if (csite$ui_attr$conc_unit_selected == "ng/l") {lev_cut <- lev_cut*10}
    
  } else {
    
    lev_cut <- csite$ui_attr$sd_lev_cut
    
  }

    
  #
  # Modify z dimension.
  #
  if (Do.Image) {
    
    if (csite$ui_attr$pred_interval %in% c("Lower 95% CI","Predicted","Upper 95% CI","IQR/2")) {
      
      if (csite$ui_attr$pred_interval != "IQR/2") {interp.pred$z <- exp(interp.pred$z)}
      
      if (csite$ui_attr$conc_unit_selected == "mg/l") {interp.pred$z <- interp.pred$z/1000}
      if (csite$ui_attr$conc_unit_selected == "ng/l") {interp.pred$z <- interp.pred$z*1000}
      
    }
    
    if (max(interp.pred$z,na.rm = T) > lev_cut[length(lev_cut)] && !Col.Option) {
      
      interp.pred$z[which(interp.pred$z > lev_cut[length(lev_cut)],arr.ind = T)] <- lev_cut[length(lev_cut)]
    }
    
  } else{
    
    interp.pred$z[,] <- NA
    
    
  }
  
  
  return(list(data = interp.pred, Do.Image = Do.Image, 
              Contour.xlim = Contour.xlim, Contour.ylim = Contour.ylim))
  
}



######################################## GWSDAT Barycentric Interpolation Function ##########################################

interpBary <- function(model,AggDate,my.area,type=c("Predicted","Lower 95% CI","Upper 95% CI","% sd","IQR/2")) {
  
  require(geometry) 
  require(Matrix) 
  
  type=match.arg(type)
  if(length(AggDate)!=1){stop("Agg Date must be length 1")}
  
  expandpoly<-function (mypol, fact) {
    
    m1 <- mean(mypol[, 1])
    m2 <- mean(mypol[, 2])
    cbind((mypol[, 1] - m1) * fact + m1, (mypol[, 2] - m2) * fact + m2)
    
  }
  
  my.area<-my.area[chull(my.area),,drop=F]
  my.exp.area<-expandpoly(my.area,fac=1.15)
  my.area<-expandpoly(my.area,fac=1.05)
  colnames(my.area)<-c("XCoord","YCoord")
  
  
  
  x0<-seq(min(my.area[,"XCoord"]),max(my.area[,"XCoord"]),length=100)
  y0<-seq(min(my.area[,"YCoord"]),max(my.area[,"YCoord"]),length=100)
  
  x0<-sort(unique(c(x0,my.area[,1])))
  y0<-sort(unique(c(y0,my.area[,2])))
  
  pred.df<-expand.grid(XCoord=x0,YCoord=y0)
  pred.df$AggDate<-as.numeric(AggDate)
  pred.df$pred<-rep(NA,nrow(pred.df))
  
  
  if(!is.null(model)){
    
    ####################### eval.df ####################################
    colnames(my.exp.area)<-c("XCoord","YCoord")
    eval.df<-gridpts(my.exp.area,250)
    eval.df<-data.frame(XCoord=eval.df[,1],YCoord=eval.df[,2])
    eval.df<-rbind(eval.df,my.exp.area)
    eval.df$AggDate=as.numeric(AggDate)
    
    
    
    temppred<-predict(model,newdata=eval.df,se=type!="Predicted")
    eval.df$pred<-temppred$predicted
    eval.df$pred.sd<-temppred$predicted.sd
    #eval.df<<-eval.df
    #print("eval.df<<-eval.df")
    if(type=="Lower 95% CI"){eval.df$pred<-eval.df$pred-1.96*eval.df$pred.sd}
    if(type=="Upper 95% CI"){eval.df$pred<-eval.df$pred+1.96*eval.df$pred.sd}
    if(type=="% sd")   {eval.df$pred<-100*(exp(eval.df$pred.sd)-1)}
    ### Daniel's approx to sd(exp(x)). 
    #if(type=="sd")   {eval.df$pred<-exp(eval.df$pred + eval.df$pred.sd^2)*sqrt(1 - exp(-eval.df$pred.sd^2))}
    if(type=="IQR/2")   {eval.df$pred<-0.5*(exp(qnorm(p=c(0.75), mean = eval.df$pred, sd = eval.df$pred.sd))-exp(qnorm(p=c(0.25), mean = eval.df$pred, sd = eval.df$pred.sd)))}
    
    
    
    #------------------------------------------------------------------#
    
    ####################### pred.df ####################################
    
    pred.df$InOut<-!point.in.polygon(pred.df$XCoord,pred.df$YCoord,my.area[,1],my.area[,2])==0
    
    predred.df<-pred.df[pred.df$InOut,]
    dn <- delaunayn(eval.df[,c("XCoord","YCoord")])
    tri <- tsearch(eval.df[,"XCoord"],eval.df[,"YCoord"],dn,predred.df[,"XCoord"],predred.df[,"YCoord"],bary=T) 
    active <- dn[tri$idx,] 
    
    
    M <- sparseMatrix(i=rep(1:nrow(predred.df),each=3),j=as.numeric(t(active)),x=as.numeric(t(tri$p)),dims=c(nrow(predred.df),length(eval.df$pred))) 
    predred.df$pred<-as.numeric(M%*%eval.df$pred)
    
    
    pred.df$pred[pred.df$InOut]<-predred.df$pred
    
    
  }
  
  out<-list(x=x0,y=y0,z=matrix(pred.df$pred,nrow = length(x0), ncol = length(y0)))
  
  return(out)
  
}



interp <- function(model,AggDate,eval.df,type=c("predicted","lower","upper","sd")){
  
  
  type=match.arg(type)
  
  if(length(AggDate)!=1){stop("Agg Date must be length 1")}
  
  x0=sort(unique(eval.df[,1]))
  y0=sort(unique(eval.df[,2]))
  my.df<-expand.grid(XCoord=x0,YCoord=y0)
  
  if(!is.null(model)){
    
    my.df$AggDate=as.numeric(AggDate)
    my.df$InOut<-!point.in.polygon(my.df$XCoord,my.df$YCoord,eval.df[chull(eval.df[,1:2]),1],eval.df[chull(eval.df[,1:2]),2])==0
    my.df$pred<-rep(NA,nrow(my.df))
    
    temppred<-predict(model,newdata=my.df[my.df$InOut, c("XCoord","YCoord","AggDate")],se=type!="predicted")
    my.df$pred[my.df$InOut]<-temppred$predicted
    my.df$pred.sd[my.df$InOut]<-temppred$predicted.sd
    
    if(type=="lower"){my.df$pred<-my.df$pred-1.96*my.df$pred.sd}
    if(type=="upper"){my.df$pred<-my.df$pred+1.96*my.df$pred.sd}
    if(type=="sd")   {my.df$pred<-my.df$pred.sd}
    
  }else{
    
    my.df$pred<-rep(NA,nrow(my.df))
    
  }
  
  list(x=x0,y=y0,z=matrix(my.df$pred,nrow = length(x0), ncol = length(y0)))
  
}

