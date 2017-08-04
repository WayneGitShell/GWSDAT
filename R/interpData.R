

interpData <- function(panel, substance, timestep, Col.Option) {
  
  
  model.tune <- panel$Fitted.Data[[substance]][["Model.tune"]]
  
  Well.Coords <- panel$All.Data$Well.Coords
  
  temp.time.eval <- panel$Fitted.Data[[substance]]$Time.Eval[timestep]
  
  #
  # Extract useable wells for given substance and timestep.
  #
  tmp_cont <- panel$Fitted.Data[[substance]]$Cont.Data
  
  tmp_wells_earlier <- unique(tmp_cont[as.numeric(tmp_cont$AggDate) <= temp.time.eval,]$WellName)
  tmp_wells_later   <- unique(tmp_cont[as.numeric(tmp_cont$AggDate) >= temp.time.eval,]$WellName)
  
  Good.Wells <- intersect(as.character(tmp_wells_earlier), as.character(tmp_wells_later))
  
  
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
  
  if ((areapl(my.area[chull(my.area),]) / panel$All.Data$All.Well.Area) < 0.01) {
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
    
    #interp.pred<-GWSDAT.Interp(model.tune$best.mod,AggDate=eval.df$AggDate[1],eval.df,type=if(is.null(panel$PredInterval)){"predict"}else{as.character(panel$PredInterval)})
    interp.pred <- try( GWSDAT.Bary.Interp(model.tune$best.mod,
                                           AggDate = eval.df$AggDate[1],
                                           my.area = my.area,
                                           type = as.character(panel$PredInterval)
                                           )
                        )
    
    if (inherits(interp.pred,"try-error")) { 
      interp.pred <- GWSDAT.Interp(NULL, AggDate = eval.df$AggDate[1], eval.df)
    }
    
  } else {
    
    interp.pred <- GWSDAT.Interp(NULL,AggDate=eval.df$AggDate[1],eval.df)
    Do.Image <- FALSE
    
  }
  
  
  #  
  # Extract level cut (for interp.pred z dimension). 
  #
  if (panel$PredInterval != "% sd") {
    
    lev.cut <- panel$lev.cut
    if (panel$rgUnits == "mg/l") {lev.cut <- lev.cut/10}
    if (panel$rgUnits == "ng/l") {lev.cut <- lev.cut*10}
    
  } else {
    
    lev.cut <- panel$sd.lev.cut
    
  }

    
  #
  # Modify z dimension.
  #
  if (Do.Image) {
    
    if (panel$PredInterval %in% c("Lower 95% CI","Predicted","Upper 95% CI","IQR/2")) {
      
      if (panel$PredInterval != "IQR/2") {interp.pred$z <- exp(interp.pred$z)}
      
      if (panel$rgUnits == "mg/l") {interp.pred$z <- interp.pred$z/1000}
      if (panel$rgUnits == "ng/l") {interp.pred$z <- interp.pred$z*1000}
      
    }
    
    if (max(interp.pred$z,na.rm = T) > lev.cut[length(lev.cut)] && !Col.Option) {
      
      interp.pred$z[which(interp.pred$z > lev.cut[length(lev.cut)],arr.ind = T)] <- lev.cut[length(lev.cut)]
    }
    
  } else{
    
    interp.pred$z[,] <- NA
    
    
  }
  
  
  return(interp.pred)
  
}