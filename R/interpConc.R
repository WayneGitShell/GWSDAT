
#' @importFrom splancs gridpts areapl
interpConc <- function(csite, substance, timepoint,UseReducedWellSet) {
  
  if(UseReducedWellSet){
       model.tune     <- csite$Reduced.Fitted.Data[[substance]][["Model.tune"]]
    }else{
       model.tune     <- csite$Fitted.Data[[substance]][["Model.tune"]]
    }
  
  Well.Coords    <- csite$All.Data$sample_loc$data
  Col.Option     <- csite$ui_attr$spatial_options["Scale colours to Data"]
  
  #
  # Extract useable wells for given substance and timestep.
  #
  tmp_cont <- csite$Fitted.Data[[substance]]$Cont.Data
  
  tmp_wells_earlier <- unique(tmp_cont[as.numeric(tmp_cont$AggDate) <= timepoint,]$WellName)
  tmp_wells_later   <- unique(tmp_cont[as.numeric(tmp_cont$AggDate) >= timepoint,]$WellName)
  
  Good.Wells <- intersect(as.character(tmp_wells_earlier), as.character(tmp_wells_later))
  
  #
  # Find the limits of the contour.
  #
  diffrangeX <- 0.06*(range(Well.Coords$XCoord)[2] - range(Well.Coords$XCoord)[1])
  diffrangeY <- 0.06*(range(Well.Coords$YCoord)[2] - range(Well.Coords$YCoord)[1])
  
  
  if (diffrangeY != 0)
    if ((diffrangeX/diffrangeY) > 1.4) {diffrangeY = 0}
  
  if (diffrangeX != 0)
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
  } else if ((splancs::areapl(tmp_my.area) / csite$All.Data$sample_loc$area) < 0.01) {
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
  my.exp.area <- expandpoly(my.area, fact = 1.05)
  
  
  if (nrow(my.exp.area) != 1) {
    eval.df <- splancs::gridpts(my.exp.area, 350)
    eval.df <- rbind(eval.df,my.exp.area)
  } else {
    eval.df <- my.exp.area
  }
  
  colnames(eval.df)[1:2] <- c("XCoord","YCoord")
  try(rownames(eval.df) <- NULL)
  eval.df <- as.data.frame(eval.df)
  eval.df$AggDate = rep(timepoint,nrow(eval.df))
  
  
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
      interp.pred <- predictValues(NULL, AggDate = eval.df$AggDate[1], eval.df)
    }
    
  } else {
    
    interp.pred <- predictValues(NULL, AggDate = eval.df$AggDate[1], eval.df)
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
