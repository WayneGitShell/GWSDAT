
getFullPlumeStats <- function(csite, substance, plume_thresh, ground_porosity,
                              progressBar = NULL) {

  # This will become a data frame containing in each row the plume statistics 
  # of each date.
  full_plume_stats <- NULL 

  nr_timesteps = length(csite$All.Data$All_Agg_Dates)

  for (i in 1:nr_timesteps) {
    
    datetmp <- csite$All.Data$All_Agg_Dates[i]
    
    progressBar$set(value = (i/nr_timesteps), detail = paste("time point ", i, " / ", nr_timesteps))
    
    interp.pred <- interpConc(csite, substance, datetmp)
    
    plume_stats <- getPlumeStats(csite, substance, datetmp, 
                                 interp.pred$data, plume_thresh, ground_porosity)
    
    # Add date. 
    plume_stats = cbind(plume_stats, "Agg.Date" = datetmp)
    
    
    # Append to full plume stats table.
    if (is.null(full_plume_stats))
      full_plume_stats <- plume_stats
    else
      full_plume_stats <- rbind(full_plume_stats, plume_stats)

  }

  return(full_plume_stats)
}


# Calculate the plume statistics including mass, area, center, and average concentration for a specific time point.
#
# @param csite         GWSDAT data object.
# @param substance     Name of the contaminant.
# @param timepoint     Time point (Date) for which to calculate the plume.
# @param predicted_val Predicted concentration values of the contaminant. 
# @param plume_thresh  Concentration limit defining the plume. 
# @param ground_porosity Porosity of the ground in percent. 
# 
# @return A data frame containing the plume statistics. 
#
#' @importFrom splancs areapl
getPlumeStats <- function(csite, 
                          substance, 
                          timepoint, 
                          predicted_val, 
                          plume_thresh, 
                          ground_porosity ) {
  
  if (csite$ui_attr$conc_unit_selected == "mg/l") {plume_thresh <- plume_thresh/1000}
  if (csite$ui_attr$conc_unit_selected == "ng/l") {plume_thresh <- plume_thresh*1000}
  
  cL <- contourLines(predicted_val, levels = plume_thresh)
  
  model.tune <- csite$Fitted.Data[[substance]][["Model.tune"]]
  
  PlumeDetails = list()
  
  
  if (length(cL) > 0) {
    
    for (i in 1:length(cL)) {
      
      cL[[i]]$Closed <- checkPlumeClosure(cL[[i]])
      
      if (cL[[i]]$Closed) {
        
        cL[[i]]$area <- splancs::areapl(cbind(cL[[i]]$x,cL[[i]]$y))
        tempPlumeQuant <- CalcPlumeStats(model.tune$best.mod,
                                         AggDate = timepoint,
                                         cL[[i]],
                                         plume_thresh = plume_thresh,
                                         type  = csite$ui_attr$pred_interval,
                                         units = csite$ui_attr$conc_unit_selected)
        
        cL[[i]]$Volume <- tempPlumeQuant$PlumeVol
        cL[[i]]$PlumeCentreofMass <- tempPlumeQuant$PlumeCentreofMass
        
      } else {
        
        cL[[i]]$area <- NA
        cL[[i]]$Volume <- NA
        cL[[i]]$PlumeCentreofMass <- NA
        
      }
      
    }
    
  }
  
  if (length(cL) > 0) { 
    PlumeDetails$cL = cL
  } else {
    PlumeDetails$cL = NULL
  }
  
  
  #
  # Create plume statistics summary.
  #
  
  plume_stats <- data.frame(area = NA, volume = NA, mass = NA, avg_conc = NA,
                            mass_centre_x = NA, mass_centre_y = NA,
                            conc_thresh = plume_thresh,
                            substance = substance,
                            ground_porosity = ground_porosity,
                            coord_unit = csite$All.Data$sample_loc$coord_unit,
                            conc_unit = csite$ui_attr$conc_unit_selected
                            )
  
  if (length(PlumeDetails$cL) > 0) {
   
    plume_stats$area     <- sum(unlist(lapply(PlumeDetails$cL,function(l){l$area})))
    plume_stats$volume   <- sum(unlist(lapply(PlumeDetails$cL,function(l){l$Volume})))
    plume_stats$mass     <- plume_stats$volume * plume_stats$ground_porosity
    plume_stats$avg_conc <- plume_stats$volume / plume_stats$area
       
    COMWeights <- unlist(lapply(PlumeDetails$cL, function(l){l$Volume}))/sum(unlist(lapply(PlumeDetails$cL,function(l){l$Volume})))
    plume_stats$mass_centre_x <- sum(COMWeights * unlist(lapply(PlumeDetails$cL,function(l){l$PlumeCentreofMass[1]})))
    plume_stats$mass_centre_y <- sum(COMWeights * unlist(lapply(PlumeDetails$cL,function(l){l$PlumeCentreofMass[2]})))
       
  }

  return(plume_stats)
  
}

#' @importFrom deldir deldir triang.list
#' @importFrom splancs gridpts
CalcPlumeStats <- function(model, AggDate, cL, plume_thresh, type, units){
  
  temppts <- cbind(cL$x, cL$y)
  temppts <- splancs::gridpts(temppts, 100)
  Plume.Tri.Points <- data.frame(XCoord = temppts[,1],YCoord = temppts[,2])
  Plume.Tri.Points$AggDate = as.numeric(AggDate)
  
  
  temppred <- predict(model, newdata = Plume.Tri.Points, se = (type != "Predicted"))
  if (type == "Lower 95% CI") {temppred$predicted <- temppred$predicted - temppred$predicted.sd*1.96}
  if (type == "Upper 95% CI") {temppred$predicted <- temppred$predicted + temppred$predicted.sd*1.96}
  
  Plume.Tri.Points$z <- exp(temppred$predicted)
  
  
  if(units=="mg/l"){Plume.Tri.Points$z<-Plume.Tri.Points$z/1000}
  if(units=="ng/l"){Plume.Tri.Points$z<-Plume.Tri.Points$z*1000}
  
  
  
  cL.Tri.Points  <- data.frame(XCoord=cL$x,YCoord=cL$y,z=rep(plume_thresh,length(cL$x)))
  Vol.Tri.Points <- unique(rbind(Plume.Tri.Points[,c("XCoord","YCoord","z")],cL.Tri.Points))
  
  mydeldir  <- deldir::deldir(x=Vol.Tri.Points$XCoord, y = Vol.Tri.Points$YCoord, z = Vol.Tri.Points$z)
  mytriangs <- deldir::triang.list(mydeldir)
  PlumeVol  <- sum(unlist(lapply(mytriangs, VolIndTri)))
  xPlumeVol <- sum(unlist(lapply(mytriangs, xVolIndTri)))
  yPlumeVol <- sum(unlist(lapply(mytriangs, yVolIndTri)))
  PlumeCentreofMass <- c(xPlumeVol, yPlumeVol) / PlumeVol
  
  
  return(list(PlumeVol=PlumeVol,PlumeCentreofMass=PlumeCentreofMass))
  
}


PlumeUnitHandlingFunc <- function(LengthUnit, rgUnits, PlumeMass, PlumeArea){
  
  if (is.null(LengthUnit)) {
    
    PlumeMass <- 1000*PlumeMass
    
    if (rgUnits == "ng/l") {PlumeMass <- PlumeMass*10^-12}
    if (rgUnits == "ug/l") {PlumeMass <- PlumeMass*10^-9}
    if (rgUnits == "mg/l") {PlumeMass <- PlumeMass*10^-6}
    
    PlumeMassUnits <- paste("(Mass/Unit Depth)",sep = "")
    PlumeAreaUnits <- paste("(Unit Area)",sep = "")
    PlumeAverageUnits <- paste("(",rgUnits,")",sep = "")
    
    
  } else {
    
    if (LengthUnit == "metres") {

      PlumeMass <- 1000*PlumeMass

      if (rgUnits == "ng/l") {PlumeMass <- PlumeMass * 10^-12}
      if (rgUnits == "ug/l") {PlumeMass <- PlumeMass * 10^-9}
      if (rgUnits == "mg/l") {PlumeMass <- PlumeMass * 10^-6}
      
      PlumeMassUnits <- paste("(kg/m)",sep = "")
      PlumeAreaUnits <- paste("(m^2)",sep = "")
      PlumeAverageUnits <- paste("(",rgUnits,")",sep = "")
      
    }
    
    
    if (LengthUnit == "feet") {
      
      PlumeMass <- 1000*PlumeMass/35.315 #per cubic ft
      
      if (rgUnits == "ng/l") {PlumeMass <- PlumeMass * 10^-12}
      if (rgUnits == "ug/l") {PlumeMass <- PlumeMass * 10^-9}
      if (rgUnits == "mg/l") {PlumeMass <- PlumeMass * 10^-6}
      
      PlumeMassUnits <- paste("(kg/ft)",sep = "")
      PlumeAreaUnits <- paste("(ft^2)",sep = "")
      PlumeAverageUnits <- paste("(",rgUnits,")",sep = "")
      
    }
    
  }
  
  return(list(PlumeMass = PlumeMass, PlumeArea = PlumeArea, 
              PlumeMassUnits = PlumeMassUnits, 
              PlumeAreaUnits = PlumeAreaUnits,
              PlumeAverageUnits = PlumeAverageUnits))
  
}





xVolIndTri <- function(l){
  
  x <- l$x
  y <- l$y
  z <- l$z
  
  z = z*x
  
  0.5*(x[1]*(y[2] - y[3]) + x[2]*(y[3]- y[1]) + x[3]*(y[1]- y[2]))*(z[1] + z[2] + z[3]) / 3
}


yVolIndTri<-function(l){
  
  x<-l$x
  y<-l$y
  z<-l$z
  
  z=z*y
  
  0.5*(x[1]*(y[2] - y[3]) + x[2]*(y[3]- y[1]) + x[3]*(y[1]- y[2]))*(z[1] + z[2] + z[3]) / 3
}

checkPlumeClosure <- function(cl){
  
  cl$x[1] == cl$x[length(cl$x)] & cl$y[1] == cl$y[length(cl$y)]
  
}

VolIndTri <- function(l){
  
  x <- l$x
  y <- l$y
  z <- l$z
  
  0.5*(x[1]*(y[2] - y[3]) + x[2] * (y[3] - y[1]) + x[3] * (y[1] - y[2]))*(z[1] + z[2] + z[3]) / 3
}

