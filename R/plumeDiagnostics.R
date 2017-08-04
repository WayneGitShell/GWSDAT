
#' Calculate the Plume Diagnostics.
#'
#' @param panel 
#' @param cL 
#' @param PLumeCutoff 
#' @param temp.time.eval 
#'
#' @return
#' @export
#'
#' @examples
getPlumeDiagnostics <- function(panel, substance, timestep, predicted_val ) {
  
  PLumeCutoff <- as.numeric(panel$PlumeLimEntry[match(substance, names(panel$Fitted.Data))])  #Defined in ug/L
  if (panel$rgUnits == "mg/l") {PLumeCutoff <- PLumeCutoff/1000}
  if (panel$rgUnits == "ng/l") {PLumeCutoff <- PLumeCutoff*1000}
  
  cL <- contourLines(predicted_val, levels = PLumeCutoff)
  
  model.tune <- panel$Fitted.Data[[substance]][["Model.tune"]]
  temp.time.eval <- panel$Fitted.Data[[substance]]$Time.Eval[timestep]
  
  
  
  PlumeDetails = list()
  
  
  if (length(cL) > 0) {
    
    for (i in 1:length(cL)) {
      
      cL[[i]]$Closed <- checkPlumeClosure(cL[[i]])
      
      if (cL[[i]]$Closed) {
        
        cL[[i]]$area <- areapl(cbind(cL[[i]]$x,cL[[i]]$y))
        tempPlumeQuant <- CalcPlumeStats(model.tune$best.mod,
                                         AggDate = temp.time.eval,
                                         cL[[i]],
                                         PLumeCutoff = PLumeCutoff,
                                         type = as.character(panel$PredInterval),
                                         units = panel$rgUnits)
        
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
  
  
  
  TotalPlume <- list()
  TotalPlume$PLumeCutoff <- PLumeCutoff
  
  if (length(PlumeDetails$cL) > 0) {
    
    TotalPlume$area <- sum(unlist(lapply(PlumeDetails$cL,function(l){l$area})))
    TotalPlume$volume <- sum(unlist(lapply(PlumeDetails$cL,function(l){l$Volume})))
    TotalPlume$Mass <- TotalPlume$volume * as.numeric(panel$Porosity)
    
    COMWeights <- unlist(lapply(PlumeDetails$cL,function(l){l$Volume}))/sum(unlist(lapply(PlumeDetails$cL,function(l){l$Volume})))
    TotalPlume$PlumeCentreofMass <- rep(NA,2)
    TotalPlume$PlumeCentreofMass[1] <- sum(COMWeights*unlist(lapply(PlumeDetails$cL,function(l){l$PlumeCentreofMass[1]})))
    TotalPlume$PlumeCentreofMass[2] <- sum(COMWeights*unlist(lapply(PlumeDetails$cL,function(l){l$PlumeCentreofMass[2]})))
    
    
  } else {
    
    TotalPlume$area <- NA
    TotalPlume$volume <- NA
    TotalPlume$Mass <- NA
    TotalPlume$PlumeCentreofMass <- c(NA,NA)
    
  }
  
  
  return(TotalPlume)
  
  
  
}

PlumeUnitHandlingFunc <- function(LengthUnit,rgUnits,PlumeMass,PlumeArea){
  
  if (is.null(LengthUnit)) {
    
    PlumeMass <- 1000*PlumeMass
    if (rgUnits == "ng/l") {PlumeMass <- PlumeMass*10^-12}
    if (rgUnits == "ug/l") {PlumeMass <- PlumeMass*10^-9}
    if (rgUnits == "mg/l") {PlumeMass <- PlumeMass*10^-6}
    PlumeMassUnits <- paste(" (Mass/Unit Depth)",sep = "")
    PlumeAreaUnits <- paste(" (Unit Area)",sep = "")
    #PlumeAverageUnits<-paste(" (Mass/Unit Volume)",sep="")
    PlumeAverageUnits <- paste("(",rgUnits,")",sep = "")
    
    
  } else {
    
    if (LengthUnit == "metres") {
      
      
      PlumeMass <- 1000*PlumeMass
      if (rgUnits == "ng/l") {PlumeMass <- PlumeMass*10^-12}
      if (rgUnits == "ug/l") {PlumeMass <- PlumeMass*10^-9}
      if (rgUnits == "mg/l") {PlumeMass <- PlumeMass*10^-6}
      PlumeMassUnits <- paste(" (kg/m)",sep = "")
      PlumeAreaUnits <- paste(" (m^2)",sep = "")
      #PlumeAverageUnits<-paste(" (kg/m^3)",sep="")
      PlumeAverageUnits <- paste("(",rgUnits,")",sep = "")
      
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


expandpoly <- function(mypol, fact) {
  
  m1 <- mean(mypol[, 1])
  m2 <- mean(mypol[, 2])
  cbind((mypol[, 1] - m1) * fact + m1, (mypol[, 2] - m2) * fact + m2)
  
}



CalcPlumeStats <- function(model,AggDate,cL,PLumeCutoff,type,units){
  
  temppts <- cbind(cL$x,cL$y)
  temppts <- gridpts(temppts,100)
  Plume.Tri.Points <- data.frame(XCoord = temppts[,1],YCoord = temppts[,2])
  Plume.Tri.Points$AggDate = as.numeric(AggDate)
  
  
  temppred <- predict(model, newdata = Plume.Tri.Points, se = (type != "Predicted"))
  if (type == "Lower 95% CI") {temppred$predicted <- temppred$predicted - temppred$predicted.sd*1.96}
  if (type == "Upper 95% CI") {temppred$predicted <- temppred$predicted + temppred$predicted.sd*1.96}
  
  Plume.Tri.Points$z <- exp(temppred$predicted)
  
  
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
  
  cl$x[1] == cl$x[length(cl$x)] & cl$y[1]==cl$y[length(cl$y)]
  
}

VolIndTri <- function(l){
  
  x <- l$x
  y <- l$y
  z <- l$z
  
  0.5*(x[1]*(y[2] - y[3]) + x[2]*(y[3]- y[1]) + x[3]*(y[1]- y[2]))*(z[1] + z[2] + z[3]) / 3
}
