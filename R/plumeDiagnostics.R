
PlumeUnitHandlingFunc <- function(LengthUnit,rgUnits,PlumeMass,PlumeArea){
  
  if(is.null(LengthUnit)) {
    
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
