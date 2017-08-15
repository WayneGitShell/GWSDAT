

#------------------------------------------------------------------------------------------------------------#

aggregateData <- function(GWSDAT_Options, All.Dates, GW.Data, Cont.Data, 
                          Well.Coords, NAPL.Thickness.Data) {
  
  if (GWSDAT_Options$Aggby == "All Dates")	{my.seq <- NULL}
  if (GWSDAT_Options$Aggby == "Monthly")	{my.seq <- as.Date(sort(seq.Date(max(as.Date(All.Dates)),min(as.Date(All.Dates))-500,by="-1 months")))}
  if (GWSDAT_Options$Aggby == "Quarterly")	{my.seq <- as.Date(sort(seq.Date(max(as.Date(All.Dates)),min(as.Date(All.Dates))-500,by="-3 month")))}
  
  #! Cont.Data changes, if I call it again, will it change again?
  Cont.Data <- GWSDAT.Create.Agg.Date(Cont.Data, GWSDAT_Options$Aggby,my.seq)
  All.Agg.Dates <- as.Date(sort(unique(Cont.Data$AggDate)))
  
  
  
  if(nrow(na.omit(GW.Data)) > 0) {
    
    Agg_GW_Data <- na.omit(GW.Data)
    Agg_GW_Data <- GWSDAT.Create.Agg.Date(Agg_GW_Data,GWSDAT_Options$Aggby,my.seq=my.seq)
    Agg_GW_Data <- GWSDAT.Create.Agg.GW(Agg_GW_Data,Well.Coords,tolower(GWSDAT_Options$AggMethod))
    All.Agg.Dates <- as.Date(sort(unique(c(All.Agg.Dates,unique(Agg_GW_Data$AggDate)))))
    
  } else {
    Agg_GW_Data = NULL
  }
  
  
  if (!is.null(NAPL.Thickness.Data)) { # Just add AggDate column
    
    #! Cont.Data changes, if I call it again, will it change again?
    NAPL.Thickness.Data <- GWSDAT.Create.Agg.Date(NAPL.Thickness.Data, GWSDAT_Options$Aggby, my.seq = my.seq)
    attr(NAPL.Thickness.Data,"lev_cuts") <- pretty(seq(0,max(NAPL.Thickness.Data$Result.Corr.ND,na.rm = T), l = 13), n = 12)
    attr(NAPL.Thickness.Data,"NAPL.Wells") <- sort(unique(as.character(NAPL.Thickness.Data$WellName)))
    
  }
  
  return(list(Cont.Data = Cont.Data, 
              All.Agg.Dates = All.Agg.Dates,
              Agg_GW_Data = Agg_GW_Data,
              NAPL.Thickness.Data = NAPL.Thickness.Data ))
}



##############################################################################################################

GWSDAT.Create.Agg.Date <- function(x,type = c("All Dates","Monthly","Quarterly"), 
                                   my.seq = NULL){
  

  type <- match.arg(type)
  
  if (type == "All Dates") { x$AggDate <- as.Date(x$SampleDate) }
  
  if (type == "Monthly") {
    
    if (is.null(my.seq)) {
      my.seq <- as.Date(sort(seq.Date(max(x$SampleDate), min(x$SampleDate) - 500,
                                      by = "-1 month")))
    }
    x$AggDate <- cut.Date(x$SampleDate,breaks = my.seq, include.lowest = T, 
                          right = T, labels = as.character(my.seq[-1]))
    x$AggDate <- as.Date(as.character(x$AggDate))
  }
  
  if (type == "Quarterly") {
    
    if (is.null(my.seq)) {
      my.seq <- as.Date(sort(seq.Date(max(x$SampleDate), min(x$SampleDate) - 500, 
                                      by = "-3 months")))
    }
    x$AggDate <- cut.Date(x$SampleDate, breaks = my.seq, include.lowest = T, 
                          right = T, labels = as.character(my.seq[-1]))
    x$AggDate <- as.Date(as.character(x$AggDate))
  }
  
  
  return(x)
}
#------------------------------------------------------------------------------------------------------------#







##############################################################################################################

GWSDAT.Create.Agg <- function(x,Well.Coords,type=c("mean","median","min","max")){
  
  require(zoo)
  type <- match.arg(type)
  
  out <- aggregate(x$Result.Corr.ND, by = list(AggDate=x$AggDate,WellName=x$WellName,Constituent=x$Constituent),type)
  names(out)[which(names(out) == "x")] <- "Result.Corr.ND"
  out$XCoord <- Well.Coords[match(as.character(out$WellName),as.character(Well.Coords$WellName)),]$XCoord
  out$YCoord <- Well.Coords[match(as.character(out$WellName),as.character(Well.Coords$WellName)),]$YCoord
  
  if(is.factor(out$AggDate)) {
    out$AggDate <- as.Date(as.numeric(as.character(out$AggDate))) #Compatibility with older versions which returns factor
  }
  
  
  
  
  out$XCoord <- as.numeric(as.character(out$XCoord))
  out$YCoord <- as.numeric(as.character(out$YCoord))
  
  return(out)
}
#------------------------------------------------------------------------------------------------------------#



##############################################################################################################

GWSDAT.Create.Agg.GW <- function(x,Well.Coords,type=c("mean","median","min","max")){
  
  #require(zoo)
  type <- match.arg(type)
  
  
  out <- aggregate(x$Result, by = list(AggDate = x$AggDate,WellName = x$WellName),type)
  names(out)[which(names(out) == "x")] <- "Result"
  
  out$XCoord <- Well.Coords[match(as.character(out$WellName),as.character(Well.Coords$WellName)),]$XCoord
  out$YCoord <- Well.Coords[match(as.character(out$WellName),as.character(Well.Coords$WellName)),]$YCoord
  
  if (is.factor(out$AggDate)) {
    out$AggDate <- as.Date(as.numeric(as.character(out$AggDate))) #Compatibility with older versions of R
  }
  
  out$XCoord <- as.numeric(as.character(out$XCoord))
  out$YCoord <- as.numeric(as.character(out$YCoord))
  
  return(out)
}
#------------------------------------------------------------------------------------------------------------#

