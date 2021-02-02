
pasteAggLimit <- function(timep, aggr_by, fchrin = "%d-%m-%Y", fout = "%d-%b-%Y") {
  
 
  aggr_by <- tolower(aggr_by)
  
  if (is.character(timep))
    timep <- as.Date(timep, fchrin)
  
  
  # Create the string for the date or date range to print
  dout <- format.Date(timep, fout)

  # Need the end of the aggregation period
  if (tolower(aggr_by) != "day") {
    
    # The second element will be the last day of the month or quarter, year.
    #period <- seq.Date(timep, by = aggr_by, length.out = 2) - 1
    period <- seq.Date(timep, by = paste0("-1 ",aggr_by), length.out = 2) + 1
    dout   <- paste0(format.Date(period[2], fout), " to ", dout)
    #dout   <- paste0(dout, " to ", format.Date(period[2], fout))
  }
  
  return(dout)
}


aggregateData <- function(Cont.Data, GW.Data, NAPL.Thickness.Data, Well.Coords, 
                          aggr_by, aggr_gw_type) {
  

  if (!(tolower(aggr_by) %in% c("day", "month", "quarter", "year" )))
    stop("Need to specify valid aggregation period (aggr_by): day, month, quarter, or year.")
  
  if (!(tolower(aggr_gw_type) %in% c("mean","median","min","max")))
    stop("Need to specify valid GW aggregation method (aggr_gw_type): mean, meadian, min, or max.")
  
  
  All.Dates<-as.Date(sort(unique(c(Cont.Data$SampleDate,GW.Data$SampleDate, NAPL.Thickness.Data$SampleDate))))
  my.seq<-as.Date(sort(seq.Date(max(Cont.Data$SampleDate),min(Cont.Data$SampleDate)-500,by=paste("-1",tolower(aggr_by)))))
  Cont.Data$AggDate<-as.Date(cut.Date(Cont.Data$SampleDate,breaks=my.seq,include.lowest=T,right=T,labels=as.character(my.seq[-1])))
  
  #Cont.Data$AggDate <- as.Date(cut.Date(Cont.Data$SampleDate, breaks = tolower(aggr_by), include.lower = TRUE))
  All_Agg_Dates <- sort(unique(Cont.Data$AggDate))
  Agg_GW_Data <- NULL
  
  # If there is groundwater data, generate the corresponding aggregation dates
  #  and append to the list of contaminant aggregation dates.
  if (nrow(na.omit(GW.Data)) > 0) {
    
    Agg_GW_Data <- na.omit(GW.Data)
    Agg_GW_Data$AggDate<-as.Date(cut.Date(Agg_GW_Data$SampleDate,breaks=my.seq,include.lowest=T,right=T,labels=as.character(my.seq[-1])))
    #Agg_GW_Data$AggDate <- as.Date(cut.Date(Agg_GW_Data$SampleDate, breaks = tolower(aggr_by), include.lower = TRUE))
    Agg_GW_Data <- createGWAggDates(Agg_GW_Data, Well.Coords, tolower(aggr_gw_type))
    
    # Append GW aggregated dates.
    All_Agg_Dates <- sort(unique(c(All_Agg_Dates, Agg_GW_Data$AggDate)))
    
  }
  
  
  if (!is.null(NAPL.Thickness.Data)) { 
   
    #NAPL.Thickness.Data$AggDate <- as.Date(cut.Date(NAPL.Thickness.Data$SampleDate, breaks = tolower(aggr_by), include.lower = TRUE))
    NAPL.Thickness.Data$AggDate<-as.Date(cut.Date(NAPL.Thickness.Data$SampleDate,breaks=my.seq,include.lowest=T,right=T,labels=as.character(my.seq[-1])))
    attr(NAPL.Thickness.Data, "lev_cuts")   <- pretty(seq(0,max(NAPL.Thickness.Data$Result.Corr.ND, na.rm = T), l = 13), n = 12)
    attr(NAPL.Thickness.Data, "NAPL.Wells") <- sort(unique(as.character(NAPL.Thickness.Data$WellName)))
    
  }
  
  return(list(All_Agg_Dates = All_Agg_Dates,
              Cont.Data = Cont.Data, 
              Agg_GW_Data = Agg_GW_Data,
              NAPL.Thickness.Data = NAPL.Thickness.Data ))
}



createGWAggDates <- function(x, Well.Coords, type=c("mean","median","min","max")){
  
  type <- match.arg(type)
  
  
  out <- aggregate(x$Result, by = list(AggDate = x$AggDate, WellName = x$WellName), type)
  names(out)[which(names(out) == "x")] <- "Result"
  
  out$XCoord <- Well.Coords[match(as.character(out$WellName), as.character(Well.Coords$WellName)),]$XCoord
  out$YCoord <- Well.Coords[match(as.character(out$WellName), as.character(Well.Coords$WellName)),]$YCoord
  
  if (is.factor(out$AggDate)) {
    out$AggDate <- as.Date(as.numeric(as.character(out$AggDate))) #Compatibility with older versions of R
  }
  
  return(out)
}


