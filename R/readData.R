



readConcData <- function(input_file, ...) {

  
 
  
  if (length(list(...)) == 0)
    AGALL = try(read.csv(input_file))
  else
    AGALL = try(read.csv(input_file, header = list(...)$header, sep = list(...)$sep, quote = list(...)$quote))
  
 

  if (!"flags" %in% tolower(names(AGALL))){AGALL$Flags <- rep(NA,nrow(AGALL))}
  AGALL <- try(AGALL[,which(tolower(names(AGALL)) == "wellname"):which(tolower(names(AGALL)) == "flags")])
  
  if (inherits(AGALL, 'try-error')) {
    
    # TK-related:
    # tkmessageBox(title="Error!",message="Error reading Monitoring data.",icon="error",type="ok")
    # try(close(pb))
    
    return(GWSDAT_Error("Error in inputting and formatting data."))
    
    #stop("Error in inputting and formatting data.")
    
  }
  
  AGALL$SampleDate <- GWSDAT.excelDate2Date(floor(as.numeric(as.character(AGALL$SampleDate)))) ##added floor function 28/6/2012
  
  
  if(any(is.na(AGALL$SampleDate))) {
    
    AGALL<-AGALL[!is.na(AGALL$SampleDate),]
    
    #TK stuff:
    #tkmessageBox(title="Warning!",message="Incorrect input date value(s) detected.",icon="error",type="ok")
    
    Run_status = GWSDAT_Warning("Incorrect input date value(s) detected.")
    
    if(nrow(AGALL)==0 || as.character(tkmessageBox(message="Do you wish to continue?",icon="question",type="yesno",default="yes"))!="yes"){
      
      #TK stuff:
      #try(close(pb))	
      #stop("Stop due to bad dates")
      
      return(GWSDAT_Error("Stop tue to bad dates."))
      
      
    } else {
      
      #TK stuff:
      #tkmessageBox(title="Warning!",message="Incorrect date values data will be omitted.",icon="error",type="ok")
      
      Run_status = GWSDAT_Warning("Incorrect date values data will be omitted.")
      
      
    }
    
  }
  
  return(AGALL)
  
  
}



############################# Read Well Coordinates Data Table #########################################################


readWellCoords <- function(input_file, ...) {

  
  if (length(list(...)) == 0)
    WellCoords = try(read.csv(input_file))
  else
    WellCoords = try(read.csv(input_file, header = list(...)$header, sep = list(...)$sep, quote = list(...)$quote))
  
  
  
  coord_unit <- as.character(WellCoords$CoordUnits[1])
  
  if (length(coord_unit) == 0 || is.na(coord_unit)) {
    coord_unit <- NULL
  }
  
  if (!"aquifer" %in% tolower(names(WellCoords))) {
    WellCoords$Aquifer <- rep(NA,nrow(WellCoords))
  }
  
  WellCoords <- try(WellCoords[,which(tolower(names(WellCoords)) == "wellname"):which(tolower(names(WellCoords)) == "aquifer")]) 
  
  if (inherits(WellCoords, 'try-error')) {
    
    #TK stuff:
    #tkmessageBox(title="Error!",message="Error reading Well Coordinates data.",icon="error",type="ok")
    #try(close(pb))
    
    #stop("Error in inputting and formatting data.")
    
    return(GWSDAT_Error("Error in inputting and formatting data."))
    
    
  }
  
  return(list(data = WellCoords, unit = coord_unit ))
  
}


