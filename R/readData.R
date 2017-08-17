



readConcData <- function(input_file, ...) {

  
 
  
  if (length(list(...)) == 0)
    AGALL = try(read.csv(input_file))
  else
    AGALL = try(read.csv(input_file, header = list(...)$header, sep = list(...)$sep, quote = list(...)$quote))
  
 

  if (!"flags" %in% tolower(names(AGALL))){AGALL$Flags <- rep(NA,nrow(AGALL))}
  AGALL <- try(AGALL[,which(tolower(names(AGALL)) == "wellname"):which(tolower(names(AGALL)) == "flags")])
  
  if (inherits(AGALL, 'try-error')) {
    
    msg <- "Reading the concentration data failed. Make sure to set proper column names."
    showModal(modalDialog(title = "Error", msg, easyClose = FALSE))
    return(NULL)

  }
  
  AGALL$SampleDate <- GWSDAT.excelDate2Date(floor(as.numeric(as.character(AGALL$SampleDate)))) 
  
  
  if (any(is.na(AGALL$SampleDate))) {
    
    AGALL <- AGALL[!is.na(AGALL$SampleDate),]
    
    msg <- "Warning: Incorrect input date value(s) detected. Ommitting these values."
    showNotification(msg, type = "warning", duration = 10)

    
    if (nrow(AGALL) == 0) {
      
      msg <- "Zero entries in concentration data read."
      showModal(modalDialog(title = "Error", msg, easyClose = FALSE))
      return(NULL)
      
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
  
    msg <- "Reading the well coordinates failed. Make sure to set proper column names."
    showModal(modalDialog(title = "Error", msg, easyClose = FALSE))
    return(NULL)
    
  }
  
  return(list(data = WellCoords, unit = coord_unit ))
  
}


