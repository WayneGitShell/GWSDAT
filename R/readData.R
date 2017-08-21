



readConcData <- function(input_file, ...) {

  
  if (length(list(...)) == 0)
    DF = try(read.csv(input_file))
  else
    DF = try(read.csv(input_file, header = list(...)$header, sep = list(...)$sep, quote = list(...)$quote))
  
 

  if (!"flags" %in% tolower(names(DF))){DF$Flags <- rep(NA,nrow(DF))}
  
  # AGALL <- try(AGALL[,which(tolower(names(AGALL)) == "wellname"):which(tolower(names(AGALL)) == "flags")])
  # 
  # if (inherits(AGALL, 'try-error')) {
  #   
  #   msg <- "Reading the concentration data failed. Make sure to set proper column names."
  #   showModal(modalDialog(title = "Error", msg, easyClose = FALSE))
  #   return(NULL)
  # 
  # }
  # 
  
  
  valid_header <- list("WellName", "Constituent", "SampleDate", "Result", "Units", "Flags")
  
  DF_extract <- data.frame(matrix(nrow = nrow(DF), ncol = 0))
  head_not_found <- ""
  
  # Filter input data frame for valid headers.    
  for (vh in valid_header) {
    
    if (vh %in% colnames(DF)) {
      
      DF_extract <- cbind(DF_extract, DF[,vh])
      colnames(DF_extract)[ncol(DF_extract)] <- vh
    } else
      head_not_found = paste(head_not_found, vh)
    
  }
  
  
  if (head_not_found != "") {
    msg <- paste0("Reading well coordinates failed. Missing following columns: ", paste(head_not_found, collapse = ", "), "." )
    showModal(modalDialog(title = "Error", msg, easyClose = FALSE))
    return(NULL)
  }
   
  #if (input$excel_date) 
  #  DF_extract$SampleDate <- as.character(GWSDAT.excelDate2Date(floor(as.numeric(as.character(DF_extract$SampleDate))))) 
  
  DF$SampleDate <- GWSDAT.excelDate2Date(floor(as.numeric(as.character(DF$SampleDate)))) 
  
  
  if (any(is.na(DF$SampleDate))) {
    
    DF <- DF[!is.na(DF$SampleDate),]
    
    msg <- "Warning: Incorrect input date value(s) detected. Ommitting these values."
    showNotification(msg, type = "warning", duration = 10)

    
    if (nrow(DF) == 0) {
      
      msg <- "Zero entries in concentration data read."
      showModal(modalDialog(title = "Error", msg, easyClose = FALSE))
      return(NULL)
    } 
  }
  
  return(DF)
  
}



############################# Read Well Coordinates Data Table #########################################################


readWellCoords <- function(input_file, ...) {

  
  if (length(list(...)) == 0)
    DF = try(read.csv(input_file))
  else
    DF = try(read.csv(input_file, header = list(...)$header, sep = list(...)$sep, quote = list(...)$quote))
  
  
  
  coord_unit <- as.character(DF$CoordUnits[1])
  
  if (length(coord_unit) == 0 || is.na(coord_unit)) {
    coord_unit <- NULL
  }
  
  if (!"aquifer" %in% tolower(names(DF))) {
    DF$Aquifer <- rep(NA,nrow(DF))
  }
  
  valid_header <- list("WellName", "XCoord", "YCoord", "Aquifer", "CoordUnits")
  
  DF_extract <- data.frame(matrix(nrow = nrow(DF), ncol = 0))
  head_not_found <- ""
  
  # Filter input data frame for valid headers.    
  for (vh in valid_header) {
    
    if (vh %in% colnames(DF)) {
      
      DF_extract <- cbind(DF_extract, DF[,vh])
      colnames(DF_extract)[ncol(DF_extract)] <- vh
    } else head_not_found = paste(head_not_found, vh)
  }
  
  if (head_not_found != "") {
    msg <- paste0("Reading well coordinates failed. Missing the following columns: ", head_not_found, "." )
    showModal(modalDialog(title = "Error", msg, easyClose = FALSE))
    return(NULL)
  } 
  
  return(list(data = DF_extract, unit = coord_unit ))
  
}


