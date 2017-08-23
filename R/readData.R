



readConcData <- function(input_file, ...) {

  
  if (length(list(...)) == 0)
    DF = try(read.csv(input_file))
  else
    DF = try(read.csv(input_file, header = list(...)$header, sep = list(...)$sep, quote = list(...)$quote))
  
 

  if (!"flags" %in% tolower(names(DF))){ DF$Flags <- rep(NA,nrow(DF))}
  
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
    msg <- paste0("Reading well coordinates failed. Headers missing: ", paste(head_not_found, collapse = ", "), "." )
    showNotification(msg, type = "error")
    return(NULL)
  }
   
  
  
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



readExcelData <- function(filein, sheet, header = NULL, get_subset = TRUE, ign_first_head = "") {
  
  excl <- read_excel(filein, sheet = sheet)
  
  # Find the headers in the sheet
  dftmp <- NULL
  head_err = ""
  end_row = 0
  
  
  for (headj in header) {
    
    found_head <- FALSE
    
    # Look into each column
    for (i in 1:ncol(excl)) {
      
      # Get row offset of header
      if (length(rowpos <- which(excl[,i] == headj)) == 1) {
        
        if (ign_first_head == headj) {
          ign_first_head = ""
          next
        }
        
        found_head <- TRUE
        
        if (get_subset) {
          
          if (end_row == 0) {
            end_row <- which(is.na(excl[ (rowpos + 1):nrow(excl) , i]))[1]
            
            if (!is.na(end_row))
              end_row <- end_row + rowpos - 1
            else 
              end_row <- nrow(excl)
            
          }
          
          # Extract the data for this header
          ctmp <- excl[(rowpos + 1):end_row, i]
          
          colnames(ctmp) <- headj
          
          if (is.null(dftmp))
            dftmp <- ctmp
          else
            dftmp <- cbind(dftmp, ctmp)
        }
        
        break
      }
      
    }
    
    if (!found_head)
      head_err = paste0(head_err, headj) 
    
  }
  
  if (head_err != "") {
    return(head_err)
  }
  
  
  return(dftmp)
  
}

