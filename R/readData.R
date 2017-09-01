

readExcel <- function(filein) {
 
  conc_header <- list("WellName", "Constituent", "SampleDate", "Result", "Units", "Flags")
  well_header <- list("WellName", "XCoord", "YCoord", "Aquifer")
  
  # Fixme: read .ending from $name and append to newfile
  newfile <- paste0(filein$datapath, ".xlsx")
  file.rename(filein$datapath, newfile)
  
  conc_data <- NULL
  well_data <- NULL
  
  ls_sheets <- excel_sheets(newfile)
  
  # Attempt to find valid tables in sheets
  for (sheet in ls_sheets) {
  
    
    ## Read Excel contaminant data #############################################
    ret <- readExcelData(newfile, sheet = sheet, header = conc_header)
  
    if (class(ret) != "data.frame") {
      #  showNotification(paste0("Could not read header: ", paste(ret, collapse = ", ")), type = "error", duration = 10)
      showNotification(paste0("Sheet \'", sheet, "\': No valid contaminant table found, skipping."), duration = 10)
      next
    }
    
    
    if (any(is.na(ret$SampleDate))) {
      
      ret <- ret[!is.na(ret$SampleDate),]
      
      msg <- paste0("Sheet \'", sheet, "\': Incorrect input date value(s) detected. Ommitting values.")
      showNotification(msg, type = "warning", duration = 10)
    
      if (nrow(ret) == 0) {
        showNotification(paste0("Sheet \'", sheet, "\': Zero entries in concentration data read, skipping."), type = "error", duration = 10)
        next
      } 
    }
    
    if (!"flags" %in% tolower(names(ret))) { ret$Flags <- rep("",nrow(ret))}
    ret$Flags[is.na(ret$Flags)] <- ""
    ret$Result[is.na(ret$Result)] <- 0
    ret$SampleDate <- excelDate2Date(floor(as.numeric(as.character(ret$SampleDate)))) 
    
    conc_data <- ret
  
  
    ## Read Excel well data ####################################################
    ret <- readExcelData(newfile, sheet = 2, header = well_header, 
                       ign_first_head = "WellName")
  
    if (class(ret) != "data.frame") {
      # showNotification(paste0("Could not read header: ", paste(ret, collapse = ", ")), type = "error", duration = 10)
      showNotification(paste0("Sheet \'", sheet, "\': No valid well table found, skipping."), duration = 10)
      next
    }
    
    
    coord_unit <- as.character(ret$CoordUnits[1])
    if (length(coord_unit) == 0 || is.na(coord_unit)) coord_unit <- "metres"
    ret$Aquifer[is.na(ret$Aquifer)] <- ""
    
    well_data <- list(data = ret, unit = coord_unit)
    
    showNotification(paste0("Sheet \'", sheet, "\': Found valid tables."), type = "message", duration = 10)
    break
  }
  
  if (is.null(conc_data) || is.null(well_data))
    return(NULL)
  
  
  return(list(conc_data = conc_data, well_data = well_data))
  
  
  
}





#' @importFrom utils read.csv
readConcData <- function(input_file, ...) {

  
  if (length(list(...)) == 0)
    DF = read.csv(input_file)
  else
    DF = read.csv(input_file, header = list(...)$header, sep = list(...)$sep, quote = list(...)$quote)
  
  
  # Create Flags column or replace NA values with "" if exist.
  if (!"flags" %in% tolower(names(DF))) { DF$Flags <- rep("",nrow(DF))}
  DF$Flags[is.na(DF$Flags)] <- ""

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

#' @importFrom utils read.csv
readWellCoords <- function(input_file, ...) {

  
  if (length(list(...)) == 0)
    DF = read.csv(input_file)
  else
    DF = read.csv(input_file, header = list(...)$header, sep = list(...)$sep, quote = list(...)$quote)
  
  
  
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
  
  # Avoid reading column header (in first row) in order to have conform input
  # if table is located somewhere inside the sheet. 'col_names = TRUE' would detect
  # types and which(excl[,i] == headj)) would fail if not character. 
  excl <- read_excel(filein, sheet = sheet, col_names = FALSE)
  
  # Find the headers in the sheet
  dftmp <- NULL
  head_err = ""
  end_row = 0
  
  # Detect empty table.
  if (ncol(excl) == 0)
    return(NULL)
  
  for (headj in header) {
    
    found_head <- FALSE
    
    # Look into each column
    for (i in 1:ncol(excl)) {
      #cat("sheet: ", sheet, ", header: ", headj, ", column: ", i, "\n")
    

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

