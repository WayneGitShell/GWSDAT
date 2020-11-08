

#' @importFrom readxl read_excel
readExcelData <- function(filein, sheet, header = NULL, get_subset = TRUE, ign_first_head = "") {
    
    # Avoid reading column header (in first row) in order to have conform input
    # if table is located somewhere inside the sheet. 'col_names = TRUE' would detect
    # types and which(excl[,i] == headj)) would fail if not a character. 
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


#' @importFrom readxl excel_sheets
readExcel <- function(filein, sheet = NULL) {
    
    
    conc_header <- list("WellName", "Constituent", "SampleDate", "Result", "Units", "Flags")
    well_header <- list("WellName", "XCoord", "YCoord", "Aquifer")
   
    conc_data <- NULL
    well_data <- NULL
    coord_unit <- "metres"
    
    # If no sheet was specified, extract them and try to find tables.
    if (is.null(sheet)) 
        ls_sheets <- readxl::excel_sheets(filein$datapath)
    else 
        ls_sheets <- list(sheet)
    
    # Attempt to find valid tables in sheets.
    for (sheet in ls_sheets) {
        
        #
        # Read the contaminant data 
        #
        ret <- readExcelData(filein$datapath, sheet = sheet, header = conc_header)
        
        if (class(ret) != "data.frame") {
            showNotification(paste0("Sheet \'", sheet, "\': No valid contaminant table found."), duration = 10, type = "error")
            next
        }
        
        # Check if the date input is correct.
        if (any(is.na(ret$SampleDate))) {
            
            ret <- ret[!is.na(ret$SampleDate),]
            
            msg <- paste0("Sheet \'", sheet, "\': Incorrect input date value(s) detected. Ommitting values.")
            showNotification(msg, type = "warning", duration = 10)
            
            if (nrow(ret) == 0) {
                showNotification(paste0("Sheet \'", sheet, "\': Zero entries in concentration data read, skipping."), type = "error", duration = 10)
                next
            } 
        }
        
        # Modify some columns in order to make it display nicely in the table view.
        if (!"flags" %in% tolower(names(ret))) { ret$Flags <- rep("",nrow(ret))}
        ret$Flags[is.na(ret$Flags)] <- ""
        ret$Result[is.na(ret$Result)] <- 0
        ret$SampleDate <- excelDate2Date(floor(as.numeric(as.character(ret$SampleDate)))) 
        
        conc_data <- ret
        
        
        #
        # Read the well data 
        #
        well_data <- readExcelData(filein$datapath, sheet = sheet, header = well_header, 
                             ign_first_head = "WellName")
        
        if (class(well_data) != "data.frame") {
            showNotification(paste0("Sheet \'", sheet, "\': No valid well table found, skipping."), duration = 10)
            next
        }
        
        
        # Extract the coordinate unit (default: metres).
        coord_unit <- as.character(ret$CoordUnits[1])
        if (length(coord_unit) == 0 || is.na(coord_unit)) coord_unit <- "metres"
        
        # Replace <NA> Aquifer with emptry string
        well_data$Aquifer[is.na(well_data$Aquifer)] <- ""
        
        #
        # Attempt to read shape files (if not found, ignore)
        # (Disabled: Shape files must be uploaded with the file input control)
        #
        # ret <- readExcelData(newfile, sheet = sheet, header = shape_header)
        # 
        # shape_files <- NULL
        # 
        # if (any(class(ret) == "data.frame")) {
        #     
        #     shape_files <- validateShapeFiles(ret, sheet)
        #     
        #     if (!is.null(shape_files))
        #         showNotification(paste0("Sheet \'", sheet, "\': Found ", length(shape_files), 
        #                                 " shape file(s)."), type = "message", duration = 10)
        #     
        # }
        

        # If we made it until here, we were able to read some valid data. 
        showNotification(paste0("Sheet \'", sheet, "\': Found valid tables."), type = "message", duration = 10)
        break
    }
    
    if (is.null(conc_data) || is.null(well_data))
        return(NULL)
    
    
    return(list(conc_data = conc_data, well_data = well_data, coord_unit = coord_unit))
     
}


#' @importFrom utils read.csv
#' @importFrom lubridate parse_date_time
readConcData <- function(input_file, valid_header, ...) {

  
  if (length(list(...)) == 0)
    DF = read.csv(input_file)
  else
    DF = read.csv(input_file, header = list(...)$header, sep = list(...)$sep, quote = list(...)$quote)
  
  
  # Create Flags column or replace NA values with "" if exist.
  if (!"flags" %in% tolower(names(DF))) { DF$Flags <- rep("",nrow(DF))}
  
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
    msg <- paste0("Reading well coordinates failed. Header missing: ", paste(head_not_found, collapse = ", "), ". Try to change the Column Separator." )
    showNotification(msg, type = "error", duration = 7)
    return(NULL)
  }
   
  
  # Check the dates
  if (any(is.na(DF$SampleDate))) {
    
    DF <- DF[!is.na(DF$SampleDate),]
    
    msg <- "Warning: Incorrect input date value(s) detected. Ommitting these values."
    showNotification(msg, type = "warning", duration = 10)

    
    if (nrow(DF) == 0) {
      
      msg <- "Detected Zero entries in concentration data. Aborting file read."
      showNotification(msg, type = "error", duration = 10)
      return(NULL)
    } 
  }
  
 
  # Make some basic modifications, maybe move these to the format function.
  # However, these mods are made before showing up in the import table, but
  # the format function is called only when the import button is pressed.
  DF$Flags[is.na(DF$Flags)] <- ""
  
  # Converting it to character (from numeric or factor) makes it possible to replace
  # NA values (if factor). formatData() will later convert it to numeric values.
  DF$Result <- as.character(DF$Result) 
  DF$Result[is.na(DF$Result)] <- "0"
    
  # Transform the 'SampleDate' column into 'Date' class.  
  if (class(DF$SampleDate) == "numeric" | class(DF$SampleDate) == "integer")
      # An integer value indicates Excel time. This is _not_ Unix time!
      DF$SampleDate <- excelDate2Date(floor(as.numeric(as.character(DF$SampleDate)))) 
  else
      # Expects string input with format "yyyy-mm-dd" or "yyyy/mm/dd".
      #   Possibly extend to "dd-mm-yyyy" or "mm-dd-yyyy" since they are more common.
      #   But this requires an additional package such as 'lubridate' or 'anytime'.
      #DF$SampleDate <- as.Date(DF$SampleDate)
      DF$SampleDate <- as.Date(parse_date_time(as.character(DF$SampleDate),orders=c("dmy", "mdy", "ymd")))    
  return(DF)
  
}



#' @importFrom utils read.csv
readWellCoords <- function(input_file, valid_header, ...) {
 
  
  if (length(list(...)) == 0)
    DF = read.csv(input_file)
  else
    DF = read.csv(input_file, header = list(...)$header, sep = list(...)$sep, quote = list(...)$quote)
  
  
  coord_unit <- as.character(DF$CoordUnits[1])
  
  if (length(coord_unit) == 0 || is.na(coord_unit)) {
    coord_unit <- "metres"
  }
  
  # If no Aquifer field was found, add one with blank strings.
  if (!"aquifer" %in% tolower(names(DF))) {
    DF$Aquifer <- rep("",nrow(DF))
  }
  
   
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
    showNotification(msg, type = "error", duration = 10)
    return(NULL)
  } 
  
  # Make sure its not a factor, or we get an error when introducing "" 
  DF_extract$Aquifer <- as.character(DF$Aquifer)
  DF_extract$Aquifer[is.na(DF_extract$Aquifer)] <- ""
  
  return(list(data = DF_extract, coord_unit = coord_unit ))
  
}

