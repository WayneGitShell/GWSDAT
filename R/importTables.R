

parseTable <- function(tbl = NULL, type = NULL, wells  = NULL, dsource = "") {
  
  if (is.null(tbl)) {
    stop("First argument \'tbl\' is missing\n")
    return(NULL)
  }
  
  if (is.null(type)) {
    stop("Need to specify \'type\' of table: \'contaminant\' or \'well\'\n")
    return(NULL)
  }
  
  if (type == "contaminant" && is.null(wells)) {
    stop("If type = \'contaminant\', need to specify the parameter \'wells\'.")
    return(NULL)
  }
  
  
  if (nrow(tbl) == 0)
    return(NULL)
  
  if (dsource == "excel")
    return(tbl)
   
  
  # Create empty buffer for valid entries
  val_buf <- tbl[-c(1:nrow(tbl)),]
  
  
  # Parse wells table.
  if (type == "wells") {
  
    duplicate_wells = c()
      
    # Loop over rows and check every column.
    for (i in 1:nrow(tbl)) {
      
      if (tbl$WellName[i] == "" || is.na(tbl$WellName[i])) next
      if (tbl$XCoord[i] == ""  || is.na(tbl$XCoord[i])) next
      if (tbl$YCoord[i] == ""  || is.na(tbl$YCoord[i])) next
      
      if (tbl$WellName[i] %in% val_buf$WellName) {
        duplicate_wells <- c(duplicate_wells, tbl$WellName[i])
        next
      }
      
      # If we made it until here, the row is ok. 
      val_buf <- rbind(val_buf, tbl[i,])
    }
    
    # Check if all wells are unique
    if (length(duplicate_wells) > 0) {
      msg <- paste("Found duplicate WellNames in Well Coordinate table: ", 
                   paste(duplicate_wells, collapse = ","), 
                   ". Only keeping first entry.")
      showNotification(msg, type = "warning", duration = 10)
    }
  }
  
  # Parse contaminant table.
  if (type == "contaminant") {
    
    # In some data sets spaces in WellName are present Well Table but not 
    # contaminant table: Delete spaces in both to synchronize.
    wells <- rm_spaces(as.character(unique(wells)))
    
    # Make sure the WellName is not a factor (will break rm_spaces)
    tbl$WellName <- as.character(tbl$WellName)
    
    # Some data sets have "Levels" instead of "Level" as unit for GW. 
    # Temporarily add "Levels" to units so they don't get lost.
    units <- tolower(c(conc_units, "Levels"))
    
    invalid_wells <- c()
    invalid_sampledates <- 0
    invalid_units <- 0
    invalid_flags <- 0
    
    #ptm <- proc.time()
    # Loop over rows and check every column.
    for (i in 1:nrow(tbl)) {
      
      
      if (tbl$WellName[i] == "") next
      
      # WellName must exist in provided 'wells' vector.
      if (!(rm_spaces(tbl$WellName[i]) %in% wells)) {
        invalid_wells <- c(invalid_wells, tbl$WellName[i])
        next
      }
      
      if (tbl$Constituent[i] == "") next
      
      # If some weird number or a string was put in, it will show up as <NA>.
      if (is.na(tbl$SampleDate[i])) {
        invalid_sampledates <- invalid_sampledates + 1
        next
      }
      
      if (tbl$Result[i] == "") next
      
      if (!(tolower(tbl$Units[i]) %in% units)) {
        invalid_units <- invalid_units + 1
        next
      }
      
      if (!(tbl$Flags[i] %in% conc_flags)) {
        invalid_flags <- invalid_flags + 1
        next
      }
      
      # If we made it until here, the row is ok. 
      val_buf <- rbind(val_buf, tbl[i,])
    }
    #print("Time to go through table (parseTable): ")
    #print(proc.time() - ptm)
    
    # Check if all wells are unique
    if (length(invalid_wells) > 0) {
      msg <- paste0("Found ", length(invalid_wells), " WellName not defined in Contaminant table.")
      showNotification(msg, type = "warning", duration = 10)
    }
    
    if (invalid_sampledates > 0) {
      msg <- paste0("Found ", invalid_sampledates, " invalid SampleDate(s) in Contaminant table.")
      showNotification(msg, type = "warning", duration = 10)
    }
    
    if (invalid_units > 0) {
      msg <- paste0("Found ", invalid_units, " invalid Units in Contaminant table.")
      showNotification(msg, type = "warning", duration = 10)
    }
    
    if (invalid_flags > 0) {
      msg <- paste0("Found ", invalid_flags, " invalid Flags in Contaminant table.")
      showNotification(msg, type = "warning", duration = 10)
    }
    
  }

  showNotification(paste0("Parsed ", nrow(val_buf), " row(s) from ", type, " table."), type = "message", duration = 10)

  if (nrow(val_buf) == 0)
    return(NULL)
  
  return(val_buf)
    
}

validateTable <- function(tbl) {
  
  if (is.null(tbl)) 
    return(FALSE)
  
  return(TRUE)
}
