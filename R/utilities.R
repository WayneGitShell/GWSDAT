
expandpoly <- function(mypol, fact) {

  m1 <- mean(mypol[, 1])
  m2 <- mean(mypol[, 2])
  cbind((mypol[, 1] - m1) * fact + m1, (mypol[, 2] - m2) * fact + m2)

}


getDataInfo <- function(csite_list) {

  data_list <- list()

  # Loop over the data list and extract some useful information.
  for (i in 1:length(csite_list)) {

    sname <- csite_list[[i]]$GWSDAT_Options$SiteName
    aname <- csite_list[[i]]$Aquifer
    cnames <- csite_list[[i]]$All.Data$cont_names
    wnames <- csite_list[[i]]$All.Data$sample_loc$names
    donotdel <- FALSE
    
    # If the DO_NOT_MODIFY Flag exists, copy it.
    if (!is.null(csite_list[[i]]$DO_NOT_MODIFY))
      donotdel <- csite_list[[i]]$DO_NOT_MODIFY
        
    if (is.null(data_list[[sname]]))
      data_list[[sname]] <- list(Aquifer = aname, csite_idx = i, 
                                 contaminants = cnames,
                                 wells = wnames,
                                 do_not_del = donotdel)
    else {
      data_list[[sname]]$Aquifer[[length(data_list[[sname]]$Aquifer) + 1]] <- aname
      data_list[[sname]]$csite_idx = c(data_list[[sname]]$csite_idx, i)
      data_list[[sname]]$contaminants = cnames
      data_list[[sname]]$wells = wnames
      data_list[[sname]]$do_not_del = donotdel
      
    }
  }

  return(data_list)
}


getValidDataName <- function(csite_list = NULL, template = "Area", propose_name = NULL) {

  if (is.null(csite_list))
    return(template)

  # If a name was provided (proposed_name not NULL), check if it already in use.
  if (!is.null(propose_name)) {
    name_conflicted <- FALSE
    
    for (j in 1:length(csite_list)) {
      if (propose_name == csite_list[[j]]$GWSDAT_Options$SiteName) {
        name_conflicted <- TRUE
        break
      }
    }
    
    # Return 'propose_name' if no equal name found in 'csite_list'.
    if (!name_conflicted)
      return(propose_name)
  }
  
  # Try a new name using 'template' as prefix for the name.
  for (i in 1:1000) {

    propose_name <- paste0(template, " ", i)
    name_conflicted <- FALSE

    # Loop over data sets and check if name already exists.
    for (j in 1:length(csite_list)) {
      if (propose_name == csite_list[[j]]$GWSDAT_Options$SiteName) {
        name_conflicted <- TRUE
        break
      }
    }

    if (!name_conflicted) break

  }

  return(propose_name)

}


getDataIndexByID <- function(csite_list, data_id) {
  
  for (i in 1:length(csite_list))
    if (csite_list[[i]]$data_id == data_id)
      return(i)
  
  return(-1)
}

createDataID <- function(csite_list) {
  
  new_id <- 0
  
  # Loop as long as no unique data id can be found. 
  while (1) {
    new_id <- sample.int(100000, 1)
    
    # Check if the new id already exists.
    for (i in 1:length(csite_list))
      if (csite_list[[i]]$data_id == new_id) 
        new_id = -1  # flag as existing   
        
    if (new_id != -1)
      break  # leave while
  }
  
  return(new_id)
}



excelDate2Date <- function(excelDate) {

  Date <- excelDate + as.Date("1900-01-01") - 2
  return(Date)
}



rm_spaces <- function(x){

  #Function to remove trailing and leading spaces!
  if (!is.character(x)) { stop("not of class character") }

  x <- sub('[[:blank:]]+?','',x)
  x <- sub(" *$","",x)

  return(x)
}



existsNAPL <- function(All.Data, well, solute) {

  Well.Data <- All.Data$Cont.Data[as.character(All.Data$Cont.Data$WellName) ==
                                    well & All.Data$Cont.Data$Constituent == solute,]

  NAPL.Present <- any("napl" %in% tolower(as.character(Well.Data$Result))) ||
    nrow(All.Data$NAPL.Thickness.Data[as.character(All.Data$NAPL.Thickness.Data$WellName) == well,]) > 0

  if (is.na( NAPL.Present)) { NAPL.Present <- FALSE }

  return(NAPL.Present)

}

# Convert array of strings in 'astr' to a single string separated by 'collapse', 
# but only include the first 'limit' elements.
pasteLimit <- function(astr, limit = NULL, collapse = ", ") {
   
    if (is.null(limit))
        return(paste(astr, collapse = collapse))
    
    if (!is.numeric(limit))
        return("Error: limit must be an integer")
    
    limit <- as.integer(limit)
    
    if (limit > length(astr))
        return(paste(astr, collapse = collapse))

    outstr <- paste(astr[1:limit], collapse = collapse)
    outstr <- paste0(outstr, ", ... (", length(astr), ")")
    
    return(outstr)
}