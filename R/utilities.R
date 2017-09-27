
expandpoly <- function(mypol, fact) {

  m1 <- mean(mypol[, 1])
  m2 <- mean(mypol[, 2])
  cbind((mypol[, 1] - m1) * fact + m1, (mypol[, 2] - m2) * fact + m2)

}


getDataInfo <- function(csite_list) {

  data_list <- list()

  for (i in 1:length(csite_list)) {

    sname <- csite_list[[i]]$GWSDAT_Options$SiteName
    aname <- csite_list[[i]]$Aquifer


    if (is.null(data_list[[sname]]))
      data_list[[sname]] <- list(Aquifer = aname, csite_idx = i)
    else {
      data_list[[sname]]$Aquifer[[length(data_list[[sname]]$Aquifer) + 1]] <- aname
      data_list[[sname]]$csite_idx = c(data_list[[sname]]$csite_idx, i)
    }
  }

  return(data_list)
}


getValidDataName <- function(csite_list = NULL, template = "Area") {

  if (is.null(csite_list))
    return(template)

  propose_name <- NULL

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


