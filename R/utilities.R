
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


#' @import RDCOMClient
setupPPV2 <- function(){
  
  ppt <<- COMCreate("PowerPoint.Application")
  ppt[["Visible"]] <<- TRUE
  
  myPres <<- ppt[["Presentations"]]$add()
  mySlides <<- myPres[["Slides"]]
  
  
}



AddPlotPPV2 <- function(wmf_file, width, height){
  

  calledsetupPP <- FALSE
  
  if (is.null(ppt)) {
    setupPPV2() 
    calledsetupPP <- TRUE 
    CheckPPT <- TRUE
  }
  
  # CheckPPT <- exists('ppt', envir = globalenv())
  
  #if (CheckPPT == FALSE) {
  #  setupPPV2() 
  #  calledsetupPP = TRUE 
  #  CheckPPT = TRUE
  #}
  
  
  CheckPPT2 <- try(as.logical(ppt[['Visible']]), silent = TRUE)
  if (inherits(CheckPPT2, "try-error")) {CheckPPT2 <- FALSE}
  
  
  CheckPPT3 <- try(!is.null(myPres[['Name']]),silent = TRUE)
  if (inherits(CheckPPT3, "try-error")) {CheckPPT3 <- FALSE}
  
  
  if ((CheckPPT & CheckPPT2 & CheckPPT3) == FALSE) {
    setupPPV2()
    calledsetupPP = TRUE
  }
  
  
  
  mySlide <- myPres[["Slides"]]$add(as.integer(max(1,myPres[["Slides"]]$Count()+1)),as.integer(12))
  myShapes <- mySlide$Shapes()
  
 
  #
  # Won't work, because I plot directly to wmf not a screen.
  #
  #
  # if (asp) {### Maintain aspect ratio in PowerPoint Plots!
  #   
  #   # Get device dimension
  #   # Note: This will not match the dimension passed to a image device, such
  #   # as win.metafile(). Thus it will only work if something was previously 
  #   # plotted to the screen.
  #   my.din <- par()$din
  #   
  #   # translate inch to px
  #   if ((my.din[1]/my.din[2]) >= 1.4) {
  #
  #     my.size <- c(10,(540 - 700*my.din[2]/my.din[1])/2,700,700*my.din[2]/my.din[1])
  #     
  #   } else {
  #     
  #     my.size <- c((720 - 500*my.din[1]/my.din[2])/2,20,500*my.din[1]/my.din[2],500)
  #     
  #   }
  # } else {
  #   
  #   my.size <- c(10,10,700,500)
  #   
  # }
  
  # Just assume this pixel density per inch
  ppi = 120
  
  # Translate width/height in inch to pixel.
  my.size <- c(1,20, ppi * width, ppi * height)
  
  myShapes$AddPicture(wmf_file, 0, -1, my.size[1], my.size[2], my.size[3], my.size[4]) 
  mySlide$Select()
  
  
}


