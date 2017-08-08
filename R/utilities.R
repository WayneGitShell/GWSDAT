



GWSDAT.excelDate2Date <- function(excelDate) 
{
  Date <- excelDate + as.Date("1900-01-01") - 2
  return(Date)
}



.my.tkdev <- function(hscale = 1, vscale = 1){
  win.metafile(width = 4 * hscale, height = 4 * vscale, restoreConsole = FALSE)
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




setupPPV2 <- function(){
  
  require(RDCOMClient)
  
  ppt <<- COMCreate("PowerPoint.Application")
  ppt[["Visible"]] <<- TRUE
  myPres <<- ppt[["Presentations"]]$add()
  mySlides <<- myPres[["Slides"]]
  
  
}



AddPlotPPV2 <- function(wmf_file, asp = FALSE){
  

  calledsetupPP <- FALSE
  
  CheckPPT <- exists('ppt',envir = globalenv())
  
  if (CheckPPT == FALSE) {
    setupPPV2() 
    calledsetupPP = TRUE 
    CheckPPT = TRUE
  }
  
  
  CheckPPT2 <- try(as.logical(ppt[['Visible']]),silent = TRUE)
  if (inherits(CheckPPT2, "try-error")) {CheckPPT2 <- FALSE}
  
  
  CheckPPT3 <- try(!is.null(myPres[['Name']]),silent = TRUE)
  if (inherits(CheckPPT3, "try-error")) {CheckPPT3 <- FALSE}
  
  
  if ((CheckPPT & CheckPPT2 & CheckPPT3) == FALSE) {
    setupPPV2()
    calledsetupPP = TRUE
  }
  
  
  
  mySlide <- myPres[["Slides"]]$add(as.integer(max(1,myPres[["Slides"]]$Count()+1)),as.integer(12))
  myShapes <- mySlide$Shapes()
  
  if (asp) {### Maintain aspect ratio in PowerPoint Plots!
    
    my.din <- par()$din
    
    if ((my.din[1]/my.din[2]) >= 1.4) {
      
      my.size <- c(10,(540 - 700*my.din[2]/my.din[1])/2,700,700*my.din[2]/my.din[1])
      
    } else {
      
      my.size <- c((720 - 500*my.din[1]/my.din[2])/2,20,500*my.din[1]/my.din[2],500)
      
    }
  } else {
    
    my.size <- c(10,10,700,500)
    
  }
  
  
  myShapes$AddPicture(wmf_file,0,-1,my.size[1],my.size[2],my.size[3],my.size[4]) 
  mySlide$Select()
  
  
}

