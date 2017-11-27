

existsPPT <- function() {
  
  sret <- requireNamespace("RDCOMClient", quietly = TRUE)

  if (!sret)
    cat("RDCOMClient package not installed: saving to Powerpoint is disabled. If on Windows, use install.packages(\"RDCOMClient\") to install it. ")
  
  return(sret)
  
}


initPPT <- function() {
  
  if (requireNamespace("RDCOMClient", quietly = TRUE)) {

    # Need to load RDCOMClient here, otherwise I get the following error: 
    #Warning: Error in createCOMReference: could not find function "createCOMReference"
    #Stack trace (innermost first):
    #  73: COMCreate
    #  72: getCOMInstance
    #  71: RDCOMClient::COMCreate
    
    require(RDCOMClient)
    
    ppt <- RDCOMClient::COMCreate("PowerPoint.Application")
    ppt[["Visible"]] <- TRUE
  
    myPres <- ppt[["Presentations"]]$add()
    mySlides <- myPres[["Slides"]]
  
    return(list(ppt = ppt, pres = myPres, slides = mySlides))
  }
  
  return(NULL)
  
}


addPlotPPT <- function(imgfile, ppt_lst, width, height) {
  
  slide <- ppt_lst$pres[["Slides"]]$add(as.integer(max(1, ppt_lst$pres[["Slides"]]$Count() + 1)), 
                                        as.integer(12))
  shapes <- slide$Shapes()
 
  shapes$AddPicture(imgfile, LinkToFile = FALSE, SaveWithDocument = TRUE, 
                    Top = 1, Left = 20, Width = width * 0.7, Height = height * 0.7)
  slide$Select()
  
  return(ppt_lst)
}

