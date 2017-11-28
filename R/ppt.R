

existsPPT <- function() {
  
  # This would be the way to go, if RDCOMClient would be part of the 
  # official CRAN, but it is not (anymore), thus I can't put RDCOMClient
  # into the Suggests tag of the DESCRIPTIONS file and requireNamespace()
  # throws a warning with devtools::check(). 
  # does_exist <- requireNamespace("RDCOMClient", quietly = TRUE)

  
  tryCatch(
    res <- find.package("RDCOMClient"),
    error = function(e) {
      cat("Info: RDCOMClient package not installed: Saving to Powerpoint is disabled. If on Windows, use install.packages(\"RDCOMClient\") to install it.\n")
      
    }
  )
  
  if (exists("res"))
    return(TRUE)
  else return(FALSE)

  
}


initPPT <- function() {
  
  if (existsPPT()) {
    
    # Important: RDCOMClient needs to be loaded (require(RDCOMClient)) prior to
    # running GWSDAT::launchApp() or the following code will fail with the error below.
    #
    # require(RDCOMClient) can not be called here, or GWSDAT will not pass CRAN submission.
    #
    # Warning: Error in createCOMReference: could not find function "createCOMReference"
    # Stack trace (innermost first):
    #   73: COMCreate
    #   72: getCOMInstance
    #   71: RDCOMClient::COMCreate
    
    
    
    tryCatch(
      #ppt <- RDCOMClient::COMCreate("PowerPoint.Application"),
      ppt <- RDCOMClient::COMCreate("PowerPoint.Application"),
      error = function(e) {
        showNotification("Initializing RDCOMClient failed: Package is probably not loaded. Use require(RDCOMClient) before launching the app.", type = "error", duration = 10)    
        
      }
    )
    
    # Variable 'ppt' will not exist if an error occurs in the tryCatch() above.
    if (exists("ppt")) {  
      ppt[["Visible"]] <- TRUE
  
      myPres <- ppt[["Presentations"]]$add()
      mySlides <- myPres[["Slides"]]
  
      return(list(ppt = ppt, pres = myPres, slides = mySlides))
    }
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

