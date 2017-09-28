

existsPPT <- function() {
  
  if (!require(RDCOMClient))
    return(FALSE)
  
  return(TRUE)
}


initPPT <- function() {
  
  if (!require(RDCOMClient))
    return(NULL)
  
  
  ppt <- RDCOMClient::COMCreate("PowerPoint.Application")
  ppt[["Visible"]] <- TRUE
  
  myPres <- ppt[["Presentations"]]$add()
  mySlides <- myPres[["Slides"]]
  
  return(list(ppt = ppt, pres = myPres, slides = mySlides))
  
}


addPlotPPT <- function(wmf_file, ppt_lst, width, height, ppi = 120) {
  
  slide <- ppt_lst$pres[["Slides"]]$add(as.integer(max(1, ppt_lst$pres[["Slides"]]$Count() + 1)), 
                                        as.integer(12))
  shapes <- slide$Shapes()
  
  # Translate width/height in inch to pixel.
  my.size <- c(1,20, ppi * width, ppi * height)
  
  shapes$AddPicture(wmf_file, 0, -1, my.size[1], my.size[2], my.size[3], my.size[4])
  slide$Select()
  
  return(ppt_lst)
}

