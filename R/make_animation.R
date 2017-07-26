


make_animation <- function(panel, addtoPPt = FALSE){
  
  keep_timestep <- panel$timestep
  browser()
  graphics.off()
  
  .SavedPlots <<- NULL
  
  windows(record = T,width = 11, height = 8)
  
  try(bringToTop())
  
  for (i in panel$timestep_range[1]:panel$timestep_range[2]) {
  
      panel$timestep <- i
      
      try(Plot_ImagePlot(panel))
      
      if (addtoPPt) { 
        AddPlotPPV2(panel, asp = TRUE) 
      }
    
  }
  
  # Might not be necessary, because panel is only changed with <<-
  panel$jjj <- keep_timestep
  
  #return(panel)
  
}


AddPlotPPV2 <- function(panel,asp=FALSE){
  
  require(RDCOMClient)
  
  
  calledsetupPP <- FALSE
  CheckforNoPlot <- is.na(match(dev.cur(), dev.list(), NA))
  if(CheckforNoPlot){rp.messagebox("No active plot to export.", title = "Error"); return(panel)}
  CheckPPT<-exists('ppt',envir=globalenv())
  
  if(CheckPPT == FALSE){setupPPV2(); calledsetupPP = TRUE; CheckPPT = TRUE}
  
  
  CheckPPT2 <- try(as.logical(ppt[['Visible']]),silent=TRUE)
  if(inherits(CheckPPT2, "try-error")){CheckPPT2<-FALSE}
  
  
  CheckPPT3 <- try(!is.null(myPres[['Name']]),silent=TRUE)
  if(inherits(CheckPPT3, "try-error")){CheckPPT3<-FALSE}
  
  
  if((CheckPPT & CheckPPT2 & CheckPPT3) == FALSE){setupPPV2(); calledsetupPP = TRUE}
  
  
  OutputGraphics <- panel$GWSDAT_Options$OutputGraphics
  if(is.null(OutputGraphics)){OutputGraphics="wmf"}
  
  mytemp<-tempfile()
  mytemp<-paste(mytemp,OutputGraphics ,sep='.')
  mytemp<- gsub("/", "\\\\", as.character(mytemp))
  
  savePlot(mytemp,type=OutputGraphics,restoreConsole = FALSE)
  if(file.exists(paste(mytemp,OutputGraphics,sep="."))){file.rename(paste(mytemp,OutputGraphics,sep="."),mytemp)}
  
  mySlide<-myPres[["Slides"]]$add(as.integer(max(1,myPres[["Slides"]]$Count()+1)),as.integer(12))
  myShapes<-mySlide$Shapes()
  
  if(asp){### Maintain aspect ratio in PowerPoint Plots!
    my.din<-par()$din
    
    if((my.din[1]/my.din[2])>=1.4){
      
      my.size<-c(10,(540-700*my.din[2]/my.din[1])/2,700,700*my.din[2]/my.din[1])
      
    }else{
      
      my.size<-c((720-500*my.din[1]/my.din[2])/2,20,500*my.din[1]/my.din[2],500)
      
    }
  }else{
    
    my.size<-c(10,10,700,500)
    
  }
  
  
  myShapes$AddPicture(mytemp,0,-1,my.size[1],my.size[2],my.size[3],my.size[4]) 
  mySlide$Select()
  browser()
  
  try(file.remove(mytemp))
  try(rm(mytemp))
  #return(panel)
}


setupPPV2<-function(){
  
  require(RDCOMClient)
  
  ppt <<- COMCreate("PowerPoint.Application")
  ppt[["Visible"]] <<- TRUE
  myPres <<- ppt[["Presentations"]]$add()
  mySlides <<- myPres[["Slides"]]
  
  
}
