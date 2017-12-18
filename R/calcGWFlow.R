

delDirNeighbours <- function(tempDelDir){

  tr <- tempDelDir$delsgs
  my.list <- list()
  
  for (i in 1:nrow(tempDelDir$summary)) {
    my.list[[i]] <- sort(c(tr[,c("ind1")][which(tr[,c("ind2")] == i)],tr[,c("ind2")][which(tr[,c("ind1")] == i)]))
  }
  
  return(my.list)
}




calcGWFlow <- function(temp.GW) {


  if (nrow(temp.GW) < 3) {return(NULL)}

  if (any(duplicated(temp.GW[,c("XCoord","YCoord")]))) {
    
  	#warning("non unique well Coords")
	  new.temp.GW <- temp.GW[!duplicated(temp.GW[,c("XCoord","YCoord")]),,drop = FALSE]

  	if (nrow(new.temp.GW) < 3) {return(NULL)}

  	for (i in 1:nrow(new.temp.GW)) { 
      new.temp.GW$Result[i] <- mean(temp.GW$Result[temp.GW$XCoord == new.temp.GW$XCoord[i] & temp.GW$YCoord == new.temp.GW$YCoord[i]], na.rm = T)
  	}

    temp.GW <- new.temp.GW
  }
  
  temp.tr <- deldir(temp.GW$XCoord, temp.GW$YCoord, duplicate = "remove",frac = 0)

  temp.tr.nghbrs <- delDirNeighbours(temp.tr)
  temp.GW$R <- temp.GW$GradY <- temp.GW$GradX <- rep(NA,nrow(temp.GW))

  for (i in 1:nrow(temp.GW)) {

  	temp.lm <- lm(Result~XCoord+YCoord,temp.GW[c(i,temp.tr.nghbrs[[i]]),])
  	temp.GW$GradX[i] <- (-1)*temp.lm$coeff["XCoord"]
  	temp.GW$GradY[i] <- (-1)*temp.lm$coeff["YCoord"]
  	temp.GW$R[i] <- sqrt(temp.GW$GradX[i]^2 + temp.GW$GradY[i]^2)
  }


  temp.GW$RAD <- atan2(temp.GW$GradY,temp.GW$GradX)
  
  return(temp.GW)
}


evalGWFlow <- function(Agg_GW_Data) {
  

  #if (showProgress) {
  #  progress$set(value = PctDone, detail = paste("calculating groundwater"))
  #} 

  GW.Flows <- NULL

  if (!is.null(Agg_GW_Data)) {
    
    tryCatch(
      GW.Flows <- do.call('rbind', by(Agg_GW_Data, Agg_GW_Data$AggDate, calcGWFlow)),
      error = function(e) {
        showNotification(paste0("Failed to calculate groundwater flows: ", e$message), type = "error", duration = 10)
      })
    
    if (!is.null(GW.Flows)) {    
      GW.Flows$R <- GW.Flows$R/quantile(GW.Flows$R, p = 0.9, na.rm = T)
      GW.Flows$R[GW.Flows$R > 1] <- 1
      GW.Flows <- na.omit(GW.Flows)    
    }
  }

  return(GW.Flows)
}