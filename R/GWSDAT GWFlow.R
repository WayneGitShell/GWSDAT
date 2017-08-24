

GWSDAT.DelDir.Neighbours<-function(tempDelDir){

tr<-tempDelDir$delsgs
my.list<-list()

for(i in 1:nrow(tempDelDir$summary)){

my.list[[i]]<-sort(c(tr[,c("ind1")][which(tr[,c("ind2")]==i)],tr[,c("ind2")][which(tr[,c("ind1")]==i)]))

}


return(my.list)
}










computeGroundwater <- function(temp.GW) {


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

  temp.tr.nghbrs <- GWSDAT.DelDir.Neighbours(temp.tr)
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
