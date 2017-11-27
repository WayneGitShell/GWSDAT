
###### Barycentric Interpolation Function ####


#' @importFrom Matrix sparseMatrix
#' @importFrom sp point.in.polygon
# #' @importFrom geometry delaunay
interpBary <- function(model,AggDate,my.area,type=c("Predicted","Lower 95% CI","Upper 95% CI","% sd","IQR/2")) {
  
  
  type <- match.arg(type)
  if(length(AggDate)!=1){stop("Agg Date must be length 1")}
  
  
  my.area <- my.area[chull(my.area),,drop=F]
  my.exp.area <- expandpoly(my.area, fact = 1.15)
  my.area <- expandpoly(my.area, fact = 1.05)
  colnames(my.area) <- c("XCoord", "YCoord")
  
  
  
  x0<-seq(min(my.area[,"XCoord"]),max(my.area[,"XCoord"]),length=100)
  y0<-seq(min(my.area[,"YCoord"]),max(my.area[,"YCoord"]),length=100)
  
  x0<-sort(unique(c(x0,my.area[,1])))
  y0<-sort(unique(c(y0,my.area[,2])))
  
  pred.df<-expand.grid(XCoord=x0,YCoord=y0)
  pred.df$AggDate<-as.numeric(AggDate)
  pred.df$pred<-rep(NA,nrow(pred.df))
  
  
  if(!is.null(model)){
    
    ####################### eval.df ####################################
    colnames(my.exp.area)<-c("XCoord","YCoord")
    eval.df<-gridpts(my.exp.area,250)
    eval.df<-data.frame(XCoord=eval.df[,1],YCoord=eval.df[,2])
    eval.df<-rbind(eval.df,my.exp.area)
    eval.df$AggDate=as.numeric(AggDate)
    
    
    
    temppred<-predict(model,newdata=eval.df,se=type!="Predicted")
    eval.df$pred<-temppred$predicted
    eval.df$pred.sd<-temppred$predicted.sd
    #eval.df<<-eval.df
    #print("eval.df<<-eval.df")
    if(type=="Lower 95% CI"){eval.df$pred<-eval.df$pred-1.96*eval.df$pred.sd}
    if(type=="Upper 95% CI"){eval.df$pred<-eval.df$pred+1.96*eval.df$pred.sd}
    if(type=="% sd")   {eval.df$pred<-100*(exp(eval.df$pred.sd)-1)}
    ### Daniel's approx to sd(exp(x)). 
    #if(type=="sd")   {eval.df$pred<-exp(eval.df$pred + eval.df$pred.sd^2)*sqrt(1 - exp(-eval.df$pred.sd^2))}
    if(type=="IQR/2")   {eval.df$pred<-0.5*(exp(qnorm(p=c(0.75), mean = eval.df$pred, sd = eval.df$pred.sd))-exp(qnorm(p=c(0.25), mean = eval.df$pred, sd = eval.df$pred.sd)))}
    
    
    ####################### pred.df ####################################
    
    pred.df$InOut <- !sp::point.in.polygon(pred.df$XCoord,pred.df$YCoord,my.area[,1],my.area[,2]) == 0
    
    predred.df <- pred.df[pred.df$InOut,]
    
    
    #
    # The following code needs attention: 
    #  - delaunayn() will generate warnings/errors if the set of points is not
    #    convex (check before or just catch). 
    #  - As a consequence, calls to contourLines() will produce warnings.

    dn <- try(delaunayn(eval.df[,c("XCoord","YCoord")]), silent = T)
    
    if (!inherits(dn, "try-error")) {
      tri <- tsearch(eval.df[,"XCoord"], eval.df[,"YCoord"], dn ,predred.df[,"XCoord"] ,
                     predred.df[,"YCoord"], bary = T) 
      active <- dn[tri$idx,] 
      
      
      M <- Matrix::sparseMatrix(i = rep(1:nrow(predred.df), each = 3), 
                                j = as.numeric(t(active)), x = as.numeric(t(tri$p)),
                                dims = c(nrow(predred.df), length(eval.df$pred))) 
      
      predred.df$pred <- as.numeric(M %*% eval.df$pred)
      
      
      pred.df$pred[pred.df$InOut] <- predred.df$pred
    } else {
      cat("Encountered github issue #123 associated to delaunay(). Coordinates lack convex hull.\n")
    }    
    
  }
  
  out <- list(x = x0, y = y0, z = matrix(pred.df$pred,nrow = length(x0), ncol = length(y0)))
  
  return(out)
  
}

