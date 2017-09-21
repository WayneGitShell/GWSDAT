



#' @importFrom sp point.in.polygon
predictValues <- function(model, AggDate, eval.df, type = c("predicted","lower","upper","sd")) {
  
  
  type=match.arg(type)
  
  if(length(AggDate)!=1){stop("Agg Date must be length 1")}
  
  x0 = sort(unique(eval.df[,1]))
  y0 = sort(unique(eval.df[,2]))
  my.df <- expand.grid(XCoord=x0,YCoord=y0)
  
  if(!is.null(model)){
    
    my.df$AggDate <- as.numeric(AggDate)
    my.df$InOut <- !sp::point.in.polygon(my.df$XCoord,my.df$YCoord,eval.df[chull(eval.df[,1:2]),1],eval.df[chull(eval.df[,1:2]),2]) == 0
    my.df$pred <- rep(NA,nrow(my.df))
    
    temppred <- predict(model, newdata = my.df[my.df$InOut, c("XCoord","YCoord","AggDate")], se=type!="predicted")
    my.df$pred[my.df$InOut] <- temppred$predicted
    my.df$pred.sd[my.df$InOut] <- temppred$predicted.sd
    
    if(type=="lower"){my.df$pred <- my.df$pred - 1.96 * my.df$pred.sd}
    if(type=="upper"){my.df$pred <- my.df$pred + 1.96 * my.df$pred.sd}
    if(type=="sd")   {my.df$pred <- my.df$pred.sd}
    
  } else {
    
    my.df$pred <- rep(NA,nrow(my.df))
    
  }
  
  list(x = x0,y = y0,z = matrix(my.df$pred,nrow = length(x0), ncol = length(y0)))
  
}

