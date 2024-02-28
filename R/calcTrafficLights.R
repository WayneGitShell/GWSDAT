

calcTrafficLights <- function(All.Data, Fitted.Data, smThreshSe = 1.1512, smMethod = "aicc") {

  ND.Beta.Check <- function(Cont.Data,All.Time.Evals){
  
    if (nrow(Cont.Data) <= 1) {
      return(rep(NA,length(All.Time.Evals)))
    }
    Cont.Data <- Cont.Data[order(Cont.Data$SampleDate),]
    Cont.Data$SampleDate[which(Cont.Data$SampleDate == max(Cont.Data$SampleDate))]<-
    Cont.Data$AggDate[which(Cont.Data$SampleDate == max(Cont.Data$SampleDate))]
    #print(Cont.Data$ND)
    
    ind.minus1<-approx(Cont.Data$SampleDate,as.numeric(Cont.Data$ND),xout=All.Time.Evals-1)$y
    ind<-approx(Cont.Data$SampleDate,as.numeric(Cont.Data$ND),xout=All.Time.Evals)$y
    ind.plus1<-approx(Cont.Data$SampleDate,as.numeric(Cont.Data$ND),xout=All.Time.Evals+1)$y
    cl<-cbind(ind.minus1,ind,ind.plus1)
    lookup<-apply(cl,1,mean,na.rm=T)
    lookup[is.nan(lookup)]<-NA
    
    return(floor(lookup))
  
  }
  
  
  Wrap.ND.Beta.Check<-function(Cont.Data,All.Time.Evals){
  
    out<-try(ND.Beta.Check(Cont.Data,All.Time.Evals),silent=FALSE)
    
    if(!inherits(out, "try-error")){
    	return(out)
    }else{
    	return(rep(NA,length(All.Time.Evals)))
    }
  
  }
  

  
  GWSDAT.sm.derivative <- function(x, y, h, eval.points = seq(min(x), max(x), length = 50)) {
  
    n  <- length(x)
    ne <- length(eval.points)
    wd <- matrix(rep(eval.points, rep(n, ne)), ncol = n, byrow = T)
    wd <- wd - matrix(rep(x, ne), ncol = n, byrow = T)
    w  <- exp(-.5 * (wd/h)^2)
  
    s0 <-  w         %*% rep(1,n)
    s1 <- (w * wd)   %*% rep(1,n)
    s2 <- (w * wd^2) %*% rep(1,n)

    w  <-  w * (wd * matrix(rep(s0, n), ncol = n) - matrix(rep(s1, n), ncol = n))
    w  <-  w / (matrix(rep(s2, n), ncol = n) * matrix(rep(s0, n), ncol = n)
                    - matrix(rep(s1, n), ncol = n)^2)
    est <- -as.vector(w %*% y)
    invisible(list(eval.points = eval.points, estimate = est))
        
  }
  

  #' @importFrom sm sm.regression
  sm.func <- function(x, All.Time.Evals, smThreshSe, smMethod) {
  
    x.obs<-as.numeric(x$SampleDate) #input data is still non-agg data!
    y.obs<-log(x$Result.Corr.ND)
    out.upper<-rep(NA,length(All.Time.Evals))
    out.Betas<-rep(NA,length(All.Time.Evals))
    out.index<-match(as.numeric(x$AggDate),as.numeric(All.Time.Evals)) #outindex matches the aggregate date!
    out.index<-range(out.index,na.rm=T)[1]:range(out.index,na.rm=T)[2]
    
    
    if(length(x.obs)<3){return(list(Betas=out.Betas,trend.upper.lim=out.upper,h=NA))}
    if(sum(!x$ND)<2){return(list(Betas=out.Betas,trend.upper.lim=out.upper,h=NA))}
    
    sm.fit <- try(sm::sm.regression(x.obs,y.obs,display = "none",method=smMethod,verbose=0),silent=TRUE)
    
    
    if(inherits(sm.fit, "try-error")){
    
    	return(list(Betas=out.Betas,trend.upper.lim=out.upper,h=NA))
    }
    
    if(any(is.nan(sm.fit$estimate)) || any(is.na(sm.fit$estimate)) || any(is.nan(sm.fit$se)) || any(is.na(sm.fit$se))){
    
    	return(list(Betas=out.Betas,trend.upper.lim=out.upper,h=NA))
    
    }
    
    
    sm.fit <- sm::sm.regression(x.obs,y.obs,display = "none",eval.points = All.Time.Evals,h=sm.fit$h,verbose=0)
    sm.fit$estimate[sm.fit$se>smThreshSe]<-NA
    sm.est<-exp(sm.fit$estimate)
    sm.95up<-exp(sm.fit$estimate+2*sm.fit$se)
    out.upper[out.index]<-sm.95up[out.index]
    
    
    
    # Calculate the derivaties.
    Betas <- try(matrix(GWSDAT.sm.derivative(x = x.obs, y = y.obs, h = sm.fit$h, 
                                             eval.points = as.numeric(All.Time.Evals))$estimate, ncol = 1), silent = TRUE)
    
    if (!inherits(Betas, "try-error")) {
      out.Betas[out.index] <- Betas[out.index]
      out.Betas[is.na(sm.fit$estimate)] <- NA # if se>smThreshSe then dont give Beta estimate!
    }
    
    return(list(Betas = out.Betas, trend.upper.lim = out.upper, h = sm.fit$h))
    
  }
  
  





All.Time.Evals  <- All.Data$All_Agg_Dates
sample_loc_names       <- All.Data$sample_loc$names




tr<-try(by(data=All.Data$Cont.Data,INDICES=list(Wells=All.Data$Cont.Data$WellName,Conts=All.Data$Cont.Data$Constituent),FUN=sm.func,All.Time.Evals=All.Time.Evals,smThreshSe,smMethod))

Beta.check.ND<-try(by(data=All.Data$Cont.Data,INDICES=list(Wells=All.Data$Cont.Data$WellName,Conts=All.Data$Cont.Data$Constituent),FUN=Wrap.ND.Beta.Check,All.Time.Evals=All.Time.Evals))

Abs.Thresh.Check<-my.Betas<-my.uppers<-my.Beta.ND.Check<-array(NA,dim=c(length(sample_loc_names), length(All.Data$cont_names), length(All.Time.Evals)))

dimnames(Abs.Thresh.Check)<-dimnames(my.uppers)<-dimnames(my.Betas) <- 
dimnames(my.Beta.ND.Check)<-list(sort(sample_loc_names, decreasing = TRUE), sort(All.Data$cont_names), as.character(All.Time.Evals))

my.h<-matrix(NA,nrow = length(sample_loc_names), ncol = length(All.Data$cont_names))
rownames(my.h) <- sample_loc_names
colnames(my.h) <- All.Data$cont_names



if(!inherits(tr, "try-error")){

	for(i in dimnames(my.uppers)[[1]]){
	for(j in dimnames(my.uppers)[[2]]){
	
		my.uppers[i,j,]        <- if(!inherits(try(tr[i,j],silent=T),"try-error") && !is.null(tr[i,j][[1]]$trend.upper.lim))           {tr[i,j][[1]]$trend.upper.lim}else{NA}
		my.Betas[i,j,]         <- if(!inherits(try(tr[i,j],silent=T),"try-error") && !is.null(tr[i,j][[1]]$Betas))                     {tr[i,j][[1]]$Betas}          else{NA}
		my.Beta.ND.Check[i,j,] <- if(!inherits(try(tr[i,j],silent=T),"try-error") && !is.null(Beta.check.ND[i,j][[1]]))                {Beta.check.ND[i,j][[1]]}     else{NA}
		my.h[i,j]              <- if(!inherits(try(tr[i,j],silent=T),"try-error") && !is.null(tr[i,j][[1]]$h))    		       {tr[i,j][[1]]$h}              else{NA}
	}
	}
}


my.Betas[!is.finite(my.Betas)]<-NA
my.uppers[!is.finite(my.uppers)]<-NA
my.Beta.ND.Check[!is.finite(my.Beta.ND.Check)]<-NA





################ Abs Threshold Calc ##########################################################



ND.Data<- All.Data$Cont.Data[All.Data$Cont.Data$ND==TRUE,]
D.Data <- All.Data$Cont.Data[All.Data$Cont.Data$ND==FALSE,]

if(nrow(ND.Data)>0){

	temp.AggDate<-as.character(ND.Data$AggDate)
	temp.Conts<-as.character(ND.Data$Constituent)
	temp.Wells<-as.character(ND.Data$WellName)

	for(i in 1:nrow(ND.Data)){

	try(Abs.Thresh.Check[temp.Wells[i],temp.Conts[i],temp.AggDate[i]]<- -1)

	}

}


if(nrow(D.Data)>0){

	temp.AggDate<-as.character(D.Data$AggDate)
	temp.Conts<-as.character(D.Data$Constituent)
	temp.Wells<-as.character(D.Data$WellName)

	for(i in 1:nrow(D.Data)){
	
		temp.val<-Abs.Thresh.Check[temp.Wells[i],temp.Conts[i],temp.AggDate[i]]

		if(is.finite(D.Data$Result.Corr.ND[i])){
		if(is.na(temp.val) || temp.val<D.Data$Result.Corr.ND[i]){

			try(Abs.Thresh.Check[temp.Wells[i],temp.Conts[i],temp.AggDate[i]]<- D.Data$Result.Corr.ND[i])

		}
		}
	}

}

try(rm(temp.AggDate,temp.Conts,temp.Wells,temp.val))



try(Abs.Thresh.Check[!is.finite(Abs.Thresh.Check)]<-NA) 

#--------------------------------------------------------------------------------------------#

return(list(Betas=my.Betas,Smooth.Upper.lims=my.uppers,Beta.ND.Check=my.Beta.ND.Check,h=my.h,Abs.Thresh.Check=Abs.Thresh.Check))

}
