########################################################################################################################
GWSDAT.Prior<- function(lambda)
{
return(1)
 
}
#----------------------------------------------------------------------------------------------------------------------#



########################################################################################################################

GWSDAT.st.matrices <- function(x, xrange, ndims, nseg, bdeg = 3, pord = 2, computeP = TRUE) {

    # Compute a set of basis functions and a penalty matrix associated with x.
    # An intercept term and the main effect of any interaction terms are removed.
    n    <- nrow(x)
    if (missing(nseg)) nseg <- rep(7, 3)
    
    # Compute B-spline basis
    b <- list(length = 3)
    m <- vector(length = 3)
    for (i in 1:3) {
      
       b[[i]] <- GWSDAT.bbase(x[,i], xl = xrange[i , 1], xr = xrange[i, 2], nseg = nseg[i], deg = bdeg)
       m[i]   <- ncol(b[[i]])
    }

    B <- b[[1]]
    B <- t(apply(cbind(b[[1]], b[[2]]), 1,function(x) c(x[1:m[1]] %x% x[-(1:m[1])])))
    B <- t(apply(cbind(B,  b[[3]]), 1, 
                function(x) c(x[1:(m[1]*m[2])] %x% x[-(1:(m[1]*m[2]))])))
    
    result <- list(B = B, xrange = xrange, nseg = nseg, bdeg = bdeg, pord = pord)
                                                                
    if (computeP) {
      # Construct smoothness penalty matrices
      P <- list(length = 3)
      for (i in 1:3) {
        P[[i]] <- diff(diag(m[i]), diff = pord)
        P[[i]] <- t(P[[i]]) %*% P[[i]]
      }
      P[[1]] <- P[[1]] %x% diag(m[2])
      P[[2]] <- diag(m[2]) %x% P[[2]]
      P[[1]] <- P[[1]] %x% diag(m[3])
      P[[2]] <- P[[2]] %x% diag(m[3])
      P[[3]] <- diag(m[1]) %x% diag(m[2]) %x% P[[3]]
      pmat <- matrix(0, nrow = ncol(B), ncol = ncol(B))
      for (i in 1:3)
        pmat <- pmat + P[[i]]
      result$P <- pmat
    }
    invisible(result)
}
#----------------------------------------------------------------------------------------------------------------------#



########################################################################################################################

GWSDAT.bbase <- function(x, xl = min(x), xr = max(x), nseg = 10, deg = 3) {
# Construct B-spline basis
    dx <- (xr - xl) / nseg
    
    knots <- seq(xl - deg * dx, xr + deg * dx, by = dx)
    P <- outer(x, knots, GWSDAT.tpower, deg)
    n <- dim(P)[2]
    D <- diff(diag(n), diff = deg + 1) / (gamma(deg + 1) * dx ^ deg)
    B <- (-1) ^ (deg + 1) * P %*% t(D)
    B
}
#----------------------------------------------------------------------------------------------------------------------#



########################################################################################################################

GWSDAT.tpower <- function(x, t, p){
# Truncated p-th power function
    (x - t) ^ p * (x > t)
}
#----------------------------------------------------------------------------------------------------------------------#


########################################################################################################################
GWSDAT.RecomputeXtinv<-function(ContData,GWSDAT_Options){
##Function to recompute the huge Xtinv following loading a GWSDAT session. 

######################################## Prepare Data  #########################################################
names(ContData)[names(ContData)=="AggDate"]<-"AggDatekeep"
names(ContData)[names(ContData)=="SampleDate"]<-"AggDate"

form<-log(Result.Corr.ND)~XCoord+YCoord+AggDate-1
X<-model.matrix(form,ContData)
colnames(X)<-c("XCoord","YCoord","AggDate")
center <- colMeans(X, na.rm = TRUE)
X <- sweep(X, 2L, center)
scale<-apply(X,2,sd)
scale[1:2]<-rep(min(scale[1:2]),2)
X <- sweep(X, 2L, scale, "/")
Y<-model.response(model.frame(form,ContData))
#--------------------------------------------------------------------------------------------------------------#


######################################## Initialise ############################################################
NIG.a		<- GWSDAT_Options$PSplineVars$NIG.a       
NIG.b		<- GWSDAT_Options$PSplineVars$NIG.b   
nseg 		<- GWSDAT_Options$PSplineVars$nseg
pord 		<- GWSDAT_Options$PSplineVars$pord
bdeg		<- GWSDAT_Options$PSplineVars$bdeg
Trial.Lambda 	<- GWSDAT_Options$PSplineVars$Trial.Lambda
#--------------------------------------------------------------------------------------------------------------#
browser()
  mat    <- GWSDAT.st.matrices(X, xrange= xrange <- t(apply(X, 2, range)), ndims = 3, nseg = rep(nseg,3), pord=pord, bdeg=bdeg)
  #BestModel<-GWSDAT.compute.map.coef(mat$B, mat$P, Y, lambdas=Trial.Lambda , ig.a=NIG.a, ig.b=NIG.b, prior= GWSDAT.Prior)
  B<-mat$B
  DtD<-mat$P
 
  BtB <- t(B)%*%B
  P.eigen <- eigen(BtB+DtD)
  Mt <- t(P.eigen$vectors)*(1/sqrt(P.eigen$values))
  Q.svd <- svd(B%*%t(Mt),nu=ncol(B), nv=ncol(B))
  d <- c(pmin(Q.svd$d,1), rep(0, ncol(B)-length(Q.svd$d)))^2
  e <- 1-d
  Xtinv <- t(t(P.eigen$vectors)*sqrt(1/P.eigen$values))%*%Q.svd$v
  return(Xtinv)

}
#----------------------------------------------------------------------------------------------------------------------#



########################################################################################################################

# Computes the coefficient vector for the MAP estimate of lambda.
GWSDAT.compute.map.coef <- function(B, DtD, y, ig.a=1e-3, ig.b=1e-3, lambdas, prior=function(lambda) 1) {

  # Prepare and do the fancy linear algebra
  BtB <- t(B)%*%B
  P.eigen <- eigen(BtB+DtD)
  if (any(P.eigen$values<sqrt(.Machine$double.eps)*max(P.eigen$values)))
    stop("Singularity detected. No well-defined estimate.")
  Mt <- t(P.eigen$vectors)*(1/sqrt(P.eigen$values))
  Q.svd <- svd(B%*%t(Mt),nu=ncol(B), nv=ncol(B))
  d <- c(pmin(Q.svd$d,1), rep(0, ncol(B)-length(Q.svd$d)))^2
  e <- 1-d
  Xtinv <- t(t(P.eigen$vectors)*sqrt(1/P.eigen$values))%*%Q.svd$v
  sel <- 1:length(Q.svd$d)
  log.det.XtX <- sum(log(P.eigen$values))
  rank.D <- sum(e>sqrt(.Machine$double.eps))
  z <- drop(t(Q.svd$u)%*%y)

  # Function to compute the loglikelihood
  loglik <- function(lambda) {
    # Get the coefficient
    coef <- drop(Xtinv[,sel]%*%(z*sqrt(d[sel]) / (d[sel]+lambda*e[sel])))
    residuals <- y-B%*%coef
    # Get the posterior determinant
    log.post.det <- -sum(log(d+lambda*e))-log.det.XtX
    # Compute the log-posterior
    0.5 * rank.D * log(lambda) + 0.5 * log.post.det - (ig.a + length(y)/2) * log(2*ig.b + sum(y*residuals)) + log(prior(lambda))
  } 

  # Get the best lambda
  logliks <- sapply(lambdas, loglik)
  lambda <- lambdas[which.max(logliks)]
  # alpha coefficient the the best lambda
  alpha <- drop(Xtinv[,sel]%*%(z*sqrt(d[sel]) / (d[sel]+lambda*e[sel])))
  fitted <- drop(B %*% alpha)

  return(list(best.lambda=lambda,trial.lambdas=lambdas,logliks=logliks,alpha=alpha,fitted=fitted))

  ## Alternative when SEs are needed.
  ## Quantities for Standard error calc
  #post.ig.a <- ig.a + length(y)/2
  #post.ig.b <- ig.b + sum(y*(y-fitted))/2
  #return(list(	best.lambda=lambda,trial.lambdas=lambdas,logliks=logliks,alpha=alpha,fitted=fitted,
  #		post.ig.a=post.ig.a,post.ig.b=post.ig.b,Xtinv=Xtinv,e=e,d=d))


}
#----------------------------------------------------------------------------------------------------------------------#



########################################################################################################################

GWSDAT.PSplinetune <- function(ContData, GWSDAT_Options, verbose = interactive()){


######################################## Prepare Data  #########################################################

form <- log(Result.Corr.ND) ~ XCoord + YCoord + AggDate - 1
X <- model.matrix(form,ContData)

colnames(X) <- c("XCoord","YCoord","AggDate")
center <- colMeans(X, na.rm = TRUE)
X <- sweep(X, 2L, center)
scale <- apply(X, 2, sd)
scale[1:2] <- rep(min(scale[1:2]),2)
X <- sweep(X, 2L, scale, "/")
Y <- model.response(model.frame(form, ContData))
#--------------------------------------------------------------------------------------------------------------#


######################################## Initialise ############################################################
NIG.a		<- GWSDAT_Options$PSplineVars$NIG.a       
NIG.b		<- GWSDAT_Options$PSplineVars$NIG.b   
nseg 		<- GWSDAT_Options$PSplineVars$nseg
pord 		<- GWSDAT_Options$PSplineVars$pord
bdeg		<- GWSDAT_Options$PSplineVars$bdeg
Trial.Lambda 	<- GWSDAT_Options$PSplineVars$Trial.Lambda
#--------------------------------------------------------------------------------------------------------------#

mat    <- GWSDAT.st.matrices(X, xrange = xrange <- t(apply(X, 2, range)), ndims = 3, nseg = rep(nseg,3), pord=pord, bdeg=bdeg)
BestModel<-GWSDAT.compute.map.coef(mat$B, mat$P, Y, lambdas=Trial.Lambda , ig.a=NIG.a, ig.b=NIG.b, prior= GWSDAT.Prior)

if(verbose){

op<-par(no.readonly = TRUE);
par(mfrow=c(1,1))
plot(log(BestModel$trial.lambdas,base=10),BestModel$logliks)
par(op)

}



#--------------------------------------------------------------------------------------------------------------#

best.model<-list(
Lambda=BestModel$best.lambda,xrange=mat$xrange,nseg=nseg,ndims=3,bdeg = mat$bdeg,pord = mat$pord,scale=scale,center=center,
alpha=BestModel$alpha,fitted=BestModel$fitted)

##Alternative for SEs
#best.model<-list(
#Lambda=BestModel$best.lambda,xrange=mat$xrange,nseg=nseg,ndims=3,bdeg = mat$bdeg,pord = mat$pord,scale=scale,center=center,
#alpha=BestModel$alpha,fitted=BestModel$fitted,
#post.ig.a=BestModel$post.ig.a,post.ig.b=BestModel$post.ig.b ,Xtinv=BestModel$Xtinv,d=BestModel$d,e=BestModel$e
#)

class(best.model) <- "GWSDAT.PSpline"


Model.tune <- list(Trial.Lambda=Trial.Lambda,best.model=best.model)
	


return(Model.tune)

}
#----------------------------------------------------------------------------------------------------------------------#



########################################################################################################################

fitPSpline <- function(ContData,GWSDAT_Options){


names(ContData)[names(ContData) == "AggDate"]    <- "AggDatekeep"
names(ContData)[names(ContData) == "SampleDate"] <- "AggDate"


Model.tune <- try(GWSDAT.PSplinetune(ContData,GWSDAT_Options))



if(!inherits(Model.tune, "try-error")){


	#pred<-predict(Model.tune$best.model,newdata=ContData,se=TRUE)
	#ContData$ModelPred<-exp(pred$predicted)
	#ContData$Upper95<-exp(pred$predicted+1.96*pred$predicted.sd)
	#ContData$Lower95<-exp(pred$predicted-1.96*pred$predicted.sd)
	
	#Alternative to SEs
	pred<-predict(Model.tune$best.model,newdata=ContData,se=FALSE)
	ContData$ModelPred<-exp(pred$predicted)
	ContData$Upper95<-ContData$Lower95<-rep(NA,nrow(ContData))
	

	
	

}else{

	ContData$ModelPred<-rep(NA,nrow(ContData))
	ContData$Upper95<-rep(NA,nrow(ContData))
	ContData$Lower95<-rep(NA,nrow(ContData))


}

names(ContData)[names(ContData)=="AggDate"]<-"SampleDate"
names(ContData)[names(ContData)=="AggDatekeep"]<-"AggDate"

#### Legacy func from GWSDAT SVM.R. Need to check for NAPL only data sets. 
ContData$Result.Corr.ND[!is.finite(ContData$Result.Corr.ND)]<-NA #Wayne V3 coerce -inf to NA for NAPL only data sets. 

list(Cont.Data=ContData,Model.tune=Model.tune)

}
#----------------------------------------------------------------------------------------------------------------------#



########################################################################################################################

predict.GWSDAT.PSpline<-function(mod,newdata,se=FALSE){

X <- model.matrix(~XCoord+YCoord+AggDate-1,newdata)
X <- sweep(X, 2L, mod$center)
X <- sweep(X, 2L, mod$scale, "/")


mat<-GWSDAT.st.matrices(x=X, xrange=mod$xrange, ndims=mod$ndims,nseg=rep(mod$nseg,mod$ndims), bdeg = mod$bdeg, pord = mod$pord, computeP = FALSE)
B<-mat$B

result=list(predicted.sd=rep(NA,nrow(B)))


if(se){

	
     post.ig.a <- mod$post.ig.a
     post.ig.b <- mod$post.ig.b

     if (post.ig.a<=2) {
        result$predicted.sd <- rep(Inf, nrow(B))
     } else {
	result$predicted.sd <- sqrt((post.ig.b / (post.ig.a-2)) * ((B%*%mod$Xtinv)^2  %*% (1 / (mod$d + mod$Lambda * mod$e))))

     }
	
	result$predicted.sd<-drop(result$predicted.sd)
}



result$predicted=as.numeric(B %*% mod$alpha)
return(result)

}
#----------------------------------------------------------------------------------------------------------------------#
