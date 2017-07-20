



GWSDAT.sigest <- function(x, data = NULL, frac = 0.6, na.action = na.omit, scaled = TRUE){
  
        call <- match.call()
        m <- match.call(expand.dots = FALSE)
        if (is.matrix(eval(m$data, parent.frame()))) 
            m$data <- as.data.frame(data)
        m$formula <- m$x
        m$x <- NULL
        m$scaled <- NULL
        m$frac <- NULL
        m[[1]] <- as.name("model.frame")
        m <- eval(m, parent.frame())
        Terms <- attr(m, "terms")
        attr(Terms, "intercept") <- 0
        x <- model.matrix(Terms, m)
        if (length(scaled) == 1) 
            scaled <- rep(scaled, ncol(x))
        if (any(scaled)) {
            remove <- unique(c(which(labels(Terms) %in% names(attr(x, 
                "contrasts"))), which(!scaled)))
            scaled <- !attr(x, "assign") %in% remove
        }
	
        ret <- GWSDAT.sigest.mat(x, scaled = scaled, frac = frac, na.action = na.action)
        return(ret)
    }



GWSDAT.sigest.mat <- function(x, frac = 0.25, scaled = TRUE, na.action = na.omit){
	set.seed(1)
        x <- na.action(x)
        if (length(scaled) == 1) 
            scaled <- rep(scaled, ncol(x))
        if (any(scaled)) {
            co <- !apply(x[, scaled, drop = FALSE], 2, var)
            if (any(co)) {
                scaled <- rep(FALSE, ncol(x))
                warning(paste("Variable(s)", paste("`", colnames(x[, 
                  scaled, drop = FALSE])[co], "'", sep = "", 
                  collapse = " and "), "constant. Cannot scale data."))
            }
            else {
                xtmp <- scale(x[, scaled])
                x[, scaled] <- xtmp
            }
        }
        m <- dim(x)[1]
        n <- floor(frac * m)
        index <- sample(1:m, n, replace = TRUE)
        index2 <- sample(1:m, n, replace = TRUE)
        temp <- x[index, , drop = FALSE] - x[index2, , drop = FALSE]
        dist <- rowSums(temp^2)
        ds <- sort(dist[dist != 0])
        sl <- ds[ceiling(0.1 * length(ds))]
        su <- ds[ceiling(0.9 * length(ds))]
        srange <- c(1/su, 1/sl)
        names(srange) <- NULL
        return(srange)
}

GWSDAT.Auto.Select.gamma <- function(temp.Cont.Data,gamma){

if(gamma[1] != 0) { return(gamma) }


tempgamma <- matrix(nrow=50,ncol=2)

	for (i in 1:nrow(tempgamma)) {

	tempgamma[i,] <- GWSDAT.sigest(log(Result.Corr.ND)~AggDate+XCoord+YCoord,temp.Cont.Data)

	}

	if (length(gamma) == 1) {

		gamma<-mean(0.5*(tempgamma[,1]+tempgamma[,2]))
		#gamma<-median(1/apply(tempgamma,1,mean)) #Wayne 26th June 2009

	}else{

		#gamma<-quantile(apply(tempgamma,1,mean),p=c(0.1,0.5,0.9))
		#gamma<-c(mean(0.5*(tempgamma[,1]+tempgamma[,2])), quantile(tempgamma[,2],p=0.9))
		#gamma<-c(quantile(tempgamma[,2],p=0.95))
		#gamma<-sort(apply(tempgamma,2,mean))[1]+c(.3,.5,.7)*diff(sort(apply(tempgamma,2,mean)))
		#gamma<-quantile(1/apply(tempgamma,1,mean),p=c(.1,.5,.9)) #Wayne 26th June 2009
		gamma<-sort(apply(tempgamma,2,mean))[1]+c(.3,.5,.7)*diff(sort(apply(tempgamma,2,mean)))
		
		


	}

return(gamma)
}












