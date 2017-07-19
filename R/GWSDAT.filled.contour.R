

######################################## GWSDAT Barycentric Interpolation Function ##########################################

"GWSDAT.Bary.Interp"<-function(model,AggDate,my.area,type=c("Predicted","Lower 95% CI","Upper 95% CI","% sd","IQR/2")){

require(geometry) 
require(Matrix) 

type=match.arg(type)
if(length(AggDate)!=1){stop("Agg Date must be length 1")}

expandpoly<-function (mypol, fact) {

    m1 <- mean(mypol[, 1])
    m2 <- mean(mypol[, 2])
    cbind((mypol[, 1] - m1) * fact + m1, (mypol[, 2] - m2) * fact + m2)

}

my.area<-my.area[chull(my.area),,drop=F]
my.exp.area<-expandpoly(my.area,fac=1.15)
my.area<-expandpoly(my.area,fac=1.05)
colnames(my.area)<-c("XCoord","YCoord")



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



#------------------------------------------------------------------#

####################### pred.df ####################################

pred.df$InOut<-!point.in.polygon(pred.df$XCoord,pred.df$YCoord,my.area[,1],my.area[,2])==0

predred.df<-pred.df[pred.df$InOut,]
dn <- delaunayn(eval.df[,c("XCoord","YCoord")])
tri <- tsearch(eval.df[,"XCoord"],eval.df[,"YCoord"],dn,predred.df[,"XCoord"],predred.df[,"YCoord"],bary=T) 
active <- dn[tri$idx,] 


M <- sparseMatrix(i=rep(1:nrow(predred.df),each=3),j=as.numeric(t(active)),x=as.numeric(t(tri$p)),dims=c(nrow(predred.df),length(eval.df$pred))) 
predred.df$pred<-as.numeric(M%*%eval.df$pred)


pred.df$pred[pred.df$InOut]<-predred.df$pred

#------------------------------------------------------------------#

}

out<-list(x=x0,y=y0,z=matrix(pred.df$pred,nrow = length(x0), ncol = length(y0)))

return(out)

}


#--------------------------------------------------------------------------------------------------------------------------#


######################################### GWSDAT Interpolation Function #####################################################

"GWSDAT.Interp"<-function(model,AggDate,eval.df,type=c("predicted","lower","upper","sd")){


type=match.arg(type)

if(length(AggDate)!=1){stop("Agg Date must be length 1")}

x0=sort(unique(eval.df[,1]))
y0=sort(unique(eval.df[,2]))
my.df<-expand.grid(XCoord=x0,YCoord=y0)

if(!is.null(model)){

	my.df$AggDate=as.numeric(AggDate)
	my.df$InOut<-!point.in.polygon(my.df$XCoord,my.df$YCoord,eval.df[chull(eval.df[,1:2]),1],eval.df[chull(eval.df[,1:2]),2])==0
	my.df$pred<-rep(NA,nrow(my.df))

	temppred<-predict(model,newdata=my.df[my.df$InOut, c("XCoord","YCoord","AggDate")],se=type!="predicted")
	my.df$pred[my.df$InOut]<-temppred$predicted
	my.df$pred.sd[my.df$InOut]<-temppred$predicted.sd

	if(type=="lower"){my.df$pred<-my.df$pred-1.96*my.df$pred.sd}
	if(type=="upper"){my.df$pred<-my.df$pred+1.96*my.df$pred.sd}
	if(type=="sd")   {my.df$pred<-my.df$pred.sd}

}else{

	my.df$pred<-rep(NA,nrow(my.df))

}

list(x=x0,y=y0,z=matrix(my.df$pred,nrow = length(x0), ncol = length(y0)))

}


#--------------------------------------------------------------------------------------------------------------------------#


######################################### GWSDAT Filled Contour Function ###################################################


 "GWSDAT.filled.contour"<-function (x = seq(0, 1, len = nrow(z)), y = seq(0, 1, len = ncol(z)), 
    z, xlim = range(x, finite = TRUE), ylim = range(y, finite = TRUE), 
    zlim = range(z, finite = TRUE), levels =pretty(zlim, nlevels) , 
    nlevels = 20, color.palette = cm.colors, col = color.palette(length(levels) - 
        1), plot.title, plot.axes, key.title, key.axes, asp = NA, 
    xaxs = "i", yaxs = "i", las = 1, axes = TRUE, frame.plot = axes, ShapeFiles=NULL,fixedConcScale=FALSE,PlumeDetails=NULL,
    ...) 
{

  
     if (missing(z)) {
        if (!missing(x)) {
            if (is.list(x)) {
                z <- x$z
                y <- x$y
                x <- x$x
            }
            else {
                z <- x
                x <- seq(0, 1, len = nrow(z))
            }
        }
        else stop("no 'z' matrix specified")
    }
    else if (is.list(x)) {
        y <- x$y
        x <- x$x
    }


    if (any(diff(x) <= 0) || any(diff(y) <= 0)) 
        stop("increasing 'x' and 'y' values expected")
    mar.orig <- (par.orig <- par(c("mar", "las", "mfrow")))$mar
    on.exit(par(par.orig))
    w <- (3 + mar.orig[2]) * par("csi") * 2.54
    layout(matrix(c(2, 1), nc = 2), widths = c(1, lcm(w)))
    par(las = las)
    mar <- mar.orig
    mar[4] <- mar[2]
    mar[2] <- 1
    par(mar = mar)
    plot.new()
	
    plot.window(xlim = c(0, 1), ylim = range(levels), xaxs = "i", yaxs = "i")
    #rect(0, levels[-length(levels)], 1, levels[-1], col = col)
    equal.cuts<-seq(range(levels)[1],range(levels)[2],length=length(levels))#GWSDAT Change!
    rect(0, equal.cuts[-length(equal.cuts)],1,equal.cuts[-1],col = col) #GWSDAT Change!
    
    if(fixedConcScale){#GWSDAT Change

	    my.at<-equal.cuts[-length(equal.cuts)]
	    my.at<-c(my.at,0.4*equal.cuts[length(equal.cuts)-1]+.6*equal.cuts[length(equal.cuts)])
	    my.labs<-paste(" ",levels[-length(levels)],sep="")
    	    my.labs<-c(my.labs,paste(">",levels[length(levels)-1],sep=""))
	    #my.labs<-c(my.labs,paste(levels[length(levels)-1],"+",sep=""))

    }else{

	    my.at=equal.cuts
	    my.labs<-as.numeric(levels)

    }#GWSDAT Change

   
    axis(side=4,at=my.at,labels=my.labs)#GWSDAT Change
	

    if (missing(key.axes)) {
        if (axes) 
            NULL #axis(4) #GWSDAT Change
    }
    else key.axes
    box()
    if (!missing(key.title)) 
        key.title
    mar <- mar.orig
    mar[4] <- 1
    par(mar = mar)
    plot.new()
    plot.window(xlim, ylim, "", xaxs = xaxs, yaxs = yaxs, asp = asp)
    if (!is.matrix(z) || nrow(z) <= 1 || ncol(z) <= 1) 
        stop("no proper 'z' matrix specified")
    if (!is.double(z)) 
        storage.mode(z) <- "double"
	
	if( R.Version()$major=="2"){
     		.Internal(filledcontour(as.double(x), as.double(y), z, as.double(levels),col = col))
 	}else{
     		.filled.contour(x, y, z, levels, col)
	 }

    ################## ShapeFile Plotting ####################################

	if(!is.null(ShapeFiles)){

	for(i in 1:length(ShapeFiles)){
			
		try(GWSDAT.PlotShapeFile(ShapeFiles[[i]]))

	}

	}


    #------------------------------------------------------------------------#

    if (missing(plot.axes)) {
        if (axes) {
            title(main = "", xlab = "", ylab = "")
            Axis(x, side = 1)
            Axis(y, side = 2)
        }
    }
    else plot.axes
    if (frame.plot) 
        box()
    if (missing(plot.title)) 
        title(...)
    else plot.title
    invisible()
}

#--------------------------------------------------------------------------------------------------------------------------#
