with(citrate, {
Citrate<-as.matrix(citrate)
nSubj<-dim(Citrate)[1]
nTime<-dim(Citrate)[2]
Time<-(1:nTime)
plot(c(min(Time),max(Time)), c(min(Citrate),max(Citrate)),
    type="n", xlab="time", ylab="Citrate")
for(i in 1:nSubj) lines(Time,as.vector(Citrate[i,]))
pause()
a <- sm.rm(y=Citrate, display.rice=TRUE)
sm.regression(Time,a$aux$mean,h=1.2,hmult=1,add=TRUE,lty=3)
})
