provide.data(trees)
h1 <- 1.5
h2 <- 4
par(mfrow=c(2,2))
par(cex=0.7)
sm.regression(Diameter, Volume, h= h1, model = "linear")
sm.regression(Height, Volume, h= h2, model = "linear")
X <- cbind(Diameter, Height)
sm.regression(X, Volume, h = c(h1,h2), model = "linear",
	   zlim = c(0,80))
X <- cbind(log(Diameter), log(Height))
colnames(X) <- c("log(Diameter)","log(Height)")
sm.regression(X, log(Volume), h = log(c(h1,h2)),
           model = "linear")
par(cex=1)
par(mfrow=c(1,1))

