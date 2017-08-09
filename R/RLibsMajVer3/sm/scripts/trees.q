diameter <- trees$Girth
volume   <- trees$Volume
height   <- trees$Height
h1 <- 1.5
h2 <- 4
par(mfrow=c(2,2))
par(cex=0.7)
sm.regression(diameter, volume, h = h1, model = "linear")
sm.regression(height, volume, h = h2, model = "linear")
X <- cbind(diameter, height)
sm.regression(X, volume, h = c(h1, h2), model = "linear",
	   zlim = c(0,80))
X <- cbind(log(diameter), log(height))
colnames(X) <- c("log(Diameter)","log(Height)")
sm.regression(X, log(volume), h = log(c(h1, h2)),
           model = "linear")
par(cex=1)
par(mfrow=c(1,1))

