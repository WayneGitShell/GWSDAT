X <- cbind(rep(1, 36), as.matrix(mildew[ , 1:11]))
e <- residuals(lsfit(X, mildew$Yield, intercept = FALSE))
position <- 1:36
par(mfrow = c(1,2))
sig.trace(sm.regression(position, e, design.mat=X, model =
   "no.effect", display="none"), hvec = seq(1, 20, by=1.5))
sm.regression(position, e, design.mat=X, h=7, model="no.effect")
par(mfrow=c(1,1))

