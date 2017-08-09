ind       <- (trawl$Year == 0 & trawl$Zone == 1)
score     <- trawl$Score1[ind]
longitude <- trawl$Longitude[ind]
par(mfrow=c(1,2))
sig.trace(sm.regression(longitude, score, 
        model = "linear", display="none"), 
        hvec = seq(0.02, 0.2, length = 10))
sm.regression(longitude, score, h = 0.1, model = "linear")
par(mfrow=c(1,1))
print(summary(lm(score ~ poly(longitude,2))))
