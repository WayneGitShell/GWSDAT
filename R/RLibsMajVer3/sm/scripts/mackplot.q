with(mackerel, {
Position  <- cbind(Latitude=mack.lat, Longitude=mack.long)
depth     <- mack.depth
par(mfrow=c(2,2))
sm.regression(Position,    log(Density), h=c(0.3, 0.3), hull=FALSE)
sm.regression(Position,    log(depth),   h=c(0.3, 0.3), hull=FALSE)
sm.regression(log(depth),  log(Density), h = 0.2)
sm.regression(Temperature, log(Density), h = 3)
par(mfrow = c(1,1))
})
