with(lcancer, {
cases    <- cbind(Easting, Northing)[Cancer == 1,]/10000
controls <- cbind(Easting, Northing)[Cancer == 2,]/10000
xlim     <- range(Easting/10000)
ylim     <- range(Northing/10000)
par(mfrow=c(2,2))
plot(Easting/10000, Northing/10000, type = "n")
points(cases)
points(35.45, 41.3, pch = 6)
plot(Easting/10000, Northing/10000, type = "n")
points(controls)
points(35.45, 41.3, pch = 6)
h <- c(0.12,0.12)
sm.density(cases,    h = h, xlim=xlim, ylim=ylim, zlim=c(0,2))
sm.density(controls, h = h, xlim=xlim, ylim=ylim, zlim=c(0,2))
par(mfrow=c(1,1))
})
