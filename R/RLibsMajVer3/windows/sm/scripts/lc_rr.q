with(lcancer, {
cases    <- cbind(Easting, Northing)[Cancer == 1,]/10000
controls <- cbind(Easting, Northing)[Cancer == 2,]/10000
xlim     <- range(Easting/10000)
ylim     <- range(Northing/10000)

par(mfrow=c(2,2))

h <- c(0.12,0.12)
cases.sm    <- sm.density(cases,    h = h,
		xlim = xlim, ylim = ylim, display = "none")
controls.sm <- sm.density(controls, h = h,
		xlim = xlim, ylim = ylim, display = "none")
delta <- 0.1
ratio <- (cases.sm$estimate + delta)/(controls.sm$estimate + delta)
xgrid     <- cases.sm$eval.points[,1]
ygrid     <- cases.sm$eval.points[,2]
rr <- ratio/(1+ratio)
par(cex=0.8)
persp(xgrid, ygrid, rr, zlim=c(0,0.8), theta = -30,
      phi = 40, d = 4, expand = 0.7,
      xlab="Easting", ylab="Northing", zlab="Risk")
persp(xgrid, ygrid, log(rr), zlim=c(-1.5,0),
      theta = -30, phi = 40, d = 4, expand = 0.7,
      xlab="Easting", ylab="Northing", zlab="Log risk")
diff.sm <- sqrt(cases.sm$estimate) - sqrt(controls.sm$estimate)
se      <- sqrt(cases.sm$se^2 + controls.sm$se^2)
persp(xgrid, ygrid, diff.sm/se,
      theta = -30, phi = 40, d = 4, expand = 0.7,
      xlab="Easting", ylab="Northing", zlab="Std. difference")
plot(xgrid, ygrid, type = "n", xlab="Easting", ylab="Northing")
contour(xgrid, ygrid, diff.sm/se, levels = c(-4, -2), col = 2, add = TRUE)
contour(xgrid, ygrid, diff.sm/se, levels = c( 2,  4), col = 6, add = TRUE)
par(cex=1)
par(mfrow=c(1,1))
})
