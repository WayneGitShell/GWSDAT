with(magrem, {
par(mfrow=c(1,2))
sm.sphere(20, -30, theta=60, phi=10, sphim=TRUE, kappa=13.6)
sm.sphere(maglat, maglong, theta=60, phi=10, sphim=TRUE,
          kappa=13.6)
par(mfrow=c(1,1))
})
