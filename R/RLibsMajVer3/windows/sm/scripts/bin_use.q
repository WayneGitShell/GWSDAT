# example of use of binning

cat("Examples of use of function binning()\n")

#    1-d example

x  <- rnorm(1000)
xb <-binning(x)
h  <-hnorm(x)
sm.density(xb$x, h=h, weights=xb$x.freq, ylim=c(0,0.5/sqrt(var(x))))
pause()

#     2-d example

x  <-cbind(x,x+rnorm(1000))
xb <-binning(x)
h  <-hnorm(x)
par(mfrow=c(1,2))
sm.density(xb$x, h=h, weights=xb$x.freq)
sm.density(xb$x, h=h, weights=xb$x.freq, display="slice")
par(mfrow=c(1,1))
pause()

#      and another

with(airpc, {
pc3    <- cbind(Comp.1, Comp.2)[Period==3,]
pc.bin <- binning(pc3)
par(mfrow=c(1,2))
sm.density(pc.bin$x, h = hnorm(pc3), display = "image", ngrid=100,
          weights=pc.bin$x.freq)
plot(pc3, xlab="First Principal Component", ylab="Second Principal Component")
cat("this time original data rather than grid data are plotted\n")
sm.density(pc.bin$x, h = hnorm(pc3), display = "slice", ngrid=30,
          add=TRUE, weights=pc.bin$x.freq)
par(mfrow=c(1,1))
})

