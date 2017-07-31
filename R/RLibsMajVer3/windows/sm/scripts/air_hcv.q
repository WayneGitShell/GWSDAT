with(aircraft, {
y <- log(Span)[Period==3]
par(mfrow=c(1,2))
sm.density(y, h = hcv(y), xlab="Log span", lty=3, yht=1.4)
sm.density(y, h = hsj(y), add = TRUE)
with(airpc, {
   pc3 <- cbind(Comp.1, Comp.2)[Period==3,]
   sm.density(pc3, h = hcv(pc3), display = "slice")
})
par(mfrow=c(1,1))
})
