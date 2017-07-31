with(airpc, {
pc3 <- cbind(Comp.1, Comp.2)[Period==3,]
par(mfrow=c(1,2))
sm.density(pc3, display="image")
sm.density(pc3, display="slice")
par(mfrow=c(1,1))
})
