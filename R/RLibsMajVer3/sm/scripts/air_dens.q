with(airpc, {
pc3 <- cbind(Comp.1, Comp.2)[Period==3,]
par(mfrow=c(2,2))
par(cex=0.6)
plot(pc3)
sm.density(pc3,            zlim=c(0,0.08))
sm.density(pc3, hmult=1/2, zlim=c(0,0.15))
sm.density(pc3, hmult=2,   zlim=c(0,0.04))
par(cex=1)
par(mfrow=c(1,1))
})
