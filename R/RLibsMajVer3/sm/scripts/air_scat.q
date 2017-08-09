with(airpc, {
pc  <- cbind(Comp.1, Comp.2)
pc1 <- pc[Period==1,]
pc2 <- pc[Period==2,]
pc3 <- pc[Period==3,]
xlim <- range(Comp.1)
ylim <- range(Comp.2)

par(mfrow=c(2,2))
plot(Comp.1, Comp.2, xlim = xlim, ylim = ylim, main="1914-84")
lines(xlim, c(0,0), lty=2)
lines(c(0,0), ylim, lty=2)
plot(pc1, xlim = xlim, ylim = ylim, main="1914-35")
lines(xlim, c(0,0), lty=2)
lines(c(0,0), ylim, lty=2)
plot(pc2, xlim = xlim, ylim = ylim, main="1936-55")
lines(xlim, c(0,0), lty=2)
lines(c(0,0), ylim, lty=2)
plot(pc3, xlim = xlim, ylim = ylim, main="1956-84")
lines(xlim, c(0,0), lty=2)
lines(c(0,0), ylim, lty=2)
par(mfrow=c(1,1))
})
