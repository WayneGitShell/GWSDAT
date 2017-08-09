with(aircraft, {
y <- log(Span[Period==3])
par(mfrow=c(1,2))
sm.density(y, hmult = 1/3, xlab="Log span")
sm.density(y, hmult = 2,   xlab="Log span")
par(mfrow=c(1,1))
})
