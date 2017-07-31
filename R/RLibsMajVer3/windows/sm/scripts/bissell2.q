with(bissell, {
plot(Length, Flaws, xlim=c(0,1000), pch="o")
beta <- sum(Flaws)/sum(Length)
x <- seq(0, 1000, length=50)
lines(x, beta*x, lty=3)
h <- 100
W<-sm.weight(Length, x, h, options=list(poly.index=0))  
sm.beta <- (W %*% Flaws)/(W %*% Length)
lines(x,sm.beta*x)
lines(x,sm.beta*x+2*sqrt(sm.beta*x),lty=3)
lines(x,pmax(0,sm.beta*x-2*sqrt(sm.beta*x)),lty=3)
})
