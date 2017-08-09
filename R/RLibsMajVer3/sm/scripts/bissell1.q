# full script for Bissell data
# sections of it generate individual figures

with(bissell, {

#  parametric fit
plot(Length, Flaws, xlim=c(0,1000), pch="o")
beta <- sum(Flaws)/sum(Length)
x <- seq(0, 1000, length=50)
bx <- beta*x  
lines(x, bx)
lines(x,bx+2*sqrt(bx),lty=3)
lines(x,pmax(0,bx-2*sqrt(bx)),lty=3)
})
