n <- 50
x <- seq(0, 1, length = n)
m <- sin(2 * pi * x)
h <- 0.05
sigma <- 0.2
y <- rnorm(n, m, sigma)
par(mfrow=c(1,2))
h.cv <- hcv(x, y, display="line", ngrid=32)
plot(x, y)
lines(x, m)
sm.regression(x, y, h=hcv(x, y), add=TRUE, lty=2)
par(mfrow=c(1,1))
