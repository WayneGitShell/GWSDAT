n <- 100
x <- seq(0, 1, length = n)
m <- sin(2 * pi * x)
h <- 0.05
true.sigma <- 0.2
model <- sm.regression(x, m, h = h, display = "none")
upper <- model$estimate + 2 * (true.sigma/model$sigma)*model$se
lower <- model$estimate - 2 * (true.sigma/model$sigma)*model$se
y <- rnorm(n, m, true.sigma)
plot(range(x), range(y, upper, lower), type = "n",
	xlab="x", ylab="y")
z <- model$eval.points
polygon(c(z, rev(z)), c(upper, rev(lower)), border = FALSE, col = "cyan")

lines(x, m)
lines(z, model$estimate, lty = 3)
points(x, y)
