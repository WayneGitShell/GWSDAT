with(aircraft, {
y <- log(Span)[Period==3]
sm.density(y, xlab = "Log span")
for (i in 1:20) sm.density(sample(y, replace=TRUE), col=6, add=TRUE)
sm.density(y, xlab = "Log span", add=TRUE)
})
