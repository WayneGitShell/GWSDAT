with(aircraft, {
y <- log(Span)[Period==3]
sm.density(y, xlab = "Log span", display = "se")
})
