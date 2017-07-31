with(aircraft, {
y1 <- log(Span)[Period==1]
y2 <- log(Span)[Period==2]
y3 <- log(Span)[Period==3]
sm.density(y3, xlab="Log span")
sm.density(y2, add=TRUE, lty=2)
sm.density(y1, add=TRUE, lty=3)
legend(3.5, 1, c("Period 1", "Period 2", "Period 3"), lty=3:1)
})
