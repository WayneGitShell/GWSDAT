with(follicle, {
sm.regression(Age, log(Count), h = 4, lty = 2)
model <- loess(log(Count) ~ Age)
lines(Age, model$fitted, col = 6)
})
