with(aircraft, {
par(mfrow = c(1,2))
h <-  exp(mean(log(tapply(log(Span), Period, FUN = "hnorm"))))
ind <- (Period!=3)
sm.density.compare(log(Span)[ind], Period[ind], h = h, 
        xlab = "log(Span)", lty = c(3,2), ylim = c(0, 1.2))
legend(3.0, 1.1, c("Period 1", "Period 2"), lty = c(3,2))
ind <- (Period!=1)
sm.density.compare(log(Span)[ind], Period[ind], h = h,
        xlab = "log(Span)", lty = c(2,1), ylim = c(0, 1.2))
legend(3.0, 1.1, c("Period 2", "Period 3"), lty = c(2,1))
par(mfrow = c(1,1))
})
