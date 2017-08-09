with(aircraft, {
y <- log(Span)
for (i in 1:3) {
  yi  <- y[Period == i]
  med <- median(yi)
  sc  <- diff(quantile(yi, c(0.25, 0.75))) / 1.349
  y[Period == i] <- (yi - med) / sc
  }
h <-  exp(mean(log(tapply(y, Period, FUN = "hnorm"))))
sm.density.compare(y, Period, h = h, lty = c(3,2,1),
        xlab = "Standardised scale")
legend(1.5, 0.55, c("Period 1","Period 2","Period 3"), 
        lty=c(3,2,1))
})

