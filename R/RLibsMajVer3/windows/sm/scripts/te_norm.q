with(tephra, {
logit <- log(Al2O3/(100-Al2O3))
par(mfrow=c(1,2))
qqnorm(logit)
qqline(logit)
cat("ISE statistic:", nise(logit),"\n")
sm <- sm.density(logit)
y  <- sm$eval.points
sd <- sqrt(hnorm(logit)^2 + var(logit))
lines(y, dnorm(y, mean(logit), sd), lty = 3)
par(mfrow=c(1,1))
})

