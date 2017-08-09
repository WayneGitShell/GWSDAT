with(trawl, {
ind       <- (Year == 0 & Zone == 1)
score     <- Score1[ind]
longitude <- Longitude[ind]
par(mfrow=c(1,2))
plot(longitude, score, pch="o")
ls.fit <- lm(score ~ longitude)
abline(ls.fit$coefficients, lty=3)
plot(longitude, residuals(ls.fit), pch="o")
abline(0,0,lty=3)
par(mfrow=c(1,1))
print(summary(lm(score ~ poly(longitude,2))))
})
