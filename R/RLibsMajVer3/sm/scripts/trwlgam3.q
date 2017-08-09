library(gam)
with(trawl, {
ind       <- (Year == 0 & Zone == 1)
score1    <- Score1[ind]
latitude  <- Latitude[ind]
longitude <- Longitude[ind] - 143
position  <- cbind(latitude, longitude = -longitude)

model1 <- gam(score1 ~ lo(longitude) + lo(latitude))
model2 <- gam(score1 ~ lo(longitude) +    latitude)
model3 <- gam(score1 ~ lo(longitude))
print(anova(model1))
print(anova(model2, model1))
print(anova(model3, model2))

par(mfrow = c(1,2))
model4  <- sm.regression(position, score1, h = c(0.1, 0.1),
              display = "none")
ex      <- model4$eval.points[,1]
ey      <- model4$eval.points[,2]
ngrid   <- length(ex)
grid    <- data.frame(cbind(latitude = rep(ex, ngrid),
              longitude = rep(-ey, rep(ngrid, ngrid))))
surface <- predict(model2, grid)
mask    <- model4$estimate
mask[!is.na(mask)] <- 1
persp(ex, ey, matrix(surface * mask, ncol = ngrid),
              xlab = "Latitude", ylab = "Longitude")
sm.regression(position, score1, h = c(100, 0.1))
par(mfrow=c(1,1))
})
