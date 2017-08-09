library(gam)
ind       <- (trawl$Year == 0 & trawl$Zone == 1)
score1    <- trawl$Score1[ind]
latitude  <- trawl$Latitude[ind]
longitude <- trawl$Longitude[ind] - 143
position  <- cbind(latitude, longitude = -longitude)

par(mfrow = c(2,2))
par(cex=0.7)
model1  <- sm.regression(position, score1, h = c(0.1, 0.1))
model2  <- gam(score1 ~ lo(latitude) + lo(longitude))
ex      <- model1$eval.points[,1]
ey      <- model1$eval.points[,2]
ngrid   <- length(ex)
grid    <- data.frame(cbind(latitude = rep(ex, ngrid),
            longitude = rep(-ey, rep(ngrid, ngrid))))
surface <- predict(model2, grid)
mask    <- model1$estimate
mask[!is.na(mask)] <- 1
persp(ex, ey, matrix(surface * mask, ncol = ngrid),
            xlab = "latitude", ylab = "longitude")
summary(model2)
plot(model2, se=TRUE)
par(cex=1)
par(mfrow = c(1,1))
