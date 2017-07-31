with(trawl, {

ind       <- (Zone == 1)
score1    <- Score1[ind]
latitude  <- Latitude[ind]
longitude <- Longitude[ind] - 140
position  <- cbind(longitude, latitude)
year      <- Year[ind]

par(mfrow=c(2,2))
par(cex=0.7)

sm.regression(position[year == 0,], score1[year == 0],
		h= c(0.1, 0.1), eye.mult = c(8,-6,5), zlab="Score.92")
sm.regression(position[year == 1,], score1[year == 1], zlab="Score.93",
		h= c(0.1, 0.1), eye.mult = c(8,-6,5), zlim=c(-1,2))

sig  <- 0
adf  <- 0
for (i in c(0, 1)) {
  X   <- position[year == i, ]
  y   <- score1[year == i]
  W   <- sm.weight2(X, X, h = c(0.1, 0.1))
  est <- W %*% y
  adf <- adf + length(X[,1]) - sum(diag(W))
  sig <- sig + sum((y - est)^2)
  }
sig <- sqrt(sig / adf)

ngrid <- 20
x1  <- seq(min(longitude), max(longitude), length=ngrid)
x2  <- seq(min(latitude),  max(latitude),  length=ngrid)
evp <- cbind(longitude=rep(x1, ngrid), 
		latitude=rep(x2, rep(ngrid, ngrid))) 
se  <- rep(0, nrow(evp))
for (i in c(0, 1)) {
  X   <- position[year == i,]
  y   <- score1[year == i]
  W   <- sm.weight2(X, evp, h = c(0.1, 0.1))
  if (i == 0) est0 <- as.vector(W %*% y)
            else est1 <- as.vector(W %*% y)
  se  <- se + as.vector((W^2) %*% rep(1,nrow(X)))
  }
se  <- matrix(sig * sqrt(se), ncol = ngrid)

sdiff <- as.vector((est0 - est1)/se)
ind   <- !is.na(sdiff)
sm.regression(evp[ind,], sdiff[ind], h = c(0.01, 0.01), eye.mult = c(8,-6,5))
contour(x1, x2, (est1 - est0)/se, xlab = "longitude", ylab = "latitude")

par(cex=1)
par(mfrow = c(1,1))
})

