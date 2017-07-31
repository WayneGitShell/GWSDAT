with(trawl, {
par(mfrow = c(2,2))
plot(Longitude, Latitude, type = "n")
points(Longitude[Zone == 1], Latitude[Zone == 1])
text(Longitude[Zone == 0], Latitude[Zone == 0], "o")
Zone93    <- (Year == 1 & Zone == 1)
Position  <- cbind(Longitude - 143, Latitude)
sm.regression(Latitude[Zone93],  Score1[Zone93], h = 0.1)
sm.regression(Position[Zone93,], Score1[Zone93], 
     h= c(0.1, 0.1), eye.mult = c(8,-6,5), xlab="Longitude - 143")
sm.regression(Longitude[Zone93], Score1[Zone93], h = 0.1)
par(mfrow = c(1,1))
})

