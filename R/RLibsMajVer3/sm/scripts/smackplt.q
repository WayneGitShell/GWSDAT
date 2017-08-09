with(smacker, {
Presence <- Density
Presence[Presence > 0] <- 1
Position <- cbind(Longitude=-smack.long, Latitude=smack.lat)
Log.depth <- log(smack.depth)
par(mfrow = c(1,2))
plot(Position, type="n")
points(Position[Presence==1,], pch=16)
points(Position[Presence==0,], pch=1)
sm.binomial(Log.depth, Presence, h = 0.7, display = "se")
par(mfrow = c(1,1))
})

