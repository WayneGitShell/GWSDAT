with(trawl, {
ind       <- (Year == 0)
longitude <- Longitude[ind]
zone      <- Zone[ind]
score1    <- Score1[ind]
sm.ancova(longitude, score1, zone, h = 0.1)
})
