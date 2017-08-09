with(trawl, {
ind       <- (Year == 1 & Zone == 1 & !is.na(Depth))
score1    <- Score1[ind]
depth     <- Depth[ind]
par(mfrow=c(1,2))
sm.regression(depth, score1, h = 5, model = "no.effect")
sm.regression(depth, score1, h = 10, model = "no.effect")
par(mfrow=c(1,1))
})
