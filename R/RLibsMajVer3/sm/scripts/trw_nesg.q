ind       <- (trawl$Year == 1 & trawl$Zone == 1 & !is.na(trawl$Depth))
score1    <- trawl$Score1[ind]
depth     <- trawl$Depth[ind]
summary(lm(score1 ~ depth))
sig.trace(sm.regression(depth, score1, 
        model = "no.effect", display="none"), 
        hvec = seq(5, 20, length = 10))
