provide.data(trawl)
ind       <- (Year == 1 & Zone == 1 & !is.na(Depth))
score1    <- Score1[ind]
depth     <- Depth[ind]
summary(lm(score1 ~ depth))
sig.trace(sm.regression(depth, score1, 
        model = "no.effect", display="none"), 
        hvec = seq(5, 20, length = 10))
