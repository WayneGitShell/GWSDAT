data(wonions, {
par(mfrow=c(1,2))
sig.trace(sm.ancova(Density, log(Yield), Locality,
        model = "parallel",display = "none"),
        hvec = seq(5,30, length = 12))
sm.ancova(Density, log(Yield), Locality, h = 15, 
        model = "parallel")
par(mfrow=c(1,1))
})
