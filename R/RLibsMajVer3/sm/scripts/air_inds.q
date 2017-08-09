with(aircraft, {
Speed3 <- log(Speed[Period==3])
Span3  <- log(Span[Period==3])
air3  <- cbind(Span3, Speed3)	
result.12 <- sm.density(air3,   eval.points = air3,   display = "none")
result.1  <- sm.density(Span3,  eval.points = Span3,  display = "none")
result.2  <- sm.density(Speed3, eval.points = Speed3, display = "none")
tobs      <- mean(log(result.12$estimate / 
			(result.1$estimate * result.2$estimate)))
cat("Observed value: ", round(tobs,5), "\n")

nsim   <- 200
simval <- rep(0,nsim)
p      <- 0
for (i in 1:nsim) {
   samp      <- cbind(y1 = Span3, y2 = sample(Speed3))	
   result.12 <- sm.density(samp,     eval.points = samp,     display = "none")
   result.1  <- sm.density(samp[,1], eval.points = samp[,1], display = "none")
   result.2  <- sm.density(samp[,2], eval.points = samp[,2], display = "none")
   simval[i] <- mean(log(result.12$estimate / 
  			(result.1$estimate * result.2$estimate)))
   if (simval[i] > tobs) p <- p+1
   cat(i, " ")
   }
cat("Empirical significance:", round(p/nsim,3), "\n")
})

