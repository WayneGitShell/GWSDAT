with(trawl, {
ind <- (Year == 1 & Zone == 1 & !is.na(Depth))
par(mfrow=c(1,2))
sm.regression(Depth[ind], Score1[ind], h = 5,
        xlab="Depth", ylab="Score1")
plot(Depth[ind], Score1[ind], type = "n",
        xlab="Depth", ylab="Score1")
for (i in 1:100)
  	sm.regression(Depth[ind], sample(Score1[ind]),
  		h = 5, col = 6, lty = 2, add = TRUE)
sm.regression(Depth[ind], Score1[ind], h = 5, add = TRUE)
par(mfrow=c(1,1))
})
