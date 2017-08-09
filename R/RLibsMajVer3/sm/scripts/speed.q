with(aircraft, {
par(mfrow=c(1,2))
hist(Speed, ylab="Frequency")
sm.density(Speed, yht=0.0016)
par(mfrow=c(1,1))
})
