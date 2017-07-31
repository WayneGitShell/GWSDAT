with(aircraft, {
Speed3 <- log(Speed[Period==3])
Span3  <- log(Span[Period==3])
par(mfrow=c(1,2))
plot(Span3, Speed3, xlab = "Log Span", ylab = "Log Speed")
air3  <- cbind(Span3, Speed3)	
sm.density(air3, display="slice", 
        xlab= "Log Span", ylab = "Log Speed")
par(mfrow=c(1,1))
})
