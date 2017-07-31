with(aircraft, {
log.span.3 <- log(Span[Period==3])

par(mfrow=c(1,2))

subsamp    <- log.span.3[1+seq(1,length(log.span.3),20)]
hist.info  <- hist(subsamp, plot=FALSE)
freq       <- hist.info$counts
nfreq      <- length(freq)
hist(subsamp, xlab="Log Span", ylab="Frequency", xlim=c(1,5), col="blue")
for (i in 1:nfreq) {
   if (freq[i] > 1) {
      left  <- rep(hist.info$breaks[i],freq[i]-1)
      right <- rep(hist.info$breaks[i+1],freq[i]-1)
      segments(left,1:(freq[i]-1),right,1:(freq[i]-1),col=0)
      }
   }

h    <- 0.25
nsub <- length(subsamp)
sm.density(subsamp, h=h, xlab="Log span", xlim=c(1,5))
for (i in 1:nsub) {
   points(subsamp[i],0)
   temp <- sm.density(subsamp[i], h=h, display="none",
              xlim=subsamp[i]+c(-1,1)*3*h)
   lines(temp$eval.points, temp$estimate/nsub, lty=2)
   }

par(mfrow=c(1,1))
})
