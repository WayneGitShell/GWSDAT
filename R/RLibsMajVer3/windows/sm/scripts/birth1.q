
with(birth, {
Low1<-Low[Smoke=="S"]; Lwt1<-Lwt[Smoke=="S"]
Lj <- jitter(Low1, amount = 0)
plot(Lwt1,Lj,type="n",xlab="Mother weight",ylab="prob(Low)")
text(Lwt1,Lj,"S",col=3)
abline(0,0, lty=3)
abline(1,0, lty=3)
sm.regression(Lwt1,Low1,h=20,add=TRUE)
})
