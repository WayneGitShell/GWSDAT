with(birth, {
Low0 <- Low[Smoke=="N"]; Lwt0 <- Lwt[Smoke=="N"]
Low1 <- Low[Smoke=="S"]; Lwt1 <- Lwt[Smoke=="S"]
sm.binomial(Lwt0, Low0, h=20, pch="N", col=2,
      xlab="Mother weight", ylab="Prob{Low}", xlim=c(80,260))
sm.binomial(Lwt1, Low1, h=20, pch="S", col=3, add=TRUE)
})
