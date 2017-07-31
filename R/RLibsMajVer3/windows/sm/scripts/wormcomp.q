with(worm, {
Males   <- sm.binomial(Age[Sex == 1], Infection[Sex == 1], h = 10,
              display = "none")
agem    <- Males$eval.points
Females <- sm.binomial(Age[Sex == 2], Infection[Sex == 2], h = 10,
              eval.points = agem, display = "none")
estm <- Males$estimate
sem  <- Males$se
estf <- Females$estimate
sef  <- Females$se
plot(Age, Infection, ylab = "Proportion infected", type= "n")
av <- (log(estm/(1-estm)) + log(estf/(1-estf)))/2
se <- sqrt(sem^2 + sef^2)
upper <- 1/(1+exp(-(av + se)))
lower <- 1/(1+exp(-(av - se)))
polygon(c(agem, rev(agem)), c(upper, rev(lower)),
      col = "cyan", border = FALSE)
lines(agem, estm)
lines(agem, estf, lty = 3)
})
