with(tephra, {
logit <- log(Al2O3/(100-Al2O3))
par(mfrow=c(1,2))
sm.density(logit, model = "Normal")
sm.density(logit, h = hsj(logit), model = "Normal")
par(mfrow=c(1,1))
})

