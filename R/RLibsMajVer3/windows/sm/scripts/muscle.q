with(muscle, {
TypeI <- TypeI.P+TypeI.R+TypeI.B
sm.poisson(log(TypeI), TypeII, 0.25, display="se")
pm <- glm(TypeII ~ log(TypeI), family=poisson)
lines(sort(log(TypeI)), fitted(pm)[order(log(TypeI))], 
      col=4, lty=6)
})
