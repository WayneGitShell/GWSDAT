with(airpc, {
pc  <- cbind(Comp.1, Comp.2)
pc1 <- pc[Period==1,]
pc2 <- pc[Period==2,]
pc3 <- pc[Period==3,]
plot(pc, type="n")
sm.density(pc1, display="slice", props=75, add=TRUE, lty=3)
sm.density(pc2, display="slice", props=75, add=TRUE, lty=2)
sm.density(pc3, display="slice", props=75, add=TRUE, lty=1)
})
