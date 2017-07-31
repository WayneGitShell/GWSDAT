with(radioc, {
par(mfrow=c(1,2))
plot(Cal.age, Rc.age)
abline(0,1,lty=2)
ind <- (Cal.age>2000 & Cal.age<3000)
cal.age <- Cal.age[ind]
rc.age  <-  Rc.age[ind]
sm.regression(cal.age, rc.age, h = 30)
sm.regression(cal.age, rc.age, h = 1000, lty = 2, add=TRUE)
par(mfrow=c(1,1))
})
