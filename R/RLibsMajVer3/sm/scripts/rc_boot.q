with(radioc, {
x <- Cal.age[Cal.age>2000 & Cal.age<3000]
y <-  Rc.age[Cal.age>2000 & Cal.age<3000]
plot(x, y, xlab="Calendar.age", ylab="Radiocarbon.age",
        type="n")
model <- sm.regression(x, y, h=30, eval.points=x,
        display="none")
mhat <- model$estimate
r    <- y - mhat
r    <- r - mean(r)
for (i in 1:50) sm.regression(x, mhat + sample(r, replace=TRUE),
        h=30, add=TRUE, col=6, lty=2)
})
