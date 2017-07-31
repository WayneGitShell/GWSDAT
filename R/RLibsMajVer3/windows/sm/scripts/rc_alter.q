with(radioc, {
cal.age <- Cal.age[Cal.age>2000 & Cal.age<3000]
rc.age  <-  Rc.age[Cal.age>2000 & Cal.age<3000]

par(mfrow=c(2,2))

add.window <- function(x, y, h, x.eval) {
  polygon(rep(c(x.eval - 2 * h, x.eval + 2 * h), rep(2,2)),
	c(range(y), rev(range(y))), col = "cyan", border = FALSE)
  lines(rep(x.eval, 2), range(y), lty = 2)
  points(x, y)
  xseq <- seq(x.eval - 3 * h, x.eval + 3 * h, length = 20)
  kernel <- dnorm(xseq, x.eval, h)
  kernel <- min(y) + 120 * kernel / max(kernel)
  lines(xseq, kernel, lty = 2)
  }

x.eval <- 2670
hvec   <- c(30, 100, 8, 100)
for (i in 1:4) {
  plot(cal.age, rc.age,
 	xlab = "Calendar age", ylab = "Radiocarbon age")
  if (i == 2) {
    sm.regression(cal.age, rc.age, h = 100, poly.index = 0, lty = 2, add = TRUE)}
  else {
    add.window(cal.age, rc.age, hvec[i], x.eval)}
  sm.regression(cal.age, rc.age,   h = hvec[i], add = TRUE)
  }

par(mfrow=c(1,1))
})
