with(lcancer, {

cases    <- cbind(Easting/10000, Northing/10000)[Cancer == 1,]
controls <- cbind(Easting/10000, Northing/10000)[Cancer == 2,]
xlim     <- range(Easting/10000)
ylim     <- range(Northing/10000)

h <- c(0.05,0.05)
cases.sm    <- sm.density(cases,    h = h,
		 xlim = xlim, ylim = ylim, display = "none")
controls.sm <- sm.density(controls, h = h,
		 xlim = xlim, ylim = ylim, display = "none")
diff.obs <- sum((cases.sm$estimate -
			controls.sm$estimate)^2)
cat("Observed value:",diff.obs, "\n")

nboot    <- 20
p <- 0
ncase    <- nrow(cases)
ncontrol <- nrow(controls)
for (i in 1:nboot) {
  ind.control   <- sample((1:ncontrol), ncontrol, replace=TRUE)
  ind.case      <- sample((1:ncontrol), ncase,    replace=TRUE)
  controls.star <- controls[ind.control,]
  cases.star    <- controls[ind.case,]
  cases.sm.star    <- sm.density(cases.star,    h = h,
  			xlim=xlim, ylim = ylim, display = "none")
  controls.sm.star <- sm.density(controls.star, h = h,
  			xlim=xlim, ylim = ylim, display = "none")
  diff.star <- sum((cases.sm.star$estimate -
  			controls.sm.star$estimate)^2)
  if (diff.star > diff.obs) p <- p + 1
  cat(i, " ")
  }
p <- p/nboot
cat("\np-value = ", p, "\n")
})

