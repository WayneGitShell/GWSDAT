with(trawl, {

ind <- (Year == 1 & Zone == 1 & !is.na(Depth))
y <- Score1[ind]
x <- Depth[ind]
model <- sm.regression(x, y, h = 5, display = "none",
	eval.points = x)
rss.obs <- sum((y - model$estimate)^2)

p <- 0
for (i in 1:100) {
   z <- sample(y)
   model <- sm.regression(x, z, h = 5, display = "none",
   	eval.points = x)
   rss <- sum((z - model$estimate)^2)
   if (rss < rss.obs) p <- p + 1
   cat(i," ")
   }
cat("\nEmpirical p-value: ", round(p/100, 2),"\n")
})
