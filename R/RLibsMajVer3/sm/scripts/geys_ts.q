d<-geyser$duration
cat("Data are: d=(duration of geyser eruption)\n")
cat("Marginal density of d(t) first, followed by\n")
cat("estimated density of (d(t-k),d(t)), for k=1,2\n")
a<-sm.ts.pdf(d,lags=c(1,2))
