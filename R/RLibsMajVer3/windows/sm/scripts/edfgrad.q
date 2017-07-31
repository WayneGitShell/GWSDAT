with(aircraft, {

y <- log(Span[Period==3])
n <- length(y)
plot(sort(y),(1:n)/n, type="S", 
	xlab="y", ylab="Empirical distribution function")
h  <- 0.3
x  <- 3.1
x1 <- x - h
x2 <- x + h
y1 <- length(y[y<x1])/n
y2 <- length(y[y<x2])/n
lines(c(x,x),c(0.05,1),lty=2)
lines(c(x1,x2,x2,x1),c(y1,y1,y2,y1))
text(x + diff(range(y))/20, y1 - 0.05, "2d")
text(x2 + diff(range(y))/20, (y1 + y2)/2, "k/n")
text(x, 0.02, "y")
})
