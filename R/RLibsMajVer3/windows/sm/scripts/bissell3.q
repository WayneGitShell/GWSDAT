# run this script immediately after "bissel3"
# which produces the nonparametric regression estimate
plot(Length, Flaws, xlim=c(0,1000), pch="o")
Y <- Flaws[order(Length)]
X <- sort(Length)
h <- ask("Please type a value for the smoothing parameter, h")
beta <- sum(Y)/sum(X)
abline(0,beta,col=2)
dev<-function(y,mu){
       d<-(mu-y+y*log(y/mu))
       d[y==0] <- mu[y==0]
       return(2*sum(d))
       }
W<-sm.weight(X, X, h, options=list(poly.index=0))
glm.fitted<- beta*X 
p.boot <- 0  
denom <-(W %*% X)
sm.fitted <- ((W %*% Y)/denom)*X
disp <- dev(Y,sm.fitted)/(length(Y)-1)
ts.orig <- (dev(Y,glm.fitted)-dev(Y,sm.fitted))/disp
cat("Dispersion parameter = ", disp,"\n")
cat("Test statistic = ", ts.orig,"\n") 
nboot <- 500  
cat("Bootstrap samples: ")
for (i in 1:nboot) {   
  yboot<-rpois(length(glm.fitted),glm.fitted)
  sm.fitted <- ((W %*% yboot)/denom)*X
  disp <- dev(yboot,sm.fitted)/(length(yboot)-1)
  ts.boot <- (dev(yboot,glm.fitted)-dev(yboot,sm.fitted))/disp
  if (ts.boot > ts.orig) p.boot <- p.boot + 1   
  lines(X, sm.fitted, lty=2,col=6)   
  if((i %% 100)==0) {cat(i);cat(" ")}
  }  
cat("\n")
lines(x,sm.beta*x,col=1)
cat("Observed significance = ", p.boot/nboot,"\n")
   
  
 
