##to get all permutations
binary.v <-
function(n)
{
  x <- 1:(2^n)
  mx <- max(x)
  digits <- floor(log2(mx))
  ans <- 0:(digits-1); lx <- length(x)
  x <- matrix(rep(x,rep(digits, lx)),ncol=lx)
  x <- (x %/% 2^ans) %% 2
  return(x)
}
 
allperms <- function(n,k,index=1:n){
  if(n==k) return(1:n)
  tmp <- binary.v(n)
  tmp <- tmp[,colSums(tmp)==k]
  return(apply(tmp,2,function(i) index[i==1]))
}

##startcode
set.seed(1)
library(MASS)
N <- 15
p <- 10
sigma <- 5
Sigma <- matrix(1,p,p)+diag(p)
X <- mvrnorm(N,rep(0,p),Sigma)

beta <- c(p:1)/p ##2 is the intercept

Y <- 2 + X%*%beta + rnorm(N,0,sigma)
Y.test <- 2 + X%*%beta + rnorm(N,0,sigma)

rss <- list()
for(i in 1:p){
  cat(i)
  Index <- t(allperms(10,i))
  ##Because if i=1 its a vector. this turns it to crroect sized matrix
  if(i==1) Index <- t(Index)
  rss[[i]] <- apply(Index,1,function(i){
    fit1 <- lm(Y~X[,i])
    
    Yhat <- predict(fit1)
    train.rss <- sum((Y - Yhat)^2)
    test.rss <- sum((Y.test - Yhat)^2)
    
    c(train.rss, test.rss )
  })
}

bitmap(file="Plots/plot-04-01.png", type="png256",
         width=4.25, height=4.5, res=300, pointsize=8) 
plot(1:p,1:p,type="n",ylim=range(unlist(rss)),xlim=c(0,p),xlab="number of predictors",ylab="residual sum of squares",main="A little bias can be good")
for(i in 1:p){
  points(rep(i-0.15,ncol(rss[[i]])),rss[[i]][1,],col="blue")
  points(rep(i+0.15,ncol(rss[[i]])),rss[[i]][2,],col="red")
}
MIN <- sapply(rss,function(x) min(x[1,]))
lines(1:p-0.15,MIN,col="blue")
MIN <- sapply(rss,function(x) min(x[2,]))
lines(1:p+0.15,MIN,col="red")
legend(6.5,420,c("Train","Test"),col=c("blue","red"),pch=1)
dev.off()

#######################################################
### REAL DATA: PROSTAT
#######################################################
load("prostate.RData")
set.seed(1)
Y <- Prostate$lpsa
X <- Prostate[,-9]
covnames <- names(Prostate)[-9]

Sample <- sample(1:length(Y),ceiling(length(Y)/2))
Y.test <- Y[-Sample]
Y <- Y[Sample]
X.test <- X[-Sample,]
X <- X[Sample,]
p <- ncol(X)

sink("results-04.txt")
Formula <-as.formula(paste("Y~",paste(covnames,collapse="+"),sep=""))
summary(lm(Formula,data=X))
sink()

rss <- list()
for(i in 1:p){
  cat(i)
  Index <- t(allperms(p,i))
  ##Because if i=1 its a vector. this turns it to crroect sized matrix
  if(i==1) Index <- t(Index)
  rss[[i]] <- apply(Index,1,function(is){
    Formula <-as.formula(paste("Y~",paste(covnames[is],collapse="+"),sep=""))
    
    fit1 <- lm(Formula,data=X)
    Yhat <- predict(fit1)
    train.rss <- sum((Y - Yhat)^2)
    Yhat <- predict(fit1,newdata=X.test)
    test.rss <- sum((Y.test - Yhat)^2)
    
    c(train.rss, test.rss )
  })
}

bitmap(file="Plots/plot-04-02.png", type="png256",
         width=4.25, height=4.5, res=300, pointsize=8) 
plot(1:p,1:p,type="n",ylim=range(unlist(rss)),xlim=c(0,p),xlab="number of predictors",ylab="residual sum of squares",main="Prostate cancer data")
for(i in 1:p){
  points(rep(i-0.15,ncol(rss[[i]])),rss[[i]][1,],col="blue")
  points(rep(i+0.15,ncol(rss[[i]])),rss[[i]][2,],col="red")
}
MIN <- sapply(rss,function(x) min(x[1,]))
lines(1:p-0.15,MIN,col="blue")
MIN <- sapply(rss,function(x) min(x[2,]))
lines(1:p+0.15,MIN,col="red")
legend(6.5,80,c("Train","Test"),col=c("blue","red"),pch=1)
dev.off()


lambdas <- seq(0,50,len=10)
M <- length(lambdas)
train.rss <- rep(0,M)
test.rss <- rep(0,M)
betas <- matrix(0,ncol(X),M)
for(i in 1:M){
  Formula <-as.formula(paste("Y~",paste(covnames,collapse="+"),sep=""))
  fit1 <- lm.ridge(Formula,data=X,lambda=lambdas[i])
  betas[,i] <- fit1$coef
  
  scaledX <- sweep(as.matrix(X),2,fit1$xm)
  scaledX <- sweep(scaledX,2,fit1$scale,"/")
  Yhat <- scaledX%*%fit1$coef+fit1$ym
  train.rss[i] <- sum((Y - Yhat)^2)
  
  scaledX <- sweep(as.matrix(X.test),2,fit1$xm)
  scaledX <- sweep(scaledX,2,fit1$scale,"/")
  Yhat <- scaledX%*%fit1$coef+fit1$ym
  test.rss[i] <- sum((Y.test - Yhat)^2)
}

bitmap(file="Plots/plot-04-03.png", type="png256",
       width=4.25, height=4.5, res=300, pointsize=8) 
plot(lambdas,test.rss,type="l",col="red",lwd=2,ylab="RSS",ylim=range(train.rss,test.rss))
lines(lambdas,train.rss,col="blue",lwd=2,lty=2)
abline(v=11+1/9)
legend(30,30,c("Train","Test"),col=c("blue","red"),lty=c(2,1))
dev.off()


bitmap(file="Plots/plot-04-04.png", type="png256",
         width=6.25, height=4.5, res=300, pointsize=8) 
plot(lambdas,betas[1,],ylim=range(betas),type="n",ylab="Coefficients")
for(i in 1:ncol(X))
  lines(lambdas,betas[i,],type="b",lty=i,pch=as.character(i))
abline(h=0)
legend(30,0.725,covnames,pch=as.character(1:8))
dev.off()



