library(modreg)

postscript("Plots/plot-06-01.ps")
n <- 100
x <- rnorm(n);y <- rnorm(n)
lambda <- seq(0.05,1,len=50)
df <- sapply(lambda,function(l) if(l==0) return(n) else return(sum(loess(y~x,span=l)$trace.hat)))
par(mfrow=c(1,2))
plot(lambda,df,main="Loess",type="l")
lambda <- seq(0.01,2,len=100)
df <- sapply(lambda,function(l) smooth.spline(x,y,spar=l,all.k=T)$df)
plot(lambda,df,main="Smoothing spline",type="l")
dev.off()

###different definitions of degrees of freedom
postscript("Plots/plot-06-02.ps")
n <- 100
x <- sort(runif(n,0,1))
Y <- diag(n)
lambda <- seq(.02,1,len=10)
df <- matrix(0,length(lambda),3)
for(i in 1:length(lambda)){
  l <- lambda[i]
  S <- apply(Y,1,function(yy) loess(yy~x,span=l)$fitted)
  aux1 <- sum(diag(S));aux2 <- sum(diag(S%*%t(S)))
  df[i,] <- c(aux1,aux2,2*aux1-aux2)
}
par(mfrow=c(1,1))
plot(lambda,df[,1],ylim=range(df),type="l",ylab="df",log="y")
lines(lambda,df[,2],lty=2)
lines(lambda,df[,3],lty=3)
legend(.7,range(df)[2]*.95,c("tr(S)","tr(SS')","2tr(S)-tr(SS')"),lty=c(1,2,3))
dev.off()


###might as well draw the columns of the smoothing spline
postscript("Plots/plot-06-03.ps")
n <- 101
x <- sort(runif(n,0,1))
Y <- diag(n)
S <- apply(Y,1,function(yy) smooth.spline(x,yy,spar=.8,all.knots=T)$y)
par(mfrow=c(3,2))
for(i in seq(1,101,len=6))
  plot(x,S[i,],ylab=paste(i,"th row of S",sep=""),type="b")
dev.off()




n <- 30
x <- sort(runif(n,0,1))
##first linear to show example
X <- cbind(1,x)
S <- X%*%solve(t(X)%*%X)%*%t(X)
aux <- eigen(S,symmetric=T)
U <- aux$vector; D <- diag(aux$values) ###S should be UDU'

postscript("Plots/plot-06-04.ps")
par(mfrow=c(1,2),oma=c(0,0,2,0))
plot(diag(D),xlab="",ylab="Eigenvalues")
par(mfrow=c(2,2))
par(mfg=c(1,2))
plot(x,U[,1],xlab="x",ylab="First Eigenvector")
par(mfg=c(2,2))
plot(x,U[,2],xlab="x",ylab="Second Eigenvector")
mtext("Simple linear regression",outer=T,side=3,line=0,cex=1.5)
dev.off()
 
###NOW for smoothing splines
n <- 30
x <- sort(runif(n,0,1))
postscript("Plots/plot-06-05.ps")
Y <- diag(n)
S <- apply(Y,1,function(yy) smooth.spline(x,yy,spar=.6,all.knots=T)$y)
aux <- eigen(S,symmetric=T)
U <- aux$vector; D <- diag(aux$values) ###S should be UDU'
par(mfrow=c(1,1))
plot(diag(D),xlab="",ylab="Eigenvalues")
dev.off()

postscript("Plots/plot-06-06.ps")
par(mfcol=c(5,2),mai=c(.3,.5,.1,.3),oma=c(0,0,2,0))
for(i in 1:10)
  plot(x,U[,i],xlab="x",ylab=paste(i,"th Eigenvector",sep=""),type="l")
mtext("Smoothing spline",outer=T,side=3,line=0,cex=1.5)
dev.off()

postscript("Plots/plot-06-07.ps")
par(mfcol=c(5,2),mai=c(.3,.5,.1,.3),oma=c(0,0,2,0))
for(i in 11:20)
  plot(x,U[,i],xlab="x",ylab=paste(i,"th Eigenvector",sep=""),type="l")
mtext("Smoothing spline",outer=T,side=3,line=0,cex=1.5)
dev.off()


postscript("Plots/plot-06-08.ps")
par(mfcol=c(5,2),mai=c(.3,.5,.1,.3),oma=c(0,0,2,0))
for(i in 21:30)
  plot(x,U[,i],xlab="x",ylab=paste(i,"th Eigenvector",sep=""),type="l")
mtext("Smoothing spline",outer=T,side=3,line=0,cex=1.5)
dev.off()


###REACT STUFF
n <- 48
x <- (1:n)/n
y <- scan("Data/mouse-bt.dat")
U <- cbind(rep(1/sqrt(n),n),
           sapply(1:(n/2-1),function(k) sqrt(2/n)*cos(2*pi*k*x)),
           sapply(1:(n/2-1),function(k) sqrt(2/n)*sin(2*pi*k*x)),
           1/sqrt(n)*cos(pi*n*x))


postscript("Plots/plot-06-09.ps")
par(mfrow=c(5,2),mai=c(.3,.5,.1,.3),oma=c(0,0,2,0))
for(i in 1:5){
  plot(x,U[,i+1],xlab="x",ylab=paste(i,"th FFT row",sep=""),type="l")
  plot(x,U[,n/2+i],xlab="x",ylab=paste(i,"th FFT row",sep=""),type="l")
}
dev.off()



##fft of data
aux <- fft(y)/sqrt(n)
z <- c(Re(aux[1]),
       as.vector(t(matrix(c(Re(aux[2:(n/2)]),Im(aux[2:(n/2)])),ncol=2))),
       Re(aux[n/2+1]))
##this functions recieves a fft and returns an estimate of f
ifft <- function(z){
  n <- length(z)
  aux <- complex(real=z[seq(2,n-1,2)],im=z[seq(3,n-1,2)])
  yhat <-Re(fft(c(z[1],aux,z[n],rev(Conj(aux))),inv=T)/sqrt(n))
}

postscript("Plots/plot-06-10.ps")
par(mfrow=c(2,2))
for(m in c(1,4,8,23)){
  plot(x,y,main=paste(m,"non zero"))
  zhat <- z*c(rep(1,1+2*m),rep(0,n-2*m-1))
  lines(x,ifft(zhat))
}
dev.off()


###We need to define the average and PAV functions
average_function(y, wt = rep(1, length(y)))
{
# compute a weighted average of a vector, y
        if(any(is.na(wt))) stop("NA's not allowed for wt")
        if(any(wt < 0))
                stop("wt must be a vector of NON-NEGATIVE weights")
        if(length(wt) != length(y)) stop(
                "y and wt must be vectors of the same length")
# if any observations have Infinite weight, return the simple
# (unweighted) average of only those observations (giving no
# weight to observations with finite weight)
        if(any(wt == Inf)) {
                wt[wt < Inf] <- 0
                wt[wt == Inf] <- 1
        }
# if all weights are zero, return the simple (unweighted)
# average of y
        if(sum(wt) == 0)
                wt <- rep(1, length(wt))
        return(sum((y * wt)/sum(wt)))
}


PAV_function(y, wt = rep(1,p))
{
### This is a modification of Derick's PAV program
### (Weighted) Pool-Adjacent-Violators (PAV) algorithm
### for non-parametric monotonic (decreasing) regression of y on x
  n <- length(y)
  if(n != length(wt))
    stop("y, and wt must be vectors of equal length")
  yhat <- y       # initialize while loop
  j <- count <- 1
  k <- 2
  support <- vector("numeric", n)
  support[count] <- j
  while(k <= n) {
    while(yhat[j] < yhat[k]) {
      yhat[j:k] <- average(y[j:k], wt[j:k])
      if(yhat[support[count]] < yhat[k]) {
        j <- support[count]
        if(count > 1)
          count <- count - 1
      }
      else {
        k <- ifelse(k == n, k, k + 1)
      }
    }
    count <- count + 1
    support[count] <- j
    j <- k
    k <- k + 1
  }
  return(y = yhat, wt)
}
postscript("Plots/plot-06-11.ps")
par(mfrow=c(2,2))
plot(x,y,xlab="Time",ylab="Temperature",type="n",main="Harmonic Model")
points(x,y,col=4)
m <- 4
yhat <- z*c(rep(1,1+2*m),rep(0,n-2*m-1))
lines(x,ifft(yhat),col=9)
plot(2:48,abs(z[-1]),xlab="coefficient of z",ylab="Absolute value",type="h")
points(2:(1+2*m),abs(z[2:(1+2*m)]),col=3)
points((2+2*m):n,abs(z[(2+2*m):n]),col=4,pch=4)
abline(h=0)
plot(x,y,xlab="Time",ylab="Temperature",type="n",main="REACT")
points(x,y,col=4)
sigma2 <- .1520197 ##we got this from somewhere else
g <- 1 - sigma2/z^2
fhat <- PAV(g, z^2)$y
fhat <- sapply(fhat, function(x) max(x, 0))
fit1 <- ifft(z*fhat)
lines(x,fit1,col=9)
plot(2:48,abs(z[-1]),xlab="coefficient of z",ylab="Absolute value",type="h")
points(2:n,abs(z[2:n]),col=3)
points(2:n,abs(z*fhat)[-1],col=4,pch=4)
abline(h=0)
dev.off()


postscript("Plots/plot-06-12.ps")
Y <- matrix(scan("Data/all-mouse-bt.dat"),48,12,byrow=T)
Y <- sweep(Y,2,apply(Y,2,mean))
par(mfrow=c(1,1))
plot(rep(x,12),as.vector(Y),xlab="Time",ylab="Temperature")

lines(x,apply(Y,1,mean),lwd=3)
lines(x,fit1-mean(fit1),col=3,lty=2,lwd=3)
lines(x,ifft(yhat)-mean(ifft(yhat)),col=4,lty=3,lwd=3)
legend(0.05,1.5,c("Average","REACT","Harmonic"),lty=c(1,2,3),col=c(1,3,4))
dev.off()

##the rest is code-06.s... becuase of the wavelet module

