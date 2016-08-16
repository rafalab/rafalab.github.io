## BIAS VARIANCE TRADE-OFF 
##plot 01
N <- 100
x <- seq(.09,.5,len=N)
f <- 2*sin(1/x)
sigma <- 1
y <- f + rnorm(N,0,sigma)
postscript("Plots/plot-05-01.ps")
plot(x,y,main="True f(x)=5 sin(1/x) and observations")
lines(x,f,type="l")
dev.off() 

##bias and variance simulation
B <- 1000
sigma <- 1
lambdas <- seq(0.15,0.6,len=30)
trainerror <- vector("numeric",length=length(lambdas))
testerror <- vector("numeric",length=length(lambdas))
for(i in 1:B){ ## we want the same y for all competitors
  x <- sort(runif(N,.09,.5))
  f <- 2*sin(1/x)
  y <- f + rnorm(N,0,sigma)
  testy <- f + rnorm(N,0,sigma)
  for(j in seq(along=lambdas)){
    yhat <- loess(y~x,span=lambdas[j])$fitted
    #plot(x,y)
    #lines(x,yhat)
    trainerror[j] <- trainerror[j] + sum((y-yhat)^2)/B
    testerror[j] <- testerror[j] + sum((testy-yhat)^2)/B
  }
}
bitmap(file="Plots/plot-08-00.png", type="png256",width=6.25, height=4.5, res=300, pointsize=10) 
mypar(1,1)
plot(-lambdas,trainerror,ylim=range(c(trainerror,testerror)),xlab="Model complexity",ylab="Expected RSS",type="l",col=1,lty=1,xaxt="n")
lines(-lambdas,testerror,col=2,lty=2)
text(-lambdas[25],trainerror[25],"Train error")
text(-lambdas[25],testerror[25],"Test error")
dev.off()

####plot02
NN <- 5
postscript("Plots/plot-05-02.ps")
results <- sapply(1:NN,function(i){
       y <- f + rnorm(N,0,sigma)
       ksmooth(x,y,kernel="box",bandwidth=.01)$y
     })
par(mfrow=c(2,2),mai=c(0,0,0,0),oma=c(1,1,1,1))
plot(x,f,type="l",lwd=1.5,ylim=range(c(f,as.vector(results))))
lines(x,results[,1],lty=4)

plot(x,f,ylim=range(results),type="l",lwd=1.5)
apply(results,2,function(aux) lines(x,aux,lty=4))
results <- sapply(1:NN,function(i){
       y <- f + rnorm(N,0,sigma)
       ksmooth(x,y,kernel="box",bandwidth=.1)$y
     })
plot(x,f,type="l",lwd=1.5)
lines(x,results[,1],lty=4)
	
plot(x,f,type="l",lwd=1.5)
apply(results,2,function(aux) lines(x,aux,lty=4))
dev.off()


##plot03
postscript("Plots/plot-05-03.ps")
par(mfrow=c(1,2))

x <- seq(1,100,len=N)
f <- 2*sin(1/(x+18.62)*218)
y <- f + rnorm(N,0,sigma)
plot(x,y)
lines(x,f)

#N <- 500
x <- seq(1,100,len=N)
f <- sin(2*pi*x/100)
y <- f + rt(N,3)
plot(x,y)
lines(x,f)
dev.off()

########## plot04
Nl <- 50
#N <- 500
x <- seq(1,100,len=N)
f <- 2*sin(1/(x+18.62)*218)
y <- f + rnorm(N,0,sigma)
lambdas <- seq(.01,2.2,len=Nl)
cvs <- sapply(lambdas,function(l) smooth.spline(x,y,all.knots=T,spar=l,cv=T)$cv.crit) 
NN <- 200
AUX <-matrix(rnorm(N*NN,0,sigma),ncol=N)
AUX <- sweep(AUX,2,f,FUN="+")
mse <- sapply(lambdas,function(l){
  cat(".")
  aux <- cbind(rep(l,NN),AUX)
  aux<-apply(aux,1,function(yy)
	     sum((f-smooth.spline(x,yy[-1],all.knots=T,spar=yy[1])$y)^2)/500)
  mean(aux)
})
fit0 <- smooth.spline(x,y,all.knots=T,spar=lambdas[order(mse)[1]])
fit1 <- smooth.spline(x,y,all.knots=T,spar=lambdas[order(cvs)[1]])
postscript("Plots/plot-05-04.ps")
par(mfrow=c(2,3),oma=c(0,0,2,0))
plot(lambdas,cvs,xlab="lambdas",type="l")
abline(v=lambdas[order(cvs)[1]],lty=1)
plot(lambdas,mse+1,xlab="lambdas",type="l")
abline(v=lambdas[order(mse)[1]],lty=2)
o <- sort(sample(1:length(x),50))
plot(x,f,type="l",ylim=range(c(fit1$y,fit0$y[o])))
lines(x[o],fit1$y[o],lty=2,col=2)
lines(x[o],fit0$y[o],lty=3,col=3)
legend(70,0,c("truth","CV","MSE"),lty=c(1,2,3),col=c(1,2,3))

#N <- 500
x <- seq(1,100,len=N)
f <- sin(2*pi*x/100)
y <- f + rt(N,3)
N1 <- 50
cvs2 <- sapply(lambdas,function(l) smooth.spline(x,y,all.knots=T,spar=l,cv=T)$cv.crit) 
NN <- 20
AUX <-matrix(rt(N*NN,3),ncol=N)
AUX <- sweep(AUX,2,f,FUN="+")
mse2 <- sapply(lambdas,function(l){
  cat(".")
  aux <- cbind(rep(l,NN),AUX)
  aux<-apply(aux,1,function(yy)
	     sum((f-smooth.spline(x,yy[-1],all.knots=T,spar=yy[1])$y)^2)/500)
  mean(aux)
})
fit00 <- smooth.spline(x,y,all.knots=T,spar=lambdas[order(mse2)[1]])
fit11 <- smooth.spline(x,y,all.knots=T,spar=lambdas[order(cvs2)[1]])
plot(lambdas,cvs2,xlab="lambdas",type="l")
abline(v=lambdas[order(cvs2)[1]],lty=1)
plot(lambdas,mse2+sigma,xlab="lambdas",type="l")
abline(v=lambdas[order(mse2)[1]],lty=2)
o <- sort(sample(1:length(x),100))
plot(x,f,type="l",ylim=range(c(f,fit11$y,fit00$y[o])))
lines(x[o],fit11$y[o],lty=2,col=2)
lines(x[o],fit00$y[o],lty=3,col=3)
legend(70,1,c("truth","CV","MSE"),lty=c(1,2,3),col=c(1,2,3))
mtext(side=3,paste("N =",N),cex=1.2,line=0,outer=T)
dev.off()

###NOW for N=500
Nl <- 50
N <- 500
x <- seq(1,100,len=N)
f <- 2*sin(1/(x+18.62)*218)
y <- f + rnorm(N,0,sigma)
lambdas <- seq(.01,2.2,len=Nl)
cvs <- sapply(lambdas,function(l) smooth.spline(x,y,all.knots=T,spar=l,cv=T)$cv.crit) 
NN <- 20
AUX <-matrix(rnorm(N*NN,0,sigma),ncol=N)
AUX <- sweep(AUX,2,f,FUN="+")
mse <- sapply(lambdas,function(l){
  cat(".")
  aux <- cbind(rep(l,NN),AUX)
  aux<-apply(aux,1,function(yy)
	     sum((f-smooth.spline(x,yy[-1],all.knots=T,spar=yy[1])$y)^2)/500)
  mean(aux)
})
fit0 <- smooth.spline(x,y,all.knots=T,spar=lambdas[order(mse)[1]])
fit1 <- smooth.spline(x,y,all.knots=T,spar=lambdas[order(cvs)[1]])
postscript("Plots/plot-05-05.ps")
par(mfrow=c(2,3),oma=c(0,0,2,0))
plot(lambdas,cvs,xlab="lambdas",type="l")
abline(v=lambdas[order(cvs)[1]],lty=1)
plot(lambdas,mse+1,xlab="lambdas",type="l")
abline(v=lambdas[order(mse)[1]],lty=2)
o <- sort(sample(1:length(x),50))
plot(x,f,type="l",ylim=range(c(fit1$y,fit0$y[o])))
lines(x[o],fit1$y[o],lty=2,col=2)
lines(x[o],fit0$y[o],lty=3,col=3)
legend(70,0,c("truth","CV","MSE"),lty=c(1,2,3),col=c(1,2,3))

#N <- 500
x <- seq(1,100,len=N)
f <- sin(2*pi*x/100)
y <- f + rt(N,3)
N1 <- 50
cvs2 <- sapply(lambdas,function(l) smooth.spline(x,y,all.knots=T,spar=l,cv=T)$cv.crit) 
NN <- 20
AUX <-matrix(rt(N*NN,3),ncol=N)
AUX <- sweep(AUX,2,f,FUN="+")
mse2 <- sapply(lambdas,function(l){
  cat(".")
  aux <- cbind(rep(l,NN),AUX)
  aux<-apply(aux,1,function(yy)
	     sum((f-smooth.spline(x,yy[-1],all.knots=T,spar=yy[1])$y)^2)/500)
  mean(aux)
})
fit00 <- smooth.spline(x,y,all.knots=T,spar=lambdas[order(mse2)[1]])
fit11 <- smooth.spline(x,y,all.knots=T,spar=lambdas[order(cvs2)[1]])
plot(lambdas,cvs2,xlab="lambdas",type="l")
abline(v=lambdas[order(cvs2)[1]],lty=1)
plot(lambdas,mse2+sigma,xlab="lambdas",type="l")
abline(v=lambdas[order(mse2)[1]],lty=2)
o <- sort(sample(1:length(x),100))
plot(x,f,type="l",ylim=range(c(f,fit11$y,fit00$y[o])))
lines(x[o],fit11$y[o],lty=2,col=2)
lines(x[o],fit00$y[o],lty=3,col=3)
legend(70,1,c("truth","CV","MSE"),lty=c(1,2,3),col=c(1,2,3))
mtext(side=3,paste("N =",N),cex=1.2,line=0,outer=T)
dev.off()

