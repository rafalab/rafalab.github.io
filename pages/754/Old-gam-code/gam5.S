###BIAS VARIANCE TRADE-OFF 
N <- 500
x <- seq(.09,.5,len=N)
f <- 2*sin(1/x)

y <- f + rnorm(N)
postscript("plot06.ps")
plot(x,y,main="True f(x)=5 sin(1/x) and observations")
lines(x,f,type="l")
dev.off() 

NN <- 5
results <- sapply(1:NN,function(i){
       y <- f + rnorm(N)
       ksmooth(x,y,kernel="box",bandwidth=.01)$y
     })

postscript("plot07.ps")
par(mfrow=c(2,2),mai=c(0,0,0,0),oma=c(1,1,1,1))
plot(x,f,type="l",lwd=1.5,ylim=range(c(f,as.vector(results))))
lines(x,results[,1],lty=4)
	
plot(x,f,ylim=range(results),type="l",lwd=1.5)
apply(results,2,function(aux) lines(x,aux,lty=4))
results <- sapply(1:NN,function(i){
       y <- f + rnorm(N)
       ksmooth(x,y,kernel="box",bandwidth=.1)$y
     })

plot(x,f,type="l",lwd=1.5)
lines(x,results[,1],lty=4)
	
plot(x,f,type="l",lwd=1.5)
apply(results,2,function(aux) lines(x,aux,lty=4))
dev.off()

Nl <- 25
N <- 500
x <- seq(1,100,len=N)
f <- 2*sin(1/(x+18.62)*218)
y <- f + rnorm(N)
lambdas <- seq(.001,.07,len=Nl)/N*var(y)
cvs <- sapply(lambdas,function(l) smooth.spline(x,y,all.knots=T,spar=l,cv=T)$cv.crit) 
NN <- 500
AUX <-matrix(rnorm(N*NN),ncol=N)
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
postscript("plot08.ps")
par(mfrow=c(2,3))
plot(lambdas*N/var(y)/(max(x)-min(x)^3)/mean(fit1$w),cvs,xlab="lambdas",type="l")
abline(v=(lambdas*N/var(y)/(max(x)-min(x)^3)/mean(fit1$w))[order(cvs)[1]],lty=1)
plot(lambdas*N/var(y)/(max(x)-min(x)^3)/mean(fit0$w),mse+1,xlab="lambdas",type="l")
abline(v=(lambdas*N/var(y)/(max(x)-min(x)^3)/mean(fit0$w))[order(mse)[1]],lty=2)
#plot(x,y,pch=".")
o <- sort(sample(1:length(x),50))
#plot(x,y,pch="")
plot(x,f,type="l",ylim=range(c(fit1$y,fit0$y[o])))
lines(x[o],fit1$y[o],lty=2)
lines(x[o],fit0$y[o],lty=3)	
N <- 500
x <- seq(1,100,len=N)
f <- sin(2*pi*x/100)
y <- f + rt(N,3)
N1 <- 50
lambdas <- seq(.1,5,len=Nl)/N*var(y)
cvs2 <- sapply(lambdas,function(l) smooth.spline(x,y,all.knots=T,spar=l,cv=T)$cv.crit) 
NN <- 500
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
plot(lambdas*N/var(y)/(max(x)-min(x)^3)/mean(fit11$w),cvs2,xlab="lambdas",type="l")
abline(v=(lambdas*N/var(y)/(max(x)-min(x)^3)/mean(fit00$w))[order(cvs2)[1]],lty=1)
plot(lambdas*N/var(y)/(max(x)-min(x)^3)/mean(fit00$w),mse2+sigma2,xlab="lambdas",type="l")
abline(v=(lambdas*N/var(y)/(max(x)-min(x)^3)/mean(fit00$w))[order(mse2)[1]],lty=2)
o <- sort(sample(1:length(x),100))
#plot(x,y,pch="")
plot(x,f,type="l",ylim=range(c(f,fit11$y,fit00$y[o])))
lines(x[o],fit11$y[o],lty=2)
lines(x[o],fit00$y[o],lty=3)	
dev.off()

postscript("plot08.1.ps")
par(mfrow=c(1,2))
N <- 500
x <- seq(1,100,len=N)
f <- 2*sin(1/(x+18.62)*218)
y <- f + rnorm(N)
plot(x,y)
lines(x,f)

N <- 500
x <- seq(1,100,len=N)
f <- sin(2*pi*x/100)
y <- f + rt(N,3)
plot(x,y)
lines(x,f)
dev.off()


############
#### CONFIDENCE BANDS STUFF
###############
N <- 500
x <- seq(1,100,len=N)
f <- 4*sin(2*pi*x/100)
sigma <- 1
y <- f+rt(N,3)
Y <- diag(N)
lambdas <- seq(0.1,0.7,len=10) 
cvs <- sapply(lambdas,function(l){
	cat(".")
	Y <- cbind(rep(l,N),Y)
	S <- apply(Y,1,function(yy) loess(yy[-1]~x,span=yy[1])$fitted)
	S <- diag(S)
	resid <- loess(y~x,span=l)$resid
	1/N*sum((resid/(1-S))^2)
})

l <- lambdas[order(cvs)[1]]
S <- apply(Y,1,function(yy) loess(yy~x,span=l)$fitted)
SS <- S%*%t(S)
Sf <- S%*%f
dferr <- N - 2*sum(diag(S)) + sum(diag(SS))
###TRUE
NN <- 100
AUX <- matrix(rnorm(NN*N,0,sigma),ncol=N)
AUX <- sweep(AUX,2,f,FUN="+")
Gtrue <- apply(AUX,1,function(yy){
	fit1 <- loess(yy~x,span=l)$fitted
	matrix((fit1 - Sf),ncol=N)%*%SS%*%matrix((fit1 -
Sf),nrow=N)/sum((y-fit1)^2/dferr)
}
)
dfvar <- sum(diag(SS))
Gapprox <-  dfvar*rf(NN,dfvar,dferr)
fit1 <- loess(y~x,span=l)
Gboot <- sapply(1:NN,function(i){
	ystar <- fit1$fitted + sample(fit1$resid,N,replace=T)
	fit2 <- loess(ystar~x,span=l)$fitted
	matrix((fit2-fit1$fitted),ncol=N)%*%SS%*%matrix((fit2-fit1$fitted),nrow=N)/(sum((ystar-fit2)^2)/dferr)
}
)
 
postscript("plot09.ps")
par(mfrow=c(2,2))
plot(x,y,pch=".")
lines(x,f)
lines(x,fit1$fitted)

plot(lambdas,cvs)

plot(sort(Gtrue),sort(Gapprox),ylim=range(c(Gapprox,Gboot)))
points(sort(Gtrue),sort(Gboot),pch=2)
abline(0,1)

NNN <- 7
aux <- t(chol(SS))
G <- Gboot
qG <- quantile(G,.95)
fs <- sapply(1:NNN,function(i){
	aux<-fit1$fitted+aux%*%rnorm(N,0,sigma)
	auxG <-matrix(aux-fit1$fitted,ncol=N)%*%SS%*%matrix(aux-fit1$fitted,nrow=N)/(sum((fit1$resid)^2)/dferr)
	if(auxG < qG) return(c(1,aux)) else return(c(0,aux))
}
)
plot(x,fit1$fitted,ylim=range(c(fit1$fitted,as.vector(fs))),type="l")
ci <- 2*sqrt(diag(SS)*sum((fit1$resid)^2)/dferr)
polygon(c(x,rev(x)),c(fit1$fitted+ci,rev(fit1$fitted-ci)),density=-1,col=6)
apply(fs,2,function(yy) if(yy[1]) lines(x,yy[-1]))
dev.off()

N <- 100
x <- seq(1,100,len=N)
f <- 4*sin(2*pi*x/100)
sigma <- sqrt(3)
y <- f+rt(N,3)
Y <- diag(N)
lambdas <- seq(0.1,0.7,len=10) 
cvs <- sapply(lambdas,function(l){
	cat(".")
	Y <- cbind(rep(l,N),Y)
	S <- apply(Y,1,function(yy) loess(yy[-1]~x,span=yy[1])$fitted)
	S <- diag(S)
	resid <- loess(y~x,span=l)$resid
	1/N*sum((resid/(1-S))^2)
})

l <- lambdas[order(cvs)[1]]
S <- apply(Y,1,function(yy) loess(yy~x,span=l)$fitted)
SS <- S%*%t(S)
Sf <- S%*%f
dferr <- N - 2*sum(diag(S)) + sum(diag(SS))
###TRUE
NN <- 100
AUX <- matrix(rt(NN*N,3),ncol=N)
AUX <- sweep(AUX,2,f,FUN="+")
Gtrue <- apply(AUX,1,function(yy){
	fit1 <- loess(yy~x,span=l)$fitted
	matrix((fit1 - Sf),ncol=N)%*%SS%*%matrix((fit1 -
Sf),nrow=N)/sum((y-fit1)^2/dferr)
}
)
dfvar <- sum(diag(SS))
Gapprox <-  dfvar*rf(NN,dfvar,dferr)
fit1 <- loess(y~x,span=l)
Gboot <- sapply(1:NN,function(i){
	ystar <- fit1$fitted + sample(fit1$resid,N,replace=T)
	fit2 <- loess(ystar~x,span=l)$fitted
	matrix((fit2-fit1$fitted),ncol=N)%*%SS%*%matrix((fit2-fit1$fitted),nrow=N)/(sum((ystar-fit2)^2)/dferr)
}
)
 
postscript("plot10.ps")
par(mfrow=c(2,2))
plot(x,y,pch=".")
lines(x,f)
lines(x,fit1$fitted)

plot(lambdas,cvs)

plot(sort(Gtrue),sort(Gapprox),ylim=range(c(Gapprox,Gboot)))
points(sort(Gtrue),sort(Gboot),pch=2)
abline(0,1)

NNN <- 7
aux <- t(chol(SS))
G <- Gboot
qG <- quantile(G,.95)
fs <- sapply(1:NNN,function(i){
	aux<-fit1$fitted+aux%*%rnorm(N,0,sigma)
	auxG <-matrix(aux-fit1$fitted,ncol=N)%*%SS%*%matrix(aux-fit1$fitted,nrow=N)/(sum((fit1$resid)^2)/dferr)
	if(auxG < qG) return(c(1,aux)) else return(c(0,aux))
}
)
plot(x,fit1$fitted,ylim=range(c(fit1$fitted,as.vector(fs))),type="l")
ci <- 2*sqrt(diag(SS)*sum((fit1$resid)^2)/dferr)
polygon(c(x,rev(x)),c(fit1$fitted+ci,rev(fit1$fitted-ci)),density=-1,col=6)
apply(fs,2,function(yy) if(yy[1]) lines(x,yy[-1]))
dev.off()


########MORE STUFF
N <- 1000
x <- seq(1,100,len=N)
f <- sin(2*pi*x/100)
sigma <- sqrt(3)
y <- f+rt(N,3)
Y <- diag(N)
lambdas <- seq(0.1,0.7,len=10) 
cvs <- sapply(lambdas,function(l){
	cat(".")
	Y <- cbind(rep(l,N),Y)
	S <- apply(Y,1,function(yy) loess(yy[-1]~x,span=yy[1])$fitted)
	S <- diag(S)
	resid <- loess(y~x,span=l)$resid
	1/N*sum((resid/(1-S))^2)
})

l <- lambdas[order(cvs)[1]]
S <- apply(Y,1,function(yy) loess(yy~x,span=l)$fitted)
SS <- S%*%t(S)
Sf <- S%*%f
dferr <- N - 2*sum(diag(S)) + sum(diag(SS))
###TRUE
NN <- 1000
AUX <- matrix(rt(NN*N,3),ncol=N)
AUX <- sweep(AUX,2,f,FUN="+")
Gtrue <- apply(AUX,1,function(yy){
	fit1 <- loess(yy~x,span=l)$fitted
	matrix((fit1 - Sf),ncol=N)%*%SS%*%matrix((fit1 -
Sf),nrow=N)/sum((y-fit1)^2/dferr)
}
)
dfvar <- sum(diag(SS))
Gapprox <-  dfvar*rf(NN,dfvar,dferr)
fit1 <- loess(y~x,span=l)
Gboot <- sapply(1:NN,function(i){
	ystar <- fit1$fitted + sample(fit1$resid,N,replace=T)
	fit2 <- loess(ystar~x,span=l)$fitted
	matrix((fit2-fit1$fitted),ncol=N)%*%SS%*%matrix((fit2-fit1$fitted),nrow=N)/(sum((ystar-fit2)^2)/dferr)
}
)
 
postscript("plot10.1.ps")
par(mfrow=c(2,2))
plot(x,y,pch=".")
lines(x,f)
lines(x,fit1$fitted)

plot(lambdas,cvs)

plot(sort(Gtrue),sort(Gapprox),ylim=range(c(Gapprox,Gboot)))
points(sort(Gtrue),sort(Gboot),pch=2)
abline(0,1)

NNN <- 7
aux <- t(chol(SS))
G <- Gboot
qG <- quantile(G,.95)
fs <- sapply(1:NNN,function(i){
	aux<-fit1$fitted+aux%*%rnorm(N,0,sigma)
	auxG <-matrix(aux-fit1$fitted,ncol=N)%*%SS%*%matrix(aux-fit1$fitted,nrow=N)/(sum((fit1$resid)^2)/dferr)
	if(auxG < qG) return(c(1,aux)) else return(c(0,aux))
}
)
plot(x,fit1$fitted,ylim=range(c(fit1$fitted,as.vector(fs))),type="l")
ci <- 2*sqrt(diag(SS)*sum((fit1$resid)^2)/dferr)
polygon(c(x,rev(x)),c(fit1$fitted+ci,rev(fit1$fitted-ci)),density=-1,col=6)
lines(x,fit1$fitted+ci)
lines(x,fit1$fitted-ci)
apply(fs,2,function(yy) if(yy[1]) lines(x,yy[-1]))
dev.off()





#y <- f + rt(N,3)#plot(x,y)#lines
