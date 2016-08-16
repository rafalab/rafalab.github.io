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
# fs <- sapply(1:NNN,function(i){
# 	aux<-fit1$fitted+aux%*%rnorm(N,0,sigma)
# 	auxG <-matrix(aux-fit1$fitted,ncol=N)%*%SS%*%matrix(aux-fit1$fitted,nrow=N)/(sum((fit1$resid)^2)/dferr)
# 	if(auxG < qG) return(c(1,aux)) else return(c(0,aux))
# }
# )
plot(x,fit1$fitted,ylim=range(c(fit1$fitted,as.vector(fs))),type="l")
ci <- 2*sqrt(diag(SS)*sum((fit1$resid)^2)/dferr)
#polygon(c(x,rev(x)),c(fit1$fitted+ci,rev(fit1$fitted-ci)),density=-1,col=6)
lines(x,fit1$fitted+ci)
lines(x,fit1$fitted-ci)
apply(fs,2,function(yy) if(yy[1]) lines(x,yy[-1]))
dev.off()





#y <- f + rt(N,3)#plot(x,y)#lines
