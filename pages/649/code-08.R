postscript("Plots/plot-05-06.ps")
y <- read.table("Data/cholostyramine.dat",header=T)
attach(y)
plot(compliance,improvement)
abline(lm(improvement~compliance,data=y),col=4,lty=1)
o <- order(compliance)
lines(compliance[o],loess(improvement~compliance,data=y)$fitted[o],col=2,lty=2)
dev.off()



###bootsrap.. there is bootstrap function in R, but we need to download library
postscript("Plots/plot-05-07.ps")
B <- 50
N <- length(compliance)
fit1 <- lm(improvement~compliance,data=y)
fit2 <- loess(improvement~compliance,data=y)
thetas <- c(predict(fit1,data.frame(compliance=c(60,80,100))),
            predict(fit2,data.frame(compliance=c(60,80,100))))
par(mfrow=c(1,2))
aux <- matrix(sample(1:N,B*N,replace=T),B,N)
plot(compliance,improvement,type="n")
abline(h=0,v=60)
plot(compliance,improvement,type="n")
abline(h=0,v=60)
thetastars <- apply(aux,1,function(o){
  fit1 <- lm(improvement~compliance,data=y,subset=o)
  fit2 <- loess(improvement~compliance,data=y,subset=o)
  par(mfg=c(1,1,1,2))
  abline(fit1)
  par(mfg=c(1,2,1,2))
  oo <- order(compliance[o])
  lines(compliance[o][oo],fit2$fitted[oo])
  c(predict(fit1,data.frame(compliance=c(60,80,100))),
    predict(fit2,data.frame(compliance=c(60,80,100))))
})
dev.off()
detach("y")

##this prints out the results
junk <- rbind(thetas,
              apply(thetastars,1,function(x) sqrt(var(x))))

print(round(junk))



############
#### CONFIDENCE BANDS STUFF
###############
N <- 100
x <- sort(runif(N,1,100))#seq(1,100,len=N)
f <- sin(2*pi*x/100)
y <- f+rnorm(N,0,1)
Y <- diag(N)
l <- .7
S <- apply(Y,1,function(yy) loess(yy~x,span=l)$fitted)
aux <- svd(S) ##then ss' is ud^2u'
SS <- aux$u%*%diag(aux$d^2)%*%t(aux$u) ##this is ss'
dfvar <- sum(diag(SS))
dferr <- N - 2*sum(diag(S)) + dfvar
###TRUE 
NN <- 5000
AUX <- matrix(rnorm(NN*N,0,1),ncol=N)
Gtrue <- apply(AUX,1,function(error){
  yy <- error+f;resid <- yy-S%*%yy
  sum(error^2)/(sum(resid^2)/dferr)})
fit1 <- S%*%y
residstar <- y - fit1
B <- 500
Gboot <- rep(0,B)
for(i in 1:B){
  	error <- sample(residstar,N,replace=T)
        yy <- error+fit1;resid <- yy-S%*%yy
        Gboot[i] <- sum(error^2)/(sum(resid^2)/dferr)
         }
Gapprox <-dferr+  dfvar*rf(NN,dfvar,dferr) 


postscript("Plots/plot-05-08.ps")
par(mfrow=c(1,1))
plot(x,y)
lines(x,f,col=3)
lines(x,fit1,lty=2,col=4)
dev.off()

postscript("Plots/plot-05-09.ps")
par(mfrow=c(1,2))
RANGE <- range(Gtrue,Gapprox,Gboot)
qqplot(Gtrue,Gboot,ylim=RANGE,xlim=RANGE,pch="B",col=3)
lines(sort(Gtrue),sort(Gapprox),pch="F",col=4)
abline(0,1)
##NOW we make bootstrap curves
NNN <- 7
sigmahat <- sqrt((sum(((diag(N)-S)%*%y)^2)/dferr))
G <- Gboot ##we can use others here like Gapprox
qG <- quantile(G,.95)
fs <- sapply(1:NNN,function(i){
	resid<- rnorm(N,0,sigmahat)
        auxG <-  sum(resid^2)/(sigmahat^2)
        ##notice SS' = [UD][UD]'
        newf <- fit1+ aux$u%*%diag(aux$d)%*%resid
        if(auxG < qG) return(c(1,newf))
        else return(c(0,newf))
}
)
ci <- 2*sqrt(diag(SS)*sigmahat^2)
plot(x,fit1,ylim=range(c(fit1+ci,fit1-ci,as.vector(fs[-1,]))),type="l")
polygon(c(x,rev(x)),c(fit1+ci,rev(fit1-ci)),col=655,border=F)
lines(x,fit1)
apply(fs,2,function(yy) lines(x,yy[-1],col=3+yy[1],lty=3-yy[1]))
dev.off()


###NOW for t-distribution
N <- 100
x <- sort(runif(N,1,100))#seq(1,100,len=N)
f <- sin(2*pi*x/100)
y <- f+rt(N,3)
Y <- diag(N)
l <- .7
S <- apply(Y,1,function(yy) loess(yy~x,span=l)$fitted)
aux <- svd(S) ##then ss' is ud^2u'
SS <- aux$u%*%diag(aux$d^2)%*%t(aux$u) ##this is ss'
dfvar <- sum(diag(SS))
dferr <- N - 2*sum(diag(S)) + dfvar
###TRUE 
NN <- 5000
AUX <- matrix(rt(NN*N,3),ncol=N)
Gtrue <- apply(AUX,1,function(error){
  yy <- error+f;resid <- yy-S%*%yy
  sum(error^2)/(sum(resid^2)/dferr)})
fit1 <- S%*%y
residstar <- y - fit1
B <- 1000
Gboot <- rep(0,B)
for(i in 1:B){
  	error <- sample(residstar,N,replace=T)
        yy <- error+fit1;resid <- yy-S%*%yy
        Gboot[i] <- sum(error^2)/(sum(resid^2)/dferr)
         }
Gapprox <-dferr+  dfvar*rf(NN,dfvar,dferr) 



postscript("Plots/plot-05-10.ps")
par(mfrow=c(1,2))
RANGE <- range(Gtrue,Gapprox,Gboot)
qqplot(Gtrue,Gboot,ylim=RANGE,xlim=RANGE,pch="B",col=3)
lines(sort(Gtrue),sort(Gapprox),pch="F",col=4)
abline(0,1)
##NOW we make bootstrap curves
NNN <- 7
sigmahat <- sqrt((sum(((diag(N)-S)%*%y)^2)/dferr))
G <- Gboot ##we can use others here like Gapprox
qG <- quantile(G,.95)
fs <- sapply(1:NNN,function(i){
	resid<- rnorm(N,0,sigmahat)
        auxG <-  sum(resid^2)/(sigmahat^2)
        ##notice SS' = [UD][UD]'
        newf <- fit1+ aux$u%*%diag(aux$d)%*%resid
        if(auxG < qG) return(c(1,newf))
        else return(c(0,newf))
}
)
ci <- 2*sqrt(diag(SS)*sigmahat^2)
plot(x,fit1,ylim=range(c(fit1+ci,fit1-ci,as.vector(fs[-1,]))),type="l")
polygon(c(x,rev(x)),c(fit1+ci,rev(fit1-ci)),col=655,border=F)
lines(x,fit1)
apply(fs,2,function(yy) lines(x,yy[-1],col=3+yy[1],lty=3-yy[1]))
dev.off()

