###CHAPTER 5: BIAS VARIANCE TRADE-OFF 
N <- 500
x <- seq(.09,.5,len=N)
f <- 2*sin(1/x)

y <- f + rnorm(N)
postscript("Plots/plot-05-01.ps")
plot(x,y,main="True f(x)=5 sin(1/x) and observations")
lines(x,f,type="l")
dev.off() 

NN <- 5

postscript("Plots/plot-05-02.ps")
results <- sapply(1:NN,function(i){
       y <- f + rnorm(N)
       ksmooth(x,y,kernel="box",bandwidth=.01)$y
     })

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


