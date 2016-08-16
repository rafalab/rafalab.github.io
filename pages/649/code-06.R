CD4.data <- read.table("data/cd4.data",
                       col.names=c("Time","CD4","Age","Packs",
                         "Drugs","Sex","Cesd","ID")) #READ IN DATA 
attach(CD4.data) ##attach data frame, now you can use just the names
o <- order(Time)

###First Figure
bitmap("Plots/plot-07-00.png",width=6,height=6,pointsize=12,res=300,family="Helvetica")
###Running-mean and running-line 
library("modreg") ##this library contains the function ksmooth
o <- sample(1:length(Time),400) ##to make it fast for example look at only 400
 plot(Time[o],CD4[o],pch=".",main="Running Mean")
b <- c(-2,1,3) - 0.5
e <- b + 1
for(i in 1:length(b)){  
  abline(v=c(b[i],e[i]),pch=2)
  oo <- o[Time[o]>=b[i] & Time[o]<=e[i]]
sapply(oo,function(i) points(Time[oo],CD4[oo],pch=1))
lines(range(Time[oo]),rep(mean(CD4[oo]),2))
}
aux <- ksmooth(Time[o],CD4[o],kernel="box",bandwidth=1,x.points=unique(Time[o])) ##box is default!
lines(aux$x,aux$y)
dev.off()


###Figures 5
##kernel smoother
postscript("Plots/plot-02-05.ps",horizontal=T)
plot(Time[o],CD4[o],main="Kernel Smoother",pch=".")
aux <- ksmooth(Time[o],CD4[o],kernel="normal",bandwidth=1,x.points=unique(Time[o]))
lines(aux$x,aux$y)
dev.off()

###Figure 6
postscript("Plots/plot-02-06.ps",horizontal=T)
o <- sample(1:length(Time),400) ##to make it fast for example look at only 400
xx <- sort(Time[o])
aux11 <- ksmooth(xx,c(1,rep(0,length(o)-1)),kernel="normal",bandwidth=1,x.points=unique(xx)) 
plot(aux11$x,aux11$y,type="l",xlab="Time",ylab="Weights",main="Kernels",col=2)
rug(xx)
oo<- aux11$y>6.030458e-06
points(aux11$x[oo],aux11$y[oo],lty=2,pch="x",cex=0.5)
aux12 <- ksmooth(xx,c(1,rep(0,length(o)-1)),kernel="box",bandwidth=1,x.points=unique(xx))
lines(aux12$x,aux12$y,lty=2,col=2)
oo <- aux12$y>6.030458e-06
points(aux12$x[oo],aux12$y[oo],lty=2,pch="x",cex=0.5)
##
aux21 <- ksmooth(xx,c(rep(0,round(length(o)/2)),1,rep(0,length(o)-round(length(o)/2)-1)),kernel="normal",bandwidth=1,x.points=unique(xx))
lines(aux21$x,aux21$y,type="l",col=3)
oo<- aux21$y>6.030458e-06
points(aux21$x[oo],aux21$y[oo],lty=2,pch="x",cex=0.5)
aux22 <- ksmooth(xx,c(rep(0,round(length(o)/2)),1,rep(0,length(o)-round(length(o)/2)-1)),kernel="box",bandwidth=1,x.points=unique(xx))
lines(aux22$x,aux22$y,lty=2,col=3)
oo<- aux22$y>6.030458e-06
points(aux22$x[oo],aux22$y[oo],lty=2,pch="x",cex=0.5)
##
aux31 <- ksmooth(xx,c(rep(0,round(length(o)*.9)),1,rep(0,length(o)-round(length(o)*.9)-1)),kernel="normal",bandwidth=1,x.points=unique(xx))
lines(aux31$x,aux31$y,type="l",col=4)
oo<- aux31$y>6.030458e-06
points(aux31$x[oo],aux31$y[oo],lty=2,pch="x",cex=0.5)
aux32 <- ksmooth(xx,c(rep(0,round(length(o)*.9)),1,rep(0,length(o)-round(length(o)*.9)-1)),kernel="box",bandwidth=1,x.points=unique(xx))
lines(aux32$x,aux32$y,lty=2,col=4)
oo<- aux32$y>6.030458e-06
points(aux32$x[oo],aux32$y[oo],lty=2,pch="x",cex=0.5)
dev.off()
detach("CD4.data")

library(modreg) ##this is where loess is defined for R
CD4.data <- read.table("Data/cd4.data",
		       col.names=c("Time","CD4","Age","Packs",
			 "Drugs","Sex","Cesd","ID")) #READ IN DATA 
attach(CD4.data) ##attach data frame, now you can use just the names


##three plots are exactly the same except for the degrees used.
postscript("Plots/plot-03-01.ps",horizontal=F)
o <- order(Time)
spans <-c(.05,.25,.75,.95) 
par(mfrow=c(2,2),err=-1,oma=c(0,0,3,0)) ##err=-1 so that we get no warnings 
sapply(spans,function(span){
  plot(Time,CD4,pch=".",xlab="Time since zeroconversion",ylab="CD4",main=paste("span =",span),ylim=c(0,1500))##plots CD4 vs. Time 
  lines(Time[o],loess(CD4[o]~Time[o],span=span,degree=1)$fitted,col=4,lwd=2)
  })
mtext(side=3,"Degree=1",outer=T,cex=1.3,line=0)
dev.off()

postscript("Plots/plot-03-02.ps",horizontal=F)
o <- order(Time)
spans <-c(.05,.25,.75,.95) 
par(mfrow=c(2,2),err=-1,oma=c(0,0,3,0)) ##err=-1 so that we get no warnings 
sapply(spans,function(span){
  plot(Time,CD4,pch=".",xlab="Time since zeroconversion",ylab="CD4",main=paste("span =",span),ylim=c(0,1500))##plots CD4 vs. Time 
  lines(Time[o],loess(CD4[o]~Time[o],span=span,degree=2)$fitted,col=4,lwd=2)
  })
mtext(side=3,"Degree=2, the default",outer=T,cex=1.3,line=0)
dev.off()

postscript("Plots/plot-03-03.ps",horizontal=F)
o <- order(Time)
spans <-c(.05,.25,.75,.95) 
par(mfrow=c(2,2),err=-1,oma=c(0,0,3,0)) ##err=-1 so that we get no warnings 
sapply(spans,function(span){
  plot(Time,CD4,pch=".",xlab="Time since zeroconversion",ylab="CD4",main=paste("span =",span),ylim=c(0,1500))##plots CD4 vs. Time 
  lines(Time[o],loess(CD4[o]~Time[o],span=span,degree=0)$fitted,col=4,lwd=2)
  })
mtext(side=3,"Degree=0",outer=T,cex=1.3,line=0)
dev.off()
detach("CD4.data")


Array <- read.table("Data/microarray.dat",col.names=c("X","Y"))
attach(Array)

SPAN <- 2/3
postscript("Plots/plot-03-04.ps",horizontal=T)
par(mfrow=c(1,1))
plot(X*Y,Y/X,log="xy",pch=".")
junk <- loess(log(Y/X)~log(X*Y),span=SPAN,family="gaussian",degree=2)$fitted
o <- order(X*Y)
lines((X*Y)[o],exp(junk[o]),lwd=2,lty=2,col=3)
junk <- loess(log(Y/X)~log(X*Y),span=SPAN,family="symmetric",degree=2)$fitted
lines((X*Y)[o],exp(junk[o]),lwd=2,lty=3,col=4)
legend(10^6,0.8,c("gaussian","symmetric"),lty=c(2,3),col=c(3,4))
dev.off()

detach("Array")
                 
diabetes <- read.table("Data/diabetes.dat",header=T)
attach(diabetes)
fit1 <- lm(log(C.Peptide)~Age+Base.Deficit)
fit2 <- loess(log(C.Peptide)~Age*Base.Deficit)


postscript("Plots/plot-03-05.ps",horizontal=F)
par(mfrow=c(1,1),mai=c(0,0,0,0))
newAge <- seq(min(Age),max(Age),len=40)
newBase <- seq(min(Base.Deficit),max(Base.Deficit),len=40)
newcov <- expand.grid(Age=newAge,Base.Deficit=newBase)
surf <- matrix(predict(fit2,newcov),40,40)
persp(newAge,newBase,surf,xlab="Age",ylab="Base Deficit",zlab="Predicted",theta=-35,phi=30,shade=.35,col=5)
dev.off()
detach(diabetes)

