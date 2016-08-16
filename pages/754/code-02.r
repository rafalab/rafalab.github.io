####### SECTION 2 ####
####################
CD4.data <- read.table("Data/cd4.data",
                       col.names=c("Time","CD4","Age","Packs",
                         "Drugs","Sex","Cesd","ID")) #READ IN DATA 
attach(CD4.data) ##attach data frame, now you can use just the names

###Figure 1
postscript("Plots/plot-02-01.ps")
par(mfrow=c(1,1)) ##makes a 1,1 grid for the figure
plot(Time,CD4,pch=".",main="CD4 counts vs Time",xlab="Time since zeroconversion",ylab="CD4")##plots CD4 vs. Time 
dev.off()

###Figures 2
postscript("Plots/plot-02-02.ps",horizontal=T)
plot(Time,CD4,main="Regression Lines",pch=".")
abline(lm(CD4~Time))
o <- order(Time)
lines(Time[o],lm(CD4~poly(Time,2))$fitted[o],lty=2)
lines(Time[o],lm(CD4~poly(Time,3))$fitted[o],lty=4)
legend(3.75,3000,legend=c("line","parabola","cubic poly"),lty=c(1,2,4))
dev.off()

###Figure 3
###BIN SMOOTHER
postscript("Plots/plot-02-03.ps",horizontal=T)
cs <- c(-Inf,quantile(Time,probs=c(1/5,2/5,3/5,4/5)),Inf)
R <- cut(Time,cs)
plot(Time,CD4,main="Bin Smoother",pch=".")
lines(Time[o],lm(CD4~R)$fitted[o])
dev.off()
###Figure 4
###Running-mean and running-line 
library("modreg") ##this library contains the function ksmooth
postscript("Plots/plot-02-04.ps",horizontal=T)
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
