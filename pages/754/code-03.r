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

