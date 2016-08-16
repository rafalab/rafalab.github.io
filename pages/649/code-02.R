library(MASS)
set.seed(1)
x <- mvrnorm(2000,mu=c(70,190),Sigma=matrix(c(3^2,.5*3*15,.5*3*15,15^2),2,2))
bitmap("Plots/plot-02-01.png",width=6,height=6,pointsize=12,res=300,family="Helvetica")
par(mgp=c(1.25,.5,0),mar=c(2.25,2.1,1,1))
plot(x,xlab="Height",ylab="Weight",cex=.5,col="darkblue")
abline(v=mean(x[,1]),h=mean(x[,2]),lty=2,col="red")
dev.off()     

X0=73
bitmap("Plots/plot-02-02.png",width=6,height=6,pointsize=12,res=300,family="Helvetica")
par(mgp=c(1.25,.5,0),mar=c(2.25,2.1,1,1))
plot(x,xlab="Height",ylab="Weight",cex=.5,col="darkblue")
abline(v=mean(x[,1]),h=mean(x[,2]),col="red",lty=2)
abline(v=c(X0-.5,X0+.5),col="red",lwd=2)
X=x[,1]
Y=x[,2]
fit1 <- lm(Y~X)
points(X0,predict(fit1,newdata=list(X=X0)),col="orange",pch="A",cex=2)
dev.off()     

bitmap("Plots/plot-02-03.png",width=6,height=6,pointsize=12,res=300,family="Helvetica")
par(mgp=c(1.25,.5,0),mar=c(2.25,2.1,1,1))
plot(x,xlab="Height",ylab="Weight",cex=.5,col="darkblue")
abline(v=mean(x[,1]),h=mean(x[,2]),col="red",lty=2)
abline(v=c(X0-.5,X0+.5),col="red",lwd=2)
abline(fit1,col="orange",lwd=2)
points(X0,predict(fit1,newdata=list(X=X0)),col="orange",pch="A",cex=3)
dev.off()     


CD4.data <- read.table("data/cd4.data",
                       col.names=c("Time","CD4","Age","Packs",
                         "Drugs","Sex","Cesd","ID")) #READ IN DATA 
attach(CD4.data) ##attach data frame, now you can use just the names

###Figure 1
bitmap("Plots/plot-02-04.png",width=6,height=6,pointsize=12,res=300,family="Helvetica")
par(mfrow=c(1,1)) ##makes a 1,1 grid for the figure
plot(Time,CD4,pch=".",main="CD4 counts vs Time",xlab="Time since zeroconversion",ylab="CD4")##plots CD4 vs. Time 
dev.off()
