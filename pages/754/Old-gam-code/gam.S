####### SECTION 1 ####
####################
CD4.data <- read.table("Data/LDA/cd4.data",col.names=c("Time","CD4","Age","Packs","Drugs","Sex","Cesd","ID")) #READ IN DATA
attach(CD4.data) ##attach data frame, now you can use just the names



###Figure 1
postscript("plot01.ps")
par(mfrow=c(1,1)) ##makes a 1,1 grid for the figure
plot(Time,CD4,pch=".",main="Figure 1",xlab="Time since zeroconversion",ylab="CD4")##plots CD4 vs. Time 
dev.off()

###Figures 2
postscript("plot02.1.ps",horizontal=F)
#par(mfrow=c(3,2) to make all in one add this,take out the dev.off/postscript
plot(Time,CD4,main="Regression Line",pch=".")
abline(lm(CD4~Time))
o <- order(Time)
lines(Time[o],lm(CD4~poly(Time,2))$fitted[o],lty=2)
dev.off()
###Figure 3
###BIN SMOOTHER
postscript("plot02.2.ps",horizontal=F)
cs <- c(-Inf,quantile(Time,probs=c(1/5,2/5,3/5,4/5)),Inf)
R <- cut(Time,cs)
plot(Time,CD4,main="Bin Smoother",pch=".")
lines(Time[o],lm(CD4~R)$fitted[o])
dev.off()
###Figure 4
###Running-mean and running-line smootherso <- sample(1:length(Time),400)
postscript("plot02.3.ps",horizontal=F)
o <- sample(1:length(Time),400)
plot(Time[o],CD4[o],pch=".",main="Running Mean")
b <- c(-2,1,3) - 0.5
e <- b + 1
for(i in 1:length(b)){
  abline(v=c(b[i],e[i]),pch=2)
oo <- o[Time[o]>b[i] & Time[o]<e[i]]
sapply(oo,function(i) points(Time[oo],CD4[oo],pch=1))
lines(range(Time[oo]),rep(mean(CD4[oo]),2))
}
aux <- ksmooth(Time,CD4,kernel="box",bandwidth=2) ##box is default!
lines(aux$x,aux$y)
dev.off()
###Figures 5
##kernel smoother
postscript("plot02.4.ps",horizontal=F)
plot(Time,CD4,main="Kernel Smoother",pch=".")
aux <- ksmooth(Time,CD4,kernel="normal")
lines(aux$x,aux$y)
dev.off()
###Figures 6
###loess
postscript("plot02.5.ps",horizontal=F)
plot(loess(CD4~Time,span=2/3),ylim=range(CD4),main="Loess")
points(Time,CD4,pch=".")
dev.off()
###Figures 7
##splines
postscript("plot02.6.ps",horizontal=F)
o <- order(Time)
plot(Time,CD4,main="Splines",pch=".")
lines(Time[o],lm(CD4~bs(Time,knots=cs[-c(1,length(cs))]))$fitted[o])
dev.off()
detach("CD4.data")

###SECTION 2
##figure 2: HOOKES law
LENGTH <- scan("Data/LDA/hooke.data")
weight <- 0:10
lm1 <- lm(LENGTH~weight)
postscript("plot04.ps")
par(mfrow=c(1,1))
plot(weight,LENGTH,main="Figure 3: Hooke's measurments")
abline(lm1)
text(1.2,439.4,paste("esimtes:",signif(lm1$coef[1],6),signif(lm1$coef[2],3)))
text(1.2,439.35,paste("se:",0.0079,0.0013))
dev.off()


