###################################################
### chunk number 1: Rstart
###################################################
library(RColorBrewer)
set.seed(1)
par(mar=c(2.5,2.5,1.6,1.1),mgp=c(1.5,.5,0))
palette(brewer.pal(8,"Dark2"))
datadir="http://www.biostat.jhsph.edu/bstcourse/bio751/data"


###################################################
### chunk number 2: readInHeights
###################################################
dat=read.csv(file.path(datadir,"USheights_subsample.csv"))
x=dat$Height[dat$Gender==1] ##pick males
length(x)


###################################################
### chunk number 3: uniqueHeights
###################################################
length(unique(x))


###################################################
### chunk number 4: boxplot
###################################################
quantile(x,c(0:4)/4)
boxplot(x,main="US male heights")


###################################################
### chunk number 5: hist
###################################################
hist(x,main="US male heights",xlab="Heights in inches",breaks=seq(55,81))


###################################################
### chunk number 6: cdf
###################################################
plot(ecdf(x))


###################################################
### chunk number 7: mean
###################################################
mean(x)
sd(x)


###################################################
### chunk number 8: pnorm
###################################################
dnorm(0) ##this is phi
pnorm(0) ##nad this is Phi
###to get proportion with 1SD, 2SD, 3SD:
pnorm(1)-pnorm(-1)
pnorm(2)-pnorm(-2)
pnorm(3)-pnorm(-3)


###################################################
### chunk number 9: bell
###################################################
z=seq(-4,4,len=100)
plot(z,dnorm(z),type="l")


###################################################
### chunk number 10: qqplot
###################################################
qqnorm(x)
qqline(x)


###################################################
### chunk number 11: allheights
###################################################
y=dat$Height
par(mfrow=c(1,2))
hist(y)
qqnorm(y)
qqline(y)


###################################################
### chunk number 12: allheights
###################################################
y=dat$Height
z=split(y,dat$Gender)
par(mfrow=c(1,1))
plot(density(y),col="black",,ylim=c(0,0.15),main="densities")
lines(density(z[[1]]),col="pink")
lines(density(z[[2]]),col="blue")
legend("topright",c("all","female","male"),lty=1,col=c("black","pink","blue"))


###################################################
### chunk number 13: kurtosis
###################################################
##note we can read from the net
dat=read.table(file.path(datadir,"babies.data"),header=TRUE) 

names(dat)
par(mfrow=c(2,2))
for(i in c(0,1)){
  y=dat$bwt[dat$smoke==i] ##smoking i=1, non smoking i=0
  qqnorm(y,main=paste("smoke= ",i))
  qqline(y)
  z=(y-mean(y))/sd(y)
  print(mean(z^4)) ##observed kurtosis

###Simulations: Whats the sample distribution of kurtosis
  M=10000 ##number of simulations
  N=length(y) ##sample size
  ystar=matrix(rnorm(M*N,mean(y),sd(y)),N,M) ##each row is a dataset
  zstar= (ystar - rowMeans(ystar))/apply(ystar,1,sd) 
  kurt=rowMeans(zstar^4)
  hist(kurt,main=paste("smoke= ",i))
  abline(v=mean(z^4))
}


###################################################
### chunk number 14: dbinom
###################################################
k=c(0:10)
plot(k,dbinom(k,10,9/19),type="h")


###################################################
### chunk number 15: clt
###################################################
par(mfrow=c(2,2))
p=9/19
for(n in c(5,10,50,100)){
  ks=0:n
  plot(ks,dbinom(ks,n,p),type="h")
  mu=n*p
  sd=sqrt(n*p*(1-p))
  normapprox=pnorm(ks+0.5,mu,sd)-
    pnorm(ks-0.5,mu,sd)
  lines(ks,normapprox,col=2)
}
legend("topleft",c("exact","approx"),col=c("black","red"),lty=1)


###################################################
### chunk number 16: lazy
###################################################
zscore = function(n,p=18/38,a=100)
  (  (n-a)/2 - n*p ) / sqrt(n * p * (1-p) )

par(mfrow=c(1,1))
n=seq(1,10000)
plot(n,zscore(n),type="l")
abline(h=qnorm(.9999))
n[which.min(abs(zscore(n)-qnorm(.9999)))]


###################################################
### chunk number 17: coverage
###################################################
p=seq(0.05,0.95,0.05) ##try different ps and Ns

getcover=function(p,n=25,B=10000){
  X=sample(c(0,1),B*n,replace=TRUE,prob=c(1-p,p))
  X=matrix(X,B,n) ##every row is a sample
  phat=rowMeans(X)
  se1=sqrt(p*(1-p)/n)
  se2=sqrt(phat*(1-phat)/n)
  c(mean(abs(phat-p) < 1.96*se1 ), ##this the coverage. should be 0.95
    mean(abs(phat-p) < 1.96*se2) )
}
  

cover=sapply(p,getcover)
matplot(p,t(cover),type="l")
abline(h=0.95)


###################################################
### chunk number 18: ci
###################################################
X=scan(file.path(datadir,"medicalcost.csv"))
mean(X)+c(-2,2)*sd(X)/sqrt(length(X))


###################################################
### chunk number 19: bootstrap
###################################################
X=scan("data/medicalcost.csv")

B=1000 ##number of bootstrap samples
Xbarstar=sapply(1:B,function(i) mean(sample(X,replace=TRUE)))
par(mfrow=c(1,2))
hist(Xbarstar)
qqnorm(Xbarstar)
qqline(Xbarstar)
quantile(Xbarstar,c(0.025,0.975))


###################################################
### chunk number 20: medianbootstrap
###################################################
X=scan("data/medicalcost.csv")
median(X)
B=1000 ##number of bootstrap samples
medstar=sapply(1:B,function(i) median(sample(X,replace=TRUE)))
par(mfrow=c(1,2))
hist(medstar)
qqnorm(medstar)
qqline(medstar)
quantile(medstar,c(0.025,0.975))


###################################################
### chunk number 21: hyper
###################################################
k=0:4
dhyper(k,4,4,4)


###################################################
### chunk number 22: ttest
###################################################
dat=read.table(file.path(datadir,"babies.data"),header=TRUE)

Y=dat$bwt[dat$smoke==1]
X=dat$bwt[dat$smoke==0]

mean(Y)-mean(X)


###################################################
### chunk number 23: ttest2
###################################################
sd(X)/sqrt(length(X))
sd(Y)/sqrt(length(Y))


###################################################
### chunk number 24: ttest3
###################################################
t.test(Y,X)


###################################################
### chunk number 25: weight
###################################################
dat=read.csv(file.path(datadir,"USMaleHeightsAndWeights.csv"))
attach(dat)
hist(weight)
print(mean(weight))
print(sd(weight))


###################################################
### chunk number 26: height
###################################################
print(mean(height))
print(sd(height))


###################################################
### chunk number 27: sdline
###################################################
plot(height,weight)
x=seq(min(height),max(height),len=300)
lines(x,mean(weight)+(x-mean(height))*sd(weight)/sd(height),col=2)
abline(h=mean(weight),col=2)
abline(v=71+c(-1,1)*.25,col=3)


###################################################
### chunk number 28: hist2
###################################################
y=weight[round(height)==71]
hist(y)
mean(y)


###################################################
### chunk number 29: scatter
###################################################
plot(height,weight)
x=seq(min(height),max(height),len=300)
lines(x,mean(weight)+(x-mean(height))*sd(weight)/sd(height),col=2)
abline(h=mean(weight),col=2)
heights=sort(unique(height))
preds=sapply(heights,function(h) mean(weight[height==h]))
lines(heights,preds,type="b",col=1,pch=15,lwd=2)


###################################################
### chunk number 30: jointpdf
###################################################
tab=table(height,weight)
hs=as.numeric(rownames(tab))
ws=as.numeric(colnames(tab))
image(hs,ws,tab,col=brewer.pal(9,"Blues"))


###################################################
### chunk number 31: scatterdiffcor
###################################################
library(mvtnorm)
mypar(2,2)
rho=0;plot(rmvnorm(500,c(0,0),matrix(c(1,rho,rho,1),2,2)),xlab="x",ylab="y")
rho=0.5;plot(rmvnorm(500,c(0,0),matrix(c(1,rho,rho,1),2,2)),xlab="x",ylab="y")
rho=-0.5;plot(rmvnorm(500,c(0,0),matrix(c(1,rho,rho,1),2,2)),xlab="x",ylab="y")
rho=0.9;plot(rmvnorm(500,c(0,0),matrix(c(1,rho,rho,1),2,2)),xlab="x",ylab="y")


###################################################
### chunk number 32: samplecor
###################################################
cor(weight,height)
plot(height,weight)
x=seq(min(height),max(height),len=300)
lines(x,mean(weight)+(x-mean(height))*sd(weight)/sd(height),col=2)
abline(h=mean(weight),col=2)
heights=sort(unique(height))
preds=sapply(heights,function(h) mean(weight[height==h]))
lines(heights,preds,type="b",col=1,pch=15,lwd=2)
linepred=mean(weight)+cor(weight,height)*(x-mean(height))*sd(weight)/sd(height)
lines(x,linepred,col=3)


###################################################
### chunk number 33: nonlinear
###################################################
x=rnorm(500,0,2)
y=-x^2+rnorm(100)
plot(x,y)
cor(x,y)

detach(dat)


###################################################
### chunk number 34: readin
###################################################
x=read.csv(file.path(datadir,"hcmv.csv"))[,2]
plot(x,rep(1,length(x)),xlab="locations")


###################################################
### chunk number 35: bin
###################################################
breaks=seq(0,4000*round(max(x)/4000),4000)
tmp=cut(x,breaks)
counts=table(tmp)
locations=(breaks[-1]+breaks[-length(breaks)])/2
plot(locations,counts,type="l",ylab=)


###################################################
### chunk number 36: lambdaestimate
###################################################
lambda=length(x)/length(counts)


###################################################
### chunk number 37: poisqq
###################################################
theoretical=qpois(1:296/297,lambda)
qqplot(theoretical,counts)
abline(0,1)


###################################################
### chunk number 38: chisq
###################################################
observed=table(counts)
expected=57*dpois(as.numeric(names(observed)),lambda)
cbind(expected,observed)


###################################################
### chunk number 39: combine
###################################################
tmp=counts;tmp[tmp<2] <- 2;tmp[tmp>9]<-9
observed=table(tmp)
expected=57*c(ppois(2,lambda),dpois(3:8,lambda),1-ppois(8,lambda))
cbind(expected,observed)
1-pchisq(sum((observed-expected)^2/expected),6)


###################################################
### chunk number 40: readinadmissions
###################################################
dat=read.csv(file.path(datadir,"admissions.csv"))
dat$total=dat$Percent*dat$Number/100
##percent men get in
sum(dat$total[dat$Gender==1]/sum(dat$Number[dat$Gender==1]))
##percent women get in
sum(dat$total[dat$Gender==0]/sum(dat$Number[dat$Gender==0]))


###################################################
### chunk number 41: bymajor
###################################################
y=cbind(dat[1:6,c(1,3)],dat[7:12,3])
colnames(y)[2:3]=c("Male","Female")
y


###################################################
### chunk number 42: totals
###################################################
y=cbind(dat[1:6,c(1,2)],dat[7:12,2])
colnames(y)[2:3]=c("Male","Female")
y


###################################################
### chunk number 43: confounding
###################################################
y=cbind(dat[1:6,5],dat[7:12,5])
y=sweep(y,2,colSums(y),"/")*100
x=rowMeans(cbind(dat[1:6,3],dat[7:12,3]))
matplot(x,y,xlab="percent that gets in the major",ylab="percent that applies to major")
legend("topleft",c("Male","Female"),col=c(1,2),pch=c("1","2"))


###################################################
### chunk number 44: stratbymajor
###################################################
y=cbind(dat[1:6,3],dat[7:12,3])
matplot(1:6,y,xaxt="n",xlab="major",ylab="percent")


###################################################
### chunk number 45: lm
###################################################
mean(y[,1]-y[,2])


###################################################
### chunk number 46: readinbaby
###################################################
dat=read.table(file.path(datadir,"babies.data"),header=TRUE)


###################################################
### chunk number 47: removeoutliers
###################################################
##anoying use of numbers for NA
for(i in 1:ncol(dat)){
  print(names(dat)[i])
  print(table(dat[,i]))
}
##take out
dat[dat==999] <- NA
dat$height[dat$height==99]<-NA
dat$age[dat$age==99]<-NA
dat[dat[,7]==9,]<-NA


###################################################
### chunk number 48: takeoutNAs
###################################################
###TAKE OUT NAS
Index=which( rowMeans(is.na(dat)) == 0 )
dat=dat[Index,]


###################################################
### chunk number 49: corr
###################################################
round(cor(dat,use="complete"),2)


###################################################
### chunk number 50: heightsmoking
###################################################
y=dat$bwt
x=dat$height
z=dat$smoke
mypar()
plot(x,y,col=z+1)
abline(lm(y~x,subset=z==1),col=2)
abline(lm(y~x,subset=z==0),col=1)


###################################################
### chunk number 51: regexp
###################################################
fit1=lm(bwt~smoke,data=dat)
fit2=lm(bwt~height+smoke,data=dat)
summary(fit1)
summary(fit2)

summary(lm(y~z))
summary(lm(y~x))
summary(lm(y~x+z))


###################################################
### chunk number 52: readinpm
###################################################
dat=read.csv(file.path(datadir,"ny-pm10.csv"))
plot(dat$pm10,dat$death)
dat=dat[which(!is.na(dat$pm10)),]
abline(lm(death~pm10,data=dat))


###################################################
### chunk number 53: tempm10
###################################################
mypar(1,2)
plot(dat$temp,dat$pm10)
plot(dat$temp,dat$death)


###################################################
### chunk number 54: check2variables
###################################################
Indexes=split(1:nrow(dat),cut(dat$temp,quantile(dat$temp,c(0,.25,.5,.75,1))))
mypar(2,2)
for(i in seq(along=Indexes)){
  Index=Indexes[[i]]
  x=dat$pm10[Index]
  y=dat$death[Index]
  plot(x,y,xlim=range(dat$pm10),ylim=range(dat$death),main=names(Indexes)[i])
  abline(lm(y~x))
}


###################################################
### chunk number 55: regression
###################################################
summary(lm(death~temp+pm10,data=dat))


###################################################
### chunk number 56: readinmouse
###################################################
dat=read.table(file.path(datadir,"mouse.data"),header=TRUE,as.is=TRUE,comment.char="")
colnames(dat)
table(dat[,c(1,3)])


###################################################
### chunk number 57: lm
###################################################
lm(weight~sex,data=dat)$coef
tapply(dat$weight,dat$sex,mean)


###################################################
### chunk number 58: firstanove
###################################################
fragment=rep("No trisomy",nrow(dat))
DNAlevels=c("141G6","152F7", "230E8", "285E6")
tmpIndex=dat$tg==1
fragment[tmpIndex]=DNAlevels[dat$DNA[tmpIndex]]
dat$fragment=factor(fragment,levels=c("No trisomy",DNAlevels))
summary(lm (weight~fragment,data=dat))


###################################################
### chunk number 59: aov
###################################################
summary(aov(weight~fragment,data=dat))


###################################################
### chunk number 60: stratifyanova
###################################################
YLIM=range(dat$weight)
mypar(2,1)
boxplot(split(dat$weight,dat$fragment),varwidth=TRUE,ylim=YLIM)
boxplot(split(dat$weight,dat$sex),varwidth=TRUE,ylim=YLIM)


###################################################
### chunk number 61: stratifyanova2
###################################################
###transgenic by sex
Index=dat$sex==1
mypar(2,1)
boxplot(split(dat$weight[Index],dat$fragment[Index]),varwidth=TRUE,main="Male",ylim=YLIM)
boxplot(split(dat$weight[!Index],dat$fragment[!Index]),varwidth=TRUE,main="Female",ylim=YLIM)


###################################################
### chunk number 62: twowayanova
###################################################
mypar(4,2)
for(i in unique(dat$DNA)){
  Index=dat$DNA==i
  summary(aov(weight~sex+tg,data=dat,subset=Index))
  summary(lm(weight~sex+tg,data=dat,subset=Index))

  boxplot(split(dat$weight[Index],paste(dat$sex,dat$tg)[Index]),ylim=YLIM)

  summary(aov(weight~sex*tg,data=dat,subset=Index))
  summary(lm(weight~sex*tg,data=dat,subset=Index))

  boxplot(split(lm(weight~sex*tg,data=dat,subset=Index)$resid,dat$cage[Index]))
  abline(h=0)
}


###################################################
### chunk number 63: readinpoll
###################################################
tab=read.delim("http://www.biostat.jhsph.edu/bstcourse/bio751/data/poll.txt",as.is=TRUE)
###extract year
Index08=grep("08",tab$Dates)
Index07=grep("07",tab$Dates)
year=rep(0,nrow(tab))
year[Index08]="2008"
year[Index07]="2007"

###extract day
d=sapply(strsplit(tab$Dates,"-"),function(x)x[1])
d=gsub("/08","",d)
d=paste(year,d,sep="/")
d=strptime(d,format="%Y/%m/%d")
d=d-strptime("2008/11/4",format="%Y/%m/%d")
tab$day=d

##create week
week=round(d/7)
tab$week=week

##and finally, create the outomce
tab$diff=tab$Obama-tab$McCain


###################################################
### chunk number 64: filterpolls
###################################################
###keep weeks for which we have 3 data points
tmp1=table(week)
N1=3
keepIndex= week%in%as.numeric(names(tmp1)[tmp1>=N1])
tab2=tab[keepIndex,]

###keep Pollsters with 10 or more
tmp2=table(tab2$Pollster)
N2=10
keepIndex= tab2$Pollster%in%names(tmp2)[tmp2>=N2]
tab2=tab2[keepIndex,]


###################################################
### chunk number 65: justmean
###################################################
mean(y)+c(-2,2)*sd(y)/sqrt(length(y))


###################################################
### chunk number 66: anovapoll
###################################################
week=factor(-tab2$week)
pollster=factor(tab2$Pollster)
contrasts(pollster)=contr.sum
y=tab2$diff

lm1=lm(y~week+pollster)
summary(lm1)


###################################################
### chunk number 67: 
###################################################
summary(aov(y~week+pollster))
summary(aov(y~pollster+week))


###################################################
### chunk number 68: readinpoll
###################################################
tab=read.delim("http://www.biostat.jhsph.edu/bstcourse/bio751/data/poll.txt",as.is=TRUE)
###extract year
Index08=grep("08",tab$Dates)
Index07=grep("07",tab$Dates)
year=rep(0,nrow(tab))
year[Index08]="2008"
year[Index07]="2007"

###extract day
d=sapply(strsplit(tab$Dates,"-"),function(x)x[1])
d=gsub("/08","",d)
d=paste(year,d,sep="/")
d=strptime(d,format="%Y/%m/%d")
d=d-strptime("2008/11/4",format="%Y/%m/%d")
tab$day=d

##create week
week=round(d/7)
tab$week=week

##and finally, create the outomce
tab$diff=tab$Obama-tab$McCain
##keep only last 40 weeks
tab=tab[tab$week > -40,]


###################################################
### chunk number 69: weekeffect
###################################################
y=tapply(tab$diff,tab$week,mean)
x=as.numeric(names(y))
plot(x,y,xlab="week",ylab="difference",type="b")


###################################################
### chunk number 70: filter
###################################################
s=filter(y,filter=rep(1,3)/3)
plot(x,y)
lines(x,s,col=1)


###################################################
### chunk number 71: loess
###################################################
y=tab$diff
x=tab$week
plot(x,y,main="loess")
fit=loess(y~x,span=0.25,degree=1)
lines(unique(x),predict(fit,unique(x)),col=1)
fit=loess(y~x,span=0.5,degree=1)
lines(unique(x),predict(fit,unique(x)),col=2)
fit=loess(y~x,span=0.2,degree=2)
lines(unique(x),predict(fit,unique(x)),col=5)


###################################################
### chunk number 72: splines
###################################################
library(splines)
xx=c(0,unique(x))
plot(x,y,xlim=range(xx),main="splines")
fit=lm(y~ns(x,3))
lines(xx,predict(fit,list(x=xx)),col=1)
fit=lm(y~ns(x,5))
lines(xx,predict(fit,list(x=xx)),col=2)
fit=lm(y~ns(x,7))
lines(xx,predict(fit,list(x=xx)),col=3)


###################################################
### chunk number 73: filter
###################################################
###first take out pollsters with data points
tmp=table(tab$Pollster)
N=10
keepIndex= tab$Pollster%in%names(tmp)[tmp>=N]
tab=tab[keepIndex,]

##now fit a spline model
y=tab$diff
x=tab$week
pollster=factor(tab$Pollster)
fit=lm(y~ns(x,5)+pollster)
##4 because i know this is the first level
predict(fit,list(x=0,pollster=pollster[4]),se.fit=TRUE)


###################################################
### chunk number 74: yeast
###################################################
dat <- read.csv(file.path(datadir,"microarray.csv"))
y=log2(dat[,3]-64)
plot(density(y))
hist(y,nc=35)


###################################################
### chunk number 75: EM
###################################################
##EM algorithm
z1=dnorm(y,6,0.5)
z2=dnorm(y,9,1)
prior=0.25
p1=prior*z1/(prior*z1+(1-prior)*z2)
p2=1-p1
plot(y,p1,col=1)
points(y,p2,col=2)
mu1=sum(p1*y)/sum(p1)
mu2=sum(p2*y)/sum(p2)
sd1=sqrt(sum(p1*(y-mu1)^2)/sum(p1))
sd2=sqrt(sum(p2*(y-mu2)^2)/sum(p2))


###################################################
### chunk number 76: emfit
###################################################
plot(density(y))
x=sort(y)
lines(x,prior*dnorm(x,mu1,sd1),col=1)
lines(x,(1-prior)*dnorm(x,mu2,sd2),col=2)


###################################################
### chunk number 77: contour
###################################################
###this is just to make an example. Also shows a nice trick
library(mvtnorm)
nr=25;nc=25
h=seq(2,11,len=nr)
w=seq(1,7,len=nc)
x=expand.grid(l,w)
z=dmvnorm(x,c(7,4),cbind(c(1.5,1.5*0.5),c(1.5*0.5, 1)))
mat=matrix(z,nrow=nr,ncol=nc)
contour(h,w,mat,xlab="height",ylab="width")


###################################################
### chunk number 78: illustrationm
###################################################
##learn to use arrows and expression
plot(c(0,0,1,1),c(0,1,0,1),xlab="h",ylab="w",xlim=c(-0.5,1.5),ylim=c(-0.5,1.5))
points(1/2,1/2,pch="x")
text(0,0.5,expression(paste("(",h[0],",",w[0],")",sep="")))
arrows(0.15,0.5,0.45,0.5)


###################################################
### chunk number 79: illustration2
###################################################
tmp=expand.grid(seq(0,1,0.5),seq(0,1,0.5))
plot(tmp[,1],tmp[,2],xlab="h",ylab="w",xlim=c(-0.5,1.5),ylim=c(-0.5,1.5))
points(1/2,1/2,pch="x",cex=2)
text(-0.25,1/3,expression(paste("(",h[0],",",w[0],")",sep="")))
arrows(-0.1,1/3,0.45,0.5)


