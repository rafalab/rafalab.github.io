library(ellipse)
library(RColorBrewer)
mycols <- brewer.pal(8,"Accent")[c(6,1,5)]
library(MASS)


GS <- 300 ##grid size
N <- 300

##SIMULATE DATA
set.seed(3)
sigma <- 0.10
rho <- -0.15
Index <- sample(c(1,2,3),N,replace=TRUE)
mx <- c(-1,0,1)[Index]
X <- t(sapply(mx,function(mu)
              mvrnorm(1,c(mu,mu),Sigma=sigma*matrix(c(1,rho,rho,1),2,2))))
X1 <- X[,1]
X2 <- X[,2]
Y <- t(sapply(Index,function(x) as.numeric((1:3)%in%x)))

############PREP FOR PREDICTION########################
XLIM <- range(X1)
tmpx <- seq(XLIM[1],XLIM[2],len=GS)
YLIM <- range(X2)
tmpy <- seq(YLIM[1],YLIM[2],len=GS)
newx <- as.matrix(expand.grid(tmpx,tmpy)) #grid used to show prediction regions


B <- apply(Y,2,function(y) lm(y~X1+X2)$coef)
Yhat <- cbind(1,X1,X2)%*%B
Ghat <- apply(Yhat,1,which.max)
GridY <-cbind(1,newx)%*%B
GridG <- apply(GridY,1,which.max)
###FIGURE 4.2A
bitmap(file="Plots/plot-05-02-01.png", type="png256",
         width=4.25, height=4.5, res=300, pointsize=8) 
plot(X1,X2,col=mycols[Index],pch=as.character(Index))
contour(tmpx,tmpy,matrix(GridG,GS,GS),levels=c(1,2,3),add=TRUE,drawlabels=FALSE)
dev.off()

##FIGURE 4.3A
bitmap(file="Plots/plot-05-03-01.png", type="png256",
         width=4.25, height=4.5, res=300, pointsize=8) 
U1 <- X%*%c(1/sqrt(2),1/sqrt(2))
matplot(U1,Yhat,col=mycols)
for(i in 1:3)
  rug(U1[Index==i],col=mycols[i])
dev.off()

###FIGURE 4.2B
X3 <- X1^2
X4 <- X2^2
X5 <- X1*X2
X <- cbind(X1,X2,X3,X4,X5)
B <- apply(Y,2,function(y) lm(y~X1+X2+X3+X4+X5)$coef)
Yhat <- cbind(1,X)%*%B
Ghat <- apply(Yhat,1,which.max)
GridY <-cbind(1,newx,newx^2,newx[,1]*newx[,2])%*%B
GridG <- apply(GridY,1,which.max)
bitmap(file="Plots/plot-05-02-02.png", type="png256",
         width=4.25, height=4.5, res=300, pointsize=8) 
plot(X1,X2,col=mycols[Index],pch=as.character(Index))
contour(tmpx,tmpy,matrix(GridG,GS,GS),levels=c(1,2,3),add=TRUE,,drawlabels=FALSE)
dev.off()

##FIGURE 4.3B
bitmap(file="Plots/plot-05-03-02.png", type="png256",
         width=4.25, height=4.5, res=300, pointsize=8) 
matplot(U1,Yhat,col=mycols)
for(i in 1:3)
  rug(U1[Index==i],col=mycols[i])
dev.off()

###FIGURE 4.4
Train <- read.table("http://www-stat.stanford.edu/~tibs/ElemStatLearn/datasets/vowel.train",row.names=1,header=TRUE,sep=",")

Test <- read.table("http://www-stat.stanford.edu/~tibs/ElemStatLearn/datasets/vowel.test",row.names=1,header=TRUE,sep=",")

mycols2 <- brewer.pal(11,"Paired")

bitmap(file="Plots/plot-05-04.png", type="png256",
         width=4.25, height=4.5, res=300, pointsize=8) 
plot(Train[,2:3],type="n",xlab="First Coordinate",ylab="Second Coordinate")
text(Train[,2:3],as.character(Train[,1]),col=mycols2[Train[,1]],cex=1.2)
means <- apply(Train[,-1],2,function(x) tapply(x,Train[,1],mean,simplify=TRUE))
points(means[,1:2],col=mycols2,pch=16,cex=2)
dev.off()

###LDA
sigma <- .15
rho <- 0.5
Sigma <- matrix(c(1,rho,rho,1),2,2)*sigma
mu <- matrix(c(-1/sqrt(2),0,1/sqrt(2),0,1,0),3,2)

XLIM <- c(-2,2)
YLIM <- c(-2,2)

tmpx <- seq(XLIM[1],XLIM[2],len=GS)
tmpy <- seq(YLIM[1],YLIM[2],len=GS)
newx <- as.matrix(expand.grid(tmpx,tmpy)) #grid used to show prediction regions

iSigma <- solve(Sigma)
dkx <- newx%*%iSigma%*%t(mu)
tmp <- diag(0.5*mu%*%iSigma%*%t(mu)) + log(1/3)
Yhat <- sweep(dkx,2,tmp)
Ghat <- apply(Yhat,1,which.max)

bitmap(file="Plots/plot-05-05-01.png", type="png256",
         width=4.25, height=4.5, res=300, pointsize=8) 
plot(0,0,type="n",ylim=YLIM,xlim=XLIM,xlab="",ylab="")
for(i in 1:3){
  lines(ellipse(Sigma,centre=mu[i,]),col=mycols[i],lwd=2)
}
points(mu,pch=3)
contour(tmpx,tmpy,matrix(Ghat,GS,GS),levels=c(1,2,3),add=TRUE,drawlabels=FALSE)
dev.off()

##
K <- 3
X <- t(apply(mu[Index,],1,function(mu)
              mvrnorm(1,c(mu[1],mu[2]),Sigma=Sigma)))

muhat <- t(sapply(1:3,function(i) colMeans(X[Index==i,])))
Sigmahat <- sapply(1:3,function(i){
  X <- sweep(X[Index==i,],2,muhat[i,])
  cov(X)
})
Sigmahat <- matrix(rowMeans(Sigmahat),2,2)


bitmap(file="Plots/plot-05-05-02.png", type="png256",
         width=4.25, height=4.5, res=300, pointsize=8) 
plot(X,col=mycols[Index],pch=as.character(Index),xlab="",ylab="")
contour(tmpx,tmpy,matrix(Ghat,GS,GS),levels=c(1,2,3),add=TRUE,drawlabels=FALSE,lty=2)

iSigma <- solve(Sigmahat)
dkx <- newx%*%iSigma%*%t(muhat)
tmp <- diag(0.5*muhat%*%iSigma%*%t(muhat)) + log(1/3)
Yhat <- sweep(dkx,2,tmp)
Ghat <- apply(Yhat,1,which.max)
contour(tmpx,tmpy,matrix(Ghat,GS,GS),levels=c(1,2,3),add=TRUE,drawlabels=FALSE)
dev.off()

#######################################################
###################### MIXTURE TO TRY QUadratic ON ####
#######################################################
s<- sqrt(1/500)
s1 <- sqrt(1/350)
set.seed(30)
N <- 333

makeX <- function(M,n=N,sigma=diag(2)*s){
  z <- sample(1:10,n,replace=TRUE) ##pick n at random from above 10
  m <- M[z,] ##these are the n vectors (2 components)
  return(t(apply(m,1,function(mu) mvrnorm(1,mu,sigma)))) ##the final values
}

M1 <- mvrnorm(10,c(-1/2,0),matrix(c(3*s1,0,0,s1),2,2)) ##generate 10 means
M1[1,] <- c(0,1.5) ##to make it harder
M1[2,] <- c(0,.5)
M1[3,] <- c(-.5,2)
x1 <- makeX(M1)##the final values for y=0 (green)

M2 <- mvrnorm(10,c(-1,1),diag(2)*s1) ##generate 10 means
x2 <- makeX(M2)##the final values for y=0 (green)

M3 <- mvrnorm(10,c(1,.5),matrix(c(s1/10,0,0,s1*4),2,2)) ##generate 10 means
M3[1,] <- c(0.5,,-0.25)
M3[2,] <- c(0.4,1.75)
x3 <- makeX(M3)##the final values for y=0 (green)

X <- rbind(x1,x2,x3)
Y <- rep(c(1,2,3),c(N,N,N))
Index <- Y
Y <- as.factor(Y)
X1 <- X[,1]
X2 <- X[,2]

XLIM <- range(X1)
tmpx <- seq(XLIM[1],XLIM[2],len=GS)
YLIM <- range(X2)
tmpy <- seq(YLIM[1],YLIM[2],len=GS)
newx <- as.matrix(expand.grid(tmpx,tmpy)) #grid used to show prediction regions


###QDA, using Ripley;s function
fit1 <- lda(Y~X1+X2)
Ghat <- as.numeric(predict(fit1,newdata=list(X1=newx[,1],X2=newx[,2]))$class)

bitmap(file="Plots/plot-05-06-03.png", type="png256",
         width=4.25, height=4.5, res=300, pointsize=8) 
plot(X1,X2,col=mycols[Index],pch=as.character(Index),cex=1.25)
contour(tmpx,tmpy,matrix(Ghat,GS,GS),levels=c(1,2),add=TRUE,drawlabels=FALSE)
dev.off()

fit1 <- qda(Y~X1+X2)
Ghat <- as.numeric(predict(fit1,newdata=list(X1=newx[,1],X2=newx[,2]))$class)

bitmap(file="Plots/plot-05-06-01.png", type="png256",
         width=4.25, height=4.5, res=300, pointsize=8) 
plot(X1,X2,col=mycols[Index],pch=as.character(Index),cex=1.25)
contour(tmpx,tmpy,matrix(Ghat,GS,GS),levels=c(1,2),add=TRUE,drawlabels=FALSE)
dev.off()

X3 <- X1^2
X4 <- X2^2
X5 <- X1*X2

fit2 <- lda(Y~X1+X2+X3+X4+X5)
Ghat <- as.numeric(predict(fit2,newdata=list(X1=newx[,1],X2=newx[,2],X3=newx[,1]^2,X4=newx[,2],X5=newx[,1]*newx[,2]))$class)

bitmap(file="Plots/plot-05-06-02.png", type="png256",
         width=4.25, height=4.5, res=300, pointsize=8) 
plot(X1,X2,col=mycols[Index],pch=as.character(Index),cex=1.25)
contour(tmpx,tmpy,matrix(Ghat,GS,GS),levels=c(1,2),add=TRUE,drawlabels=FALSE)
dev.off()

###NOW to make regression plots
Y <- t(sapply(Index,function(x) as.numeric((1:3)%in%x)))

###FIGURE 4.1A
B <- apply(Y,2,function(y) lm(y~X1+X2)$coef)
GridY <-cbind(1,newx)%*%B
GridG <- apply(GridY,1,which.max)
bitmap(file="Plots/plot-05-01-01.png", type="png256",
         width=4.25, height=4.5, res=300, pointsize=8) 
plot(X1,X2,col=mycols[Index],pch=as.character(Index))
contour(tmpx,tmpy,matrix(GridG,GS,GS),levels=c(1,2,3),add=TRUE,drawlabels=FALSE)
dev.off()

B <- apply(Y,2,function(y) lm(y~X1+X2+X3+X4+X5)$coef)
GridY <-cbind(1,newx,newx^2,newx[,1]*newx[,2])%*%B
GridG <- apply(GridY,1,which.max)
bitmap(file="Plots/plot-05-01-02.png", type="png256",
         width=4.25, height=4.5, res=300, pointsize=8) 
plot(X1,X2,col=mycols[Index],pch=as.character(Index))
contour(tmpx,tmpy,matrix(GridG,GS,GS),levels=c(1,2,3),add=TRUE,drawlabels=FALSE)
dev.off()

setwd("Plots")
source("make-html.R")
setwd("..")












