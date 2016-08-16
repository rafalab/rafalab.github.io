library(MASS) #mvrnorm is here
library(RColorBrewer) ##nice colors
library(class) ##knn is here

mycols <- brewer.pal(8,"Accent")[c(5,3)]

##women and men heights
set.seed(1)
w <- rnorm(10000,64.5,3)
m <- rnorm(10000,70,3)
x <- c(w,m)

bitmap(file="Plots/plot-03-01-01.png", type="png256",
         width=4.25, height=4.5, res=300, pointsize=10) 
hist(x,xlab="Height in inches",nc=35,prob=TRUE)
dev.off()

bitmap(file="Plots/plot-03-01-02.png", type="png256",
         width=4.25, height=4.5, res=300, pointsize=10) 
hist(x,xlab="Height in inches",nc=35,prob=TRUE)
x <- seq(min(w),max(w),len=100)
lines(x,dnorm(x,64.5,3)/2,col=mycols[1],lwd=2)
x <- seq(min(m),max(m),len=100)
lines(x,dnorm(x,70,3)/2,col=mycols[2],lwd=2)
dev.off()

x <- c(w[1:10],m[1:10])
y <- c(rep(1,10),rep(0,10))

lm1 <- lm(y~x)
print(round(predict(lm1),2))

bitmap(file="Plots/plot-03-01-03.png", type="png256",
         width=6.25, height=2.5, res=300, pointsize=10) 
plot(x,rep(0,length(x)),col=mycols[y+1],yaxt="n",ylab="")
dev.off()

##first class

sink("results-03.txt")
s<- sqrt(1/5)
set.seed(30)
GS <- 75 ##grid size is GS x GS

makeX <- function(M,n=100,sigma=diag(2)*s){
  z <- sample(1:10,n,replace=TRUE) ##pick n at random from above 10
  m <- M[z,] ##these are the n vectors (2 components)
  return(t(apply(m,1,function(mu) mvrnorm(1,mu,sigma)))) ##the final values
}

M0 <- mvrnorm(10,c(1,0),diag(2)) ##generate 10 means
x0 <- makeX(M0)##the final values for y=0 (green)
testx0 <- makeX(M0)

M1 <- mvrnorm(10,c(0,1),diag(2)) ##generate 10 means
x1 <- makeX(M1)
testx1 <-makeX(M1)

x <- rbind(x0,x1) ## one matrix with everything
test <- rbind(testx0,testx1)

y <- c(rep(0,100),rep(1,100)) #the outcomes
cols <- mycols[c(rep(1,100),rep(2,100))]

##prepare data for colored grid:
XLIM <- c(min(c(x[,1],test[,1])),max(c(x[,1],test[,1])))
tmpx <- seq(XLIM[1],XLIM[2],len=GS)
YLIM <- c(min(c(x[,2],test[,2])),max(c(x[,2],test[,2])))
tmpy <- seq(YLIM[1],YLIM[2],len=GS)
newx <- expand.grid(tmpx,tmpy) #grid used to show color contour of predictions

### LINEAR MODEL
X1 <- x[,1] ##these are the covariates
X2 <- x[,2] 
fit1 <- lm(y~X1+X2)

##prediction on train
yhat <- predict(fit1);yhat <- as.numeric(yhat>0.5)
cat("Linear regression prediction error in train:",1-mean(yhat==y),"\n")

yhat <- predict(fit1,newdata=data.frame(X1=newx[,1],X2=newx[,2]))
m <- -fit1$coef[2]/fit1$coef[3]
b <- (0.5 - fit1$coef[1])/fit1$coef[3]

##colors for prediction
colshat <- yhat
colshat[yhat>=0.5] <- mycols[2]
colshat[yhat<0.5] <- mycols[1]

bitmap(file="Plots/plot-03-02.png", type="png256",
         width=4.25, height=4.5, res=300, pointsize=10) 
plot(x,col=cols,xlab="X1",ylab="X2",xlim=XLIM,ylim=YLIM)
abline(b,m)
points(newx,col=colshat,pch=".")
points(x,col=cols)
title("Train: Linear regression")
dev.off()

##prediction on test
yhat <- predict(fit1,newdata=data.frame(X1=test[,1],X2=test[,2]))
yhat <- as.numeric(yhat>0.5)
cat("Linear regression prediction error in test:",1-mean(yhat==y),"\n")

bitmap(file="Plots/plot-03-03.png", type="png256",
         width=4.25, height=4.5, res=300, pointsize=10) 
plot(test,col=cols,xlab="X1",ylab="X2",xlim=XLIM,ylim=YLIM)
abline(b,m)
points(newx,col=colshat,pch=".")
points(x,col=cols)
title("Test: Linear regression")
dev.off()

#### KNN with
for(k in c(1,15)){
  cat(k,"nearest neighbors\n")
  
  ##predict on train
  yhat <- knn(x,x,y,k=k)
  cat("KNN prediction error in train:",1-mean((as.numeric(yhat)-1)==y),"\n")
  
##make plot
  yhat <- knn(x,newx,y,k=k)
  colshat <- mycols[as.numeric(yhat)]
  
  bitmap(file=paste("Plots/plot-03-04-",k,".png",sep=""), type="png256",
  width=4.25, height=4.5, res=300, pointsize=10) 
  plot(x,col=cols,xlab="X1",ylab="X2",xlim=XLIM,ylim=YLIM)
  points(newx,col=colshat,pch=".")
  contour(tmpx,tmpy,matrix(as.numeric(yhat),GS,GS),levels=c(1,2),add=TRUE,drawlabels=FALSE)
  points(x,col=cols)
  title(paste("Train: KNN (",k,")",sep=""))
  dev.off()
  
  bitmap(file=paste("Plots/plot-03-05-",k,".png",sep=""), type="png256",
         width=4.25, height=4.5, res=300, pointsize=10) 
  plot(test,col=cols,xlab="X1",ylab="X2",xlim=XLIM,ylim=YLIM)
  points(newx,col=colshat,pch=".")
  contour(tmpx,tmpy,matrix(as.numeric(yhat),GS,GS),levels=c(1,2),add=TRUE,drawlabels=FALSE)
  points(x,col=cols)
  title(paste("Test: KNN (",k,")",sep=""))
  dev.off()
  
  yhat <- knn(x,test,y,k=k)
  cat("KNN prediction error in test:",1-mean((as.numeric(yhat)-1)==y),"\n")
}

###Bayes Rule
  p <- function(x){ ##probability of Y given X
  p0 <- mean(dnorm(x[1],M0[,1],s)*dnorm(x[2],M0[,2],s))
  p1 <- mean(dnorm(x[1],M1[,1],s)*dnorm(x[2],M1[,2],s))
  p1/(p0+p1)
}

##first on train
yhat <- apply(x,1,p)
cat("Bayes rule prediction error in train",1-mean(round(yhat)==y),"\n")

bayesrule <- apply(newx,1,p)
colshat <- bayesrule
colshat[bayesrule>=0.5] <- mycols[2]
colshat[bayesrule<0.5] <- mycols[1]

bitmap(file="Plots/plot-03-06.png", type="png256",
  width=4.25, height=4.5, res=300, pointsize=10) 
plot(x,col=cols,xlab="X1",ylab="X2",xlim=XLIM,ylim=YLIM)
points(newx,col=colshat,pch=".")
contour(tmpx,tmpy,matrix(round(bayesrule),GS,GS),levels=c(1,2),add=TRUE,drawlabels=FALSE)
points(x,col=cols)
title("Train: Bayes Rule")
dev.off()


bitmap(file="Plots/plot-03-07.png", type="png256",
  width=4.25, height=4.5, res=300, pointsize=10) 
plot(x,col=cols,xlab="X1",ylab="X2",xlim=XLIM,ylim=YLIM)
points(newx,col=colshat,pch=".")
contour(tmpx,tmpy,matrix(round(bayesrule),GS,GS),levels=c(1,2),add=TRUE,drawlabels=FALSE)
title("Train: Bayes Rule")
points(rbind(M0,M1),col=mycols[c(rep(1,10),rep(2,10))],pch=16,cex=1.25)
dev.off()

##now on test
yhat <- apply(test,1,p)
bayes.error <- 1-mean(round(yhat)==y)
cat("Bayes rule prediction error in test",bayes.error,"\n")


bayesrule <- apply(newx,1,p)
colshat <- bayesrule
colshat[bayesrule>=0.5] <- mycols[2]
colshat[bayesrule<0.5] <- mycols[1]

bitmap(file="Plots/plot-03-08.png", type="png256",
  width=4.25, height=4.5, res=300, pointsize=10) 
plot(test,col=cols,xlab="X1",ylab="X2",xlim=XLIM,ylim=YLIM)
points(newx,col=colshat,pch=".")
contour(tmpx,tmpy,matrix(round(bayesrule),GS,GS),levels=c(1,2),add=TRUE,drawlabels=FALSE)
points(x,col=cols)
title("Train: Bayes Rule")
dev.off()


bitmap(file="Plots/plot-03-09.png", type="png256",
  width=4.25, height=4.5, res=300, pointsize=10) 
plot(test,col=cols,xlab="X1",ylab="X2",xlim=XLIM,ylim=YLIM)
points(newx,col=colshat,pch=".")
contour(tmpx,tmpy,matrix(round(bayesrule),GS,GS),levels=c(1,2),add=TRUE,drawlabels=FALSE)
points(x,col=cols)
title("Train: Bayes Rule")
points(rbind(M0,M1),col=mycols[c(rep(1,10),rep(2,10))],pch=16,cex=1.25)
dev.off()



train.error <- rep(0,16)
test.error <- rep(0,16)
for(k in 1:16){
  cat(k,"nearest neighbors\n")
  
  ##predict on train
  yhat <- knn(x,x,y,k=2^(k/2))
  train.error[k] <- 1-mean((as.numeric(yhat)-1)==y)
  
  yhat <- knn(x,test,y,k=2^(k/2))
  test.error[k] <- 1-mean((as.numeric(yhat)-1)==y)
}

bitmap(file="Plots/plot-03-10.png", type="png256",
  width=4.25, height=4.5, res=300, pointsize=10) 
ks <- 2^((1:16/2))
plot(ks,train.error,type="n",xlab="K",ylab="Prediction Error",log="x")
lines(ks,train.error,type="b",col=2,lty=2)
lines(ks,test.error,type="b",col=3,lty=3)
abline(h=bayes.error)
legend(20,.4,c("Train","Test","Bayes"),col=c(2,3,1),lty=c(2,3,1))
dev.off()

sink()








