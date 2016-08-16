data.restore("Data/gam.datasets")
attach(kyphosis)
glm1 <- glm(Kyphosis~Age+Number+Start,family="binomial")
fit.terms <- predict(glm1,type="term") 
partial.resid <- fit.terms + resid(glm1,type="working")
postscript("Plots/plot-08-01.ps")
par(mfrow=c(1,3))
plot(Age/12,partial.resid[,"Age"],cex=1,pch=1)
o <- order(Age)
lines(Age[o]/12,fit.terms[o,"Age"])
Ageloess <- loess(partial.resid[,"Age"]~Age)
lines(Age[o]/12,Ageloess$fitted[o],lty=2)

plot(Number,partial.resid[,"Number"],cex=1,pch=1)
o <- order(Number)
lines(Number[o],fit.terms[o,"Number"])
Numberloess <- loess(partial.resid[,"Number"]~Number)
lines(Number[o],Numberloess$fitted[o],lty=2)
 
plot(Start,partial.resid[,"Start"],cex=1,pch=1)
o <- order(Start)
lines(Start[o],fit.terms[o,"Start"])
Startloess <- loess(partial.resid[,"Start"]~Start)
lines(Start[o],Startloess$fitted[o],lty=2)
dev.off()

postscript("Plots/plot-08-02.ps")
par(mfcol=c(2,2))
aux1 <- loess(Kyphosis~Start+Age,span=(2/3)^2,degree=1)
x <- seq(min(Start),max(Start),len=40)
y <- seq(min(Age),max(Age),len=40)
xy <- expand.grid(Start=x,Age=y)
z <- predict(aux1,xy) 
persp(x,y,z,xlab="Start",ylab="Age")

aux <- expand.grid(Age = seq(min(Age),max(Age),le=40),
                   Start = seq(min(Start),max(Start),len=40))
paux <- predict(aux1,aux)


plot(Age,Start,type="n")
points(Age[Kyphosis==0],Start[Kyphosis==0],pch=1,cex=.9)
points(Age[Kyphosis==1],Start[Kyphosis==1],pch=16)

contour(seq(min(Age),max(Age),le=40), seq(min(Start),max(Start),len=40),surf,add=T,v=c(0.1,0.3,0.5))

aux <- loess(Kyphosis~Start+Number,span=(2/3)^2,degree=1)
x <- seq(min(Start),max(Start),len=40)
y <- seq(min(Number),max(Number),len=40)
xy <- expand.grid(Start=x,Number=y)
z <- predict(aux,xy) 
persp(x,y,z,xlab="Start",ylab="Number")

aux <- loess(Kyphosis~Age+Number,span=(2/3)^2,degree=1)
x <- seq(min(Age),max(Age),len=40)
y <- seq(min(Number),max(Number),len=40)
xy <- expand.grid(Age=x,Number=y)
z <- predict(aux,xy) 
persp(x,y,z,xlab="Age",ylab="Number")
dev.off()

postscript("Plots/plot-08-03.ps")
gam1 <- gam(Kyphosis~s(Age)+s(Start)+s(Number),family="binomial")
par(mfrow=c(1,3))
plot.gam(gam1)
dev.off()
 


glm2 <- glm(Kyphosis~poly(Age,2) + I((Start > 12) * (Start - 12)),
	    family=binomial)
postscript("Plots/plot-08-04.ps")
par(mfrow=c(1,2))
plot.gam(glm2,se=T,residuals=T)
dev.off()



postscript("Plots/plot-08-05.ps")
pAge <- seq(0,12*30,len=100)
pStart <- seq(min(Start),max(Start),len=100)
aux1 <-predict.gam(glm2,data.frame(Age=pAge,Start=pStart),type="terms")
aux2 <- predict(glm2,data.frame(Age=pAge,Start=pStart),type="terms")
par(mfrow=c(1,2))
plot(pAge/12,binomial()$inverse(aux2[,1]),main="predict")
plot(pAge/12,binomial()$inverse(aux1[,1]),main="predict.gam")
dev.off()


gam1 <- gam(Kyphosis~s(Age)+s(Start),family="binomial")
aux <- data.frame(Age = seq(min(Age),max(Age),le=40),
			   Start = seq(min(Start),max(Start),len=40))
paux <- predict.gam(gam1,aux,type="terms")
surf <- outer(paux[,1],paux[,2],"+")
surf <- surf + attr(paux,"constant")
surf <- binomial()$inverse(surf)

postscript("Plots/plot-08-06.ps")
par(mfrow=c(1,1))
plot(Age,Start,type="n")
points(Age[Kyphosis==0],Start[Kyphosis==0],pch=1,cex=.9)
points(Age[Kyphosis==1],Start[Kyphosis==1],pch=16)
contour(aux$Age,aux$Start,surf,add=T,v=c(0.1,0.3,0.5))
dev.off()
 
###########SIMULATION
eta1 <- function(u) (-1.25+6*u)*(u <= 0.25) + 0.25*(u>0.25)
eta2 <- function(v)  (1-0.8*v)*cos(2*pi*v)
eta <- function(x) -1 + eta1(x[1]) + eta2(x[2])
mu <- function(x) exp(eta(x))/(1+exp(eta(x)))


postscript("Plots/plot-08-07.ps")
par(mfrow=c(2,2))
N <- 250
u0 <- runif(N,0,1)
v0 <- runif(N,0,1)
y0 <- rbinom(N,1,apply(cbind(u0,v0),1,mu))
fit0 <-  gam(y0~s(u0,df=4)+s(v0,df=4),family="binomial") 
aux <- predict.gam(fit0,type="terms")
o <- order(u0)
plot(u0[o],eta1(u0)[o],type="l",ylim=range(c(eta1(u0),aux[,1])))
lines(u0[o],aux[o,1],lty=2)
o <- order(v0)
plot(v0[o],eta2(v0)[o],type="l",ylim=range(c(eta2(v0),aux[,2])))
lines(v0[o],aux[o,2],lty=2)

N <- 50
u <- runif(N,0,1)
v <- runif(N,0,1)
y <- rbinom(N,1,apply(cbind(u,v),1,mu))
fit1 <-  gam(y~s(u,df=4)+s(v,df=4),family="binomial") 
aux <- predict.gam(fit1,type="terms")
o <- order(u)
plot(u[o],eta1(u)[o],type="l",ylim=range(c(eta1(u),aux[,1])))
lines(u[o],aux[o,1],lty=2)
o <- order(v)
plot(v[o],eta2(v)[o],type="l",ylim=range(c(eta2(v),aux[,2])))
lines(v[o],aux[o,2],lty=2)
dev.off()

cgrad <- function(c1, c2, steps,with.end=F){
####This function creates colors in between the specified colors used in image
###plots. In the color scheme window of the motif 
###window the numbers like 10 and 5 represent the number of intermediate
###colors which are generated between the two specified colors.  Thus, the
###first ten colors will start with black and then gradually change to blue,
###then shift from blue to magenta, etc.  To get the same effect in postscript,
###you'd have to take the rgb values for the colors in question and
###generate intermediate values yourself.        
        from <- ps.colors.rgb[c1,  ]
        to <- ps.colors.rgb[c2,  ]
	if(with.end)
	  {
	     cbind(
              seq(from = from[1], to = to[1], len = steps),
              seq(from = from[2], to = to[2], len = steps),
              seq(from = from[3], to = to[3], len = steps))
	   }
        else{
	cbind(
	      seq(from = from[1], to = to[1], len = steps)[-steps],
	      seq(from = from[2], to = to[2], len = steps)[-steps],
	      seq(from = from[3], to = to[3], len = steps)[-steps])
      }
}
 
#Now we can generate my.image.colors using cgrad:
my.image.colors <- rbind(cgrad("black", "blue", 10), 
                          cgrad("blue", "magenta", 10), 
                          cgrad("magenta", "red", 10), 
                         cgrad("red", "yellow", 5), 
                          cgrad("yellow","white", 5),with.end=T)

NN <- 150
aux <- data.frame(u=seq(0,1,len=NN),v=seq(0,1,len=NN))
aux <- predict.gam(fit1,aux,type="terms")
surf <- outer(aux[,1],aux[,2],"+")
surf <- surf + attr(aux,"constant")
surf <- binomial()$inverse(surf)

postscript("Plots/plot-08-08.ps",horizontal=F)
ps.options.send(image.colors=my.image.colors)
#image(seq(0,1,len=NN),seq(0,1,len=NN),surf,zlim=c(0,1))
aux1 <- eta1(seq(0,1,len=NN))
aux2 <- eta2(seq(0,1,len=NN))
surf <- outer(aux1,aux2,"+") - 1
surf <- binomial()$inverse(surf)
par(omd=c(0,1, 0.25, 1),mfg=c(1,1,1,1))##plot espected intensities
image(seq(0,1,len=NN),seq(0,1,len=NN),surf,zlim=c(0,1))
points(u[y==0],v[y==0],pch="0",cex=.9,col=0)
points(u[y==1],v[y==1],pch="1",cex=.9,col=0)
vecZ<-seq(min(surf),max(surf),len=300)
par(omd=c(0, 1, 0, 0.25), mfg=c(1,1,1,1)) ###plot legend
image(vecZ,1,matrix(vecZ,300,1),yaxt="n",main="legend",xlab="",ylab="",zlim=range(surf))
dev.off()

postscript("Plots/plot-08-09.ps")
par(mfrow=c(2,2))
plot.gam(fit0,se=T,resid=T,rug=F,scale=8)
plot.gam(fit1,se=T,resid=T,rug=F,scale=8)
dev.off()



