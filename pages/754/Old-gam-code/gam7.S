module(wavelets)
postscript("plot27.ps")
plot(nmr1,type="l",ylab="NMR Spectrum")
dev.off()


y <- diag(128)
W <- apply(y,1,function(x) dwt(x,wavelet="haar"))
postscript("plot28.ps")
auxplot()
dev.off()
W <- apply(y,1,function(x) dwt(x,wavelet="d4"))
postscript("plot29.ps")
auxplot()
dev.off()
 
aux <- dwt(nmr1,wavelet="s8")
postscript("plot30.ps")
plot(aux)
dev.off()

postscript("plot31.ps")
par(mfrow=c(2,2))
plot(nmr1,type="l",main="Original Data")
wavesmooth <- waveshrink(nmr1,wavelet="s8")
plot(wavesmooth,type="l",ylim=range(nmr1),main="waveshirnk")
smoothspline <- smooth.spline(1:length(nmr1),nmr1)
plot(smoothspline$y,type="l",ylim=range(nmr1),main=paste("smoothing splines: df=",round(smoothspline$df)))
Loess <- loess(nmr1~c(1:length(nmr1)),span=1/20)
plot(Loess$fitted,type="l",ylim=range(nmr1),main="loess with span 5%")
dev.off()

auxplot<- function(){
frame()
par(mfrow=c(4,4),mai=c(0.3,0.2,0.05,0.1))
par(mfg=c(1,1,4,1))
plot(W[1,],type="l")
par(mfg=c(2,1,4,1))
plot(W[2,],type="l")
par(mfg=c(3,1,4,2))
plot(W[3,],type="l")
par(mfg=c(3,2,4,2))
plot(W[4,],type="l")
par(mfg=c(4,1,4,4))
plot(W[5,],type="l")
par(mfg=c(4,2,4,4))
plot(W[6,],type="l")
par(mfg=c(4,3,4,4))
plot(W[7,],type="l")
par(mfg=c(4,4,4,4))
plot(W[8,],type="l")
}




y <- matrix(scan("mouse12.dat"),ncol=12)
y <- apply(y,1,mean)
aux <- waveshrink(y)
plot(aux,type="l")

