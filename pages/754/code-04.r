##CHAPTER 4
library(modreg)
aux <- read.table("Data/Sr.dat")
x <- aux[,1]; y <- aux[,2]

postscript("Plots/plot-04-01.ps",horizontal=T)
plot(x,y,xlab="Time in millions of years", ylab="Strontium ratios")
abline(v=66.4)
dev.off()

pen <- c(0.1,.5,1,1.5)
postscript("Plots/plot-04-02.ps",horizontal=F)
par(mfrow=c(2,2))
sapply(1:length(pen),function(i){
plot(x,y,xlab="Time in millions of years", ylab="Strontium ratios",main=paste("penalty=",as.character(pen[i])))
  smooth <- smooth.spline(x,y,spar=pen[i])
  lines(smooth$x,smooth$y,lty=i,lwd=1.5)
})
dev.off()
