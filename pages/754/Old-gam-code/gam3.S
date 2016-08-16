###SECTION 3
aux <- read.table("Data/Sr.dat")
x <- aux[,1]; y <- aux[,2]

postscript("plot04.ps",horizontal=F)
plot(x,y,xlab="Time in millions of years", ylab="Strontium ratios")
abline(v=66.4)
dev.off()

pen <- c(.1,.9075,1,5)
for(i in 1:4){
  postscript(paste("plot05.",i,".ps",sep=""),horizontal=T)
  plot(x,y,xlab="Time in millions of years", ylab="Strontium ratios",main=paste("Penalty=",pen[i],sep=""))
  smooth <- smooth.spline(x,y,penalty=pen[i])
  lines(smooth$x,smooth$y,lwd=1.5)
  dev.off()
}

pen <- c(.9075,.1,1,5)
postscript("plot05.ps",horizontal=F)
plot(x,y,xlab="Time in millions of years", ylab="Strontium ratios")
sapply(1:length(pen),function(i){
  smooth <- smooth.spline(x,y,penalty=pen[i])
  lines(smooth$x,smooth$y,lty=i,lwd=1.5)
})
legend(45,-0.1,legend=paste("penalty=",as.character(pen),sep=""),lty=c(1:4),cex=1)
dev.off()


