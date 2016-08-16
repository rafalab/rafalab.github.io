###SECTION 2
##figure 2: HOOKES law
LENGTH <- scan("Data/LDA/hooke.data")
weight <- 0:10
lm1 <- lm(LENGTH~weight)
postscript("plot03.ps")
par(mfrow=c(1,1))
plot(weight,LENGTH,main="Figure 3: Hooke's measurments")
abline(lm1)
text(1.2,439.4,paste("esimtes:",signif(lm1$coef[1],6),signif(lm1$coef[2],3)))
text(1.2,439.35,paste("se:",0.0079,0.0013))
dev.off()


