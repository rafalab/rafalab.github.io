## copy from notes
par(mfrow=c(2,2))
p=.5
for(n in c(5,10,50,100)){
  ks=0:n
  plot(sqrt(n)*2*(ks/n-.5),dbinom(ks,n,p),type="h")
  normapprox=pnorm(sqrt(n)*2*((ks+0.5)/n-.5))-
    pnorm(sqrt(n)*2*((ks-0.5)/n-.5))
  lines(sqrt(n)*2*(ks/n-.5),normapprox,col=2)
}
legend("topleft",c("exact","approx"),col=c("black","red"),lty=1)


##### simulation of means




NREPS=100000
par(mfrow=c(2,2))
p=.05
for(n in c(5,10,50,100)){
  norms= sqrt(n)*(1/sqrt(p*(1-p)))*(rbinom(NREPS, n, p)/n - p)
  hist(norms, col="grey", freq=FALSE)
  curve(dnorm, add=TRUE, col="green")
}
legend("topleft",c("exact","approx"),col=c("black","green"),lty=1)















NREPS=10000
par(mfrow=c(2,2))
p=.01
n=c(5,10,50,100)
for(i in 1:4){
  norms=sqrt(n[i])*sqrt(1/(p*(1-p)))*(rbinom(NREPS, n[i], p)/n[i] - p)
  hist(norms, col="grey", freq=FALSE)
  curve(dnorm, add=TRUE, col="red")
  print(length(which(abs(norms)<1.96))/NREPS)
}
legend("topleft",c("exact","approx"),col=c("black","red"),lty=1)


## why would you want to know which p gives the larges variance





####

data=read.csv("~/Desktop/medicalcost.csv", header=FALSE)
colnames(data)="cost"
hist(data$cost)
qqnorm(data$cost)

# try some transformations to "make it normal"
trans1=log(data$cost)
hist(trans1)
qqnorm(trans1)

# why would you do this?





# explore the poisson distribution

hist(rpois(10000, 50), col="grey")

# how is it different from the binomial?






# does it matter if something is unbiased? is that the only thing that matters? would you trade bias for precision? MSE ...












