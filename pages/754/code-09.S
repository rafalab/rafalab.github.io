
gam1 <- gam(Kyphosis~s(Age),family="binomial",data=kyphosis)
cat("AIC1 =", gam1$deviance + 2*sum(gam1$nl.df),"\n")
cat("BIC1 =", gam1$deviance + log(83)*sum(gam1$nl.df),"\n")
gam1 <- gam(Kyphosis~s(Age)+s(Start),family="binomial",data=kyphosis)
cat("AIC2 =", gam1$deviance + 2*sum(gam1$nl.df),"\n")
cat("BIC2 =", gam1$deviance + log(83)*sum(gam1$nl.df),"\n")
gam1 <- gam(Kyphosis~s(Age)+s(Start)+s(Number),family="binomial",data=kyphosis)
cat("AIC3 =", gam1$deviance + 2*sum(gam1$nl.df),"\n")
cat("BIC3 =", gam1$deviance + log(83)*sum(gam1$nl.df),"\n")
gam1 <- gam(Kyphosis~s(Age)+s(Number),family="binomial",data=kyphosis)
cat("AIC4 =", gam1$deviance + 2*sum(gam1$nl.df),"\n")
cat("BIC4 =", gam1$deviance + log(83)*sum(gam1$nl.df),"\n")

