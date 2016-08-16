# R command
library(tree)

# data frame
bwt.dat <- read.table("lbw.dat",header=T)
bwt.dat$smoke <- factor(bwt.dat$smoke,levels=0:1,labels=c("no","yes"))
bwt.dat$race <- factor(bwt.dat$race,levels=1:3,
                       labels=c("white","black","other"))
bwt.dat$ht <- factor(bwt.dat$ht,levels=0:1,labels=c("absent","present"))
bwt.dat$ui <- factor(bwt.dat$ui,levels=0:1,labels=c("absent","present"))

# grow the tree
bwt.tree <- tree(bwt~age+lwt+race+smoke+ptl+ht+ui+ftv,data=bwt.dat)
# or 
bwt.tree <- tree(bwt~.-low,data=bwt.dat)

# grow a tree using only race and smoke as predictors
bwt.tree2 <- tree(bwt~race+smoke,data=bwt.dat)

# tree info
summary(bwt.tree)
print(bwt.tree)

# plot the tree
plot(bwt.tree)
text(bwt.tree)

# Splus command
post.tree(bwt.tree)

# two choices how to plot the tree
plot(bwt.tree)
plot(bwt.tree,type="u")

# model selection 
bwt.cv <- cv.tree(bwt.tree)
# R uses pruning as default
# Splus uses shrinking as default
# pruning in Splus
bwt.cv <- cv.tree(bwt.tree,,prune.tree)
# or
bwt.cv <- cv.tree(bwt.tree,FUN=prune.tree)

# size versus deviance
plot(prune.tree(bwt.tree))

# cross-validated scores 
# (differences for pruning and shrinking)
plot(bwt.cv)

# pick the best tree from the above plot
bwt.tree.b <- prune.tree(bwt.tree,best=7)
plot(bwt.tree.b)
text(bwt.tree.b)



