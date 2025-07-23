library(stats)
library(mclust)

# range of cluster counts
candidate_k <- seq(1,10)
# number of folds
F <- 10

# a function to find knee in a curve
findKnee <- function(val,percent=10) {
  val <- which(abs(val/val[length(val)]-1) <= percent/100)
  return(min(val))
}

# load data
measPt <- read.table("measurementPoint.csv")
cDataWAns <- read.table("trueClass.csv") 
D <- dim(measPt)[2]
# create folds
foldIndex <- sample(seq(1,F), nrow(measPt), replace=T)
loglikeEM <- matrix(0,nrow=length(candidate_k)*F, ncol=3)
colnames(loglikeEM) <- c("k","holdoutLogLike","fold")

idx <- 0
for (i in seq(1,F)) {
  measPtTrain <- measPt[foldIndex != i,]
  measPtTest <- measPt[foldIndex == i,]
  # measPtTest <- measPtTrain
  
  for (Kest in candidate_k) {
    idx <- idx + 1
    # kmeans will initialize the assignments
    mykmeans <- kmeans(measPtTrain, centers=Kest, iter.max=10, nstart=1)
    # run Mstep first to get cluster parameter estimates
    msEst <- mstep(modelName="VVV", data=measPtTrain, z=unmap(mykmeans$cluster))
    # run EM until convergence for training data
    myEMtrain <- em(modelName=msEst$modelName, data=measPt, parameters=msEst$parameters)
    # run Estep on test data
    testEM <- estep(data=measPtTest, modelName = myEMtrain$modelName, parameters=myEMtrain$parameters)
    # get test log likelihood 
    loglikeEM[idx,] <- c(Kest,testEM$loglik,i)
  }
}
loglikeEM <- as.data.frame(loglikeEM)
loglikeEM$fold<- as.factor(loglikeEM$fold)

# average loglike over fold
foldAvgloglikeEM <- matrix(0,nrow=length(candidate_k),ncol=1)
for (myidx in seq(1,length(candidate_k))) {
  foldAvgloglikeEM[myidx] <- mean(loglikeEM$holdoutLogLike[loglikeEM$k==myidx])
}
# select EM model based on knee in log likelihood curve
bestK <- findKnee(foldAvgloglikeEM,15)
# use all data to select model parameters
# initialize with kmeans
mykmeans <- kmeans(measPt, centers=bestK, iter.max=10, nstart=1)
# run Mstep first to get cluster parameter estimates
msEst <- mstep(modelName="VVV", data=measPt, z=unmap(mykmeans$cluster))
# run EM until convergence for training data
myEMbest <- em(modelName=msEst$modelName, data=measPt, parameters=msEst$parameters)

plt <- ggplot(loglikeEM, aes(x=k, y= holdoutLogLike )) + 
  geom_point(aes(colour=fold)) + geom_smooth( se=FALSE) +
  geom_vline(xintercept = bestK)
plot(plt)

# draw ellipses
# points on circle with radius 2
rad <- 2*pi*seq(1,360)/360 
myx <- cos(rad)
myy <- sin(rad)
circle <- 3*as.matrix(data.frame(x=myx, y=myy))
elps <- matrix(0, nrow=0,ncol=3)
idx <- 0

mycenters <- t(as.matrix(myEMbest$parameters$mean))
for (k in seq(1,bestK)) {
elps1 <-  circle %*% myEMbest$parameters$variance$cholsigma[,,k]
elps1 <-  rep(mycenters[k,], each=nrow(elps1)) + elps1 
elps1 <- cbind(elps1,rep(k,each=nrow(elps1)))
elps <- rbind(elps,elps1)
}
colnames(elps) <- c("M1","M2","ellipse")
elps <- as.data.frame(elps)
elps$ellipse <- as.factor(elps$ellipse)
cDataWAns$trueCluster <- as.factor(cDataWAns$trueCluster)

# scatter plot
if (D==2) {
  plt <- ggplot() + 
    geom_point(data=elps, aes(x=M1, y=M2)) +
    geom_point(data=cDataWAns,aes(x=M1, y=M2, colour=trueCluster )) +
    xlab("M1") + ylab("M2")
  plot(plt)
  
}
