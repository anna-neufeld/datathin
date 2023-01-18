## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE
)
knitr::opts_chunk$set(echo = TRUE, message=FALSE, warning=FALSE)

## -----------------------------------------------------------------------------
library(datathin)
library(ggplot2)
library(patchwork)

## -----------------------------------------------------------------------------
set.seed(1)
n <- 150
p <- 2
X<- matrix(rnorm(n*p, mean=0, sd=1), ncol=p)

ggplot(data=NULL, aes(x=X[,1], y=X[,2]))+geom_point()+coord_fixed()+ggtitle("All data")

## -----------------------------------------------------------------------------
cluster.mse.naive <- function(dat, clusterlabs) {
  totSS <- 0
  for (lab in unique(clusterlabs)) {
    clustdat <- dat[clusterlabs==lab,, drop='F']
    meanvec <- colMeans(dat[clusterlabs==lab,, drop='F'])
    ss <- apply(clustdat, 1, function(u) sum((u-meanvec)^2))
    totSS <- totSS+sum(ss)
  }
  return(totSS/length(dat))
}

## -----------------------------------------------------------------------------
one.cluster <- as.factor(rep(1,n))
three.clusters <- as.factor(kmeans(X, centers=3)$cluster)
mse.1 <- cluster.mse.naive(X, one.cluster)
mse.3 <- cluster.mse.naive(X, three.clusters)

p1 <- ggplot(data=NULL, aes(x=X[,1], y=X[,2], col=one.cluster))+geom_point()+
    coord_fixed()+ggtitle("All data, 1 cluster", round(mse.1,3))
p3 <- ggplot(data=NULL, aes(x=X[,1], y=X[,2], col=three.clusters))+geom_point()+coord_fixed()+ggtitle("All data, 3 clusters", round(mse.3,3))
p1+p3

## -----------------------------------------------------------------------------
clusters.full <- sapply(1:10, function(u) kmeans(X, centers= u)$cluster)
results.naive <-  apply(clusters.full, 2, function(u) cluster.mse.naive(X, u))
ggplot(data = NULL)+
  geom_line(aes(x=1:10, y=results.naive, col="Naive method"), lwd=1.5)+
  theme(axis.text = element_text(size=16), axis.title = element_text(size=18))+
  scale_x_continuous(breaks=seq(0,10,by=2))+
  xlab("Number of Clusters") + ylab("Total within-cluster MSE")+labs(col="")

## -----------------------------------------------------------------------------
X.thin <- datathin(X, family="normal", epsilon=0.5, arg=1)
Xtrain <- X.thin$Xtr
Xtest <- X.thin$Xte
p1 <- ggplot(data=NULL, aes(x=X[,1], y=X[,2]))+geom_point()+
  xlim(c(-3,3))+ylim(c(-3,3))+
  coord_fixed()+ggtitle("All data")
p2 <- ggplot(data=NULL, aes(x=Xtrain[,1], y=Xtrain[,2]))+geom_point()+
  xlim(c(-3,3))+ylim(c(-3,3))+
  coord_fixed()+ggtitle("Training set")
p3 <- ggplot(data=NULL, aes(x=Xtest[,1], y=Xtest[,2]))+geom_point()+
  xlim(c(-3,3))+ylim(c(-3,3))+
  coord_fixed()+ggtitle("Test set")
p1+p2+p3

## -----------------------------------------------------------------------------
cluster.train <- as.factor(kmeans(Xtrain, centers=3)$cluster)
p2 <- ggplot(data=NULL, aes(x=Xtrain[,1], y=Xtrain[,2], col=cluster.train))+geom_point()+
  xlim(c(-3,3))+ylim(c(-3,3))+
  coord_fixed()+ggtitle("Training set")
p3 <- ggplot(data=NULL, aes(x=Xtest[,1], y=Xtest[,2], col=cluster.train))+geom_point()+
  xlim(c(-3,3))+ylim(c(-3,3))+
  coord_fixed()+ggtitle("Test set")
p2+p3+plot_layout(guides="collect")

## -----------------------------------------------------------------------------
cluster.mse.datathin <- function(dat.train, dat.test, clusterlabs) {
  totSS <- 0
  for (lab in unique(clusterlabs)) {
    clustdat.test <- dat.test[clusterlabs==lab,, drop='F']
    meanvec <- colMeans(dat.train[clusterlabs==lab,, drop='F'])
    ss <- apply(clustdat.test, 1, function(u) sum((u-meanvec)^2))
    totSS <- totSS+sum(ss)
  }
  return(totSS/length(dat.test))
}

## -----------------------------------------------------------------------------
clusters.full <- sapply(1:10, function(u) kmeans(X, centers= u)$cluster)
results.naive <-  apply(clusters.full, 2, function(u) cluster.mse.naive(X, u))

clusters.train <- sapply(1:10, function(u) kmeans(Xtrain, centers= u)$cluster)
results.datathin <- apply( clusters.train, 2, function(u) cluster.mse.datathin(Xtrain,Xtest, u))

eps = 0.5

ggplot(data = NULL)+
  geom_line(aes(x=1:10, y=results.naive, col="Naive method"), lwd=1.5)+
  geom_line(aes(x=1:10, y=results.datathin/eps, col="Data thinning"), lwd=1.5)+
  scale_x_continuous(breaks=seq(0,10,by=1))+
  xlab("Number of Clusters") + ylab("Total within-cluster MSE")+labs(col="")

## -----------------------------------------------------------------------------
p <- 2
n <- 150
trueClusters <- as.factor(rep(c(1,2,3), each=n/3))
X <- rbind(
  matrix(rnorm(n/3*p, -4,1), ncol=p),
  matrix(rnorm(n/3*p, 0,1), ncol=p),
  matrix(rnorm(n/3*p, 4,1), ncol=p)
)
ggplot(data=NULL, aes(x=X[,1], y=X[,2], col=trueClusters))+geom_point()

## -----------------------------------------------------------------------------
X.thin <- datathin(X, family="normal", epsilon=0.5, arg=1)
Xtrain <- X.thin$Xtr
Xtest <- X.thin$Xte
p1 <- ggplot(data=NULL, aes(x=X[,1], y=X[,2], col=trueClusters))+geom_point()+
  xlim(c(-6,6))+ylim(c(-6,6))+
  coord_fixed()+ggtitle("All data")
p2 <- ggplot(data=NULL, aes(x=Xtrain[,1], y=Xtrain[,2], col=trueClusters))+geom_point()+
  xlim(c(-6,6))+ylim(c(-6,6))+
  coord_fixed()+ggtitle("Training set")
p3 <- ggplot(data=NULL, aes(x=Xtest[,1], y=Xtest[,2], col=trueClusters))+geom_point()+
  xlim(c(-6,6))+ylim(c(-6,6))+
  coord_fixed()+ggtitle("Test set")
p1+p2+p3+plot_layout(guides="collect")

## -----------------------------------------------------------------------------
clusters.full <- sapply(1:10, function(u) kmeans(X, centers= u)$cluster)
results.naive <-  apply(clusters.full, 2, function(u) cluster.mse.naive(X, u))

clusters.train <- sapply(1:10, function(u) kmeans(Xtrain, centers= u)$cluster)
results.datathin <- apply( clusters.train, 2, function(u) cluster.mse.datathin(Xtrain,Xtest, u))

eps = 0.5

ggplot(data = NULL)+
  geom_line(aes(x=1:10, y=results.naive, col="Naive method"), lwd=1.5)+
  geom_line(aes(x=1:10, y=results.datathin/eps, col="Data thinning"), lwd=1.5)+
  scale_x_continuous(breaks=seq(0,10,by=1))+
  xlab("Number of Clusters") + ylab("Total within-cluster MSE")


## -----------------------------------------------------------------------------
cluster.mse.datathin <- function(dat.train, dat.test, clusterlabs, eps=0.5) {
  totSS <- 0
  for (lab in unique(clusterlabs)) {
    clustdat.test <- dat.test[clusterlabs==lab,, drop='F']
    meanvec <- (1-eps)/(eps)*colMeans(dat.train[clusterlabs==lab,, drop='F'])
    ss <- apply(clustdat.test, 1, function(u) sum((u-meanvec)^2))
    totSS <- totSS+sum(ss)
  }
  return(totSS/length(dat.test))
}

## -----------------------------------------------------------------------------
X.thin <- datathin(X, family="normal", epsilon=0.9, arg=1)
Xtrain <- X.thin$Xtr
Xtest <- X.thin$Xte
p1 <- ggplot(data=NULL, aes(x=X[,1], y=X[,2], col=trueClusters))+geom_point()+
  xlim(c(-6,6))+ylim(c(-6,6))+
  coord_fixed()+ggtitle("All data")
p2 <- ggplot(data=NULL, aes(x=Xtrain[,1], y=Xtrain[,2], col=trueClusters))+geom_point()+
  xlim(c(-6,6))+ylim(c(-6,6))+
  coord_fixed()+ggtitle("Training set")
p3 <- ggplot(data=NULL, aes(x=Xtest[,1], y=Xtest[,2], col=trueClusters))+geom_point()+
  xlim(c(-4,4))+ylim(c(-4,4))+
  coord_fixed()+ggtitle("Test set")
p1+p2+p3+plot_layout(guides="collect")

## -----------------------------------------------------------------------------
eps=0.9
clusters.train <- sapply(1:10, function(u) kmeans(Xtrain, centers= u)$cluster)
results.datathin <- apply( clusters.train, 2, function(u) cluster.mse.datathin(Xtrain,Xtest, u, eps))

ggplot(data = NULL)+
  geom_line(aes(x=1:10, y=results.datathin, col="Data thinning"), lwd=1.5)+
  scale_x_continuous(breaks=seq(0,10,by=1))+
  xlab("Number of Clusters") + ylab("Total within-cluster MSE")


## -----------------------------------------------------------------------------
nFolds <- 10
X.multithin <- multithin(X, family="normal", nfolds=nFolds, arg=1)
totalMSEs <- matrix(NA, nrow=nFolds, ncol=10)
for (fold in 1:nFolds) {
  Xtest <- X.multithin[[fold]]
  Xtrain <- X-Xtest
  clusters.train <- sapply(1:10, function(u) kmeans(Xtrain, centers= u)$cluster)
  totalMSEs[fold,] <- apply( clusters.train, 2, function(u) cluster.mse.datathin(Xtrain,Xtest, u, (nFolds-1)/nFolds))
}

averagedMSEs <- apply(totalMSEs, 2, mean)

ggplot(data = NULL)+
  geom_line(aes(x=1:10, y=results.datathin, col="Data thinning, eps=0.9"), lwd=1.5)+
  geom_line(aes(x=1:10, y=averagedMSEs, col="Multi thinning, 10 folds"), lwd=1.5)+
  scale_x_continuous(breaks=seq(0,10,by=1))+
  xlab("Number of Clusters") + ylab("Total within-cluster MSE")

