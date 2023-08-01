## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE
)
knitr::opts_chunk$set(echo = TRUE, message=FALSE, warning=FALSE)

## ----eval=FALSE---------------------------------------------------------------
#  remotes::install_github("anna-neufeld/datathin")

## -----------------------------------------------------------------------------
library(datathin)
library(mvtnorm)
library(extraDistr)
library(ggplot2)
library(patchwork)

## -----------------------------------------------------------------------------
set.seed(1)
dat <- rpois(10000, 7)

## -----------------------------------------------------------------------------
dat.thin <- datathin(dat, family="poisson", epsilon=c(0.3, 0.7))
dat.train <- dat.thin[,,1]
dat.test <- dat.thin[,,2]

## ---- hold=TRUE---------------------------------------------------------------
mean(dat)
mean(dat.train)
0.3*7
mean(dat.test)
0.7*7

## -----------------------------------------------------------------------------
all.equal(dat, as.numeric(dat.train+dat.test))
cor(dat.train, dat.test)

## -----------------------------------------------------------------------------
set.seed(2)
dat <- rexp(100000, rate=1/5)
mean(dat)

## -----------------------------------------------------------------------------
folds <- datathin(dat, family="exponential", K=5)

## -----------------------------------------------------------------------------
dim(folds)[3]
length(folds[,,3])

## -----------------------------------------------------------------------------
for (m in 1:5) {
  print("-------")
  print(paste("fold", m))
  
  dat.test <- folds[,,m]
  dat.train <- dat - folds[,,m]
  
  print(paste("Test set mean", round(mean(dat.test), 3)))
  print(paste("Training set mean:", round(mean(dat.train), 3)))
  print(paste("Sample correlation:", round(as.numeric(cor(dat.test, dat.train)),4)))
}

## -----------------------------------------------------------------------------
dat <- rgamma(10000, shape=12, rate=2)
mean(dat)
res <- datathin(dat, family="gamma", arg=12)
dat.train <- res[,,1]
dat.test <- res[,,2]
mean(dat.train)
mean(dat.test)
as.numeric(cor(dat.train, dat.test))

## -----------------------------------------------------------------------------
set.seed(3)
dat <- matrix(rnorm(10000*10, mean=5, sd=sqrt(2)), nrow=10000)

## -----------------------------------------------------------------------------
res <- datathin(dat, family="normal", arg=2)
dat.train <- res[,,1]
dat.test <- res[,,2]

cors <- sapply(1:ncol(dat.train), function(u) round(cor(dat.train[,u], dat.test[,u]), 4))
print(paste("Correlation between train and test in column", 1:10, ":", cors))

## -----------------------------------------------------------------------------
dat <- cbind(rnorm(100000, mean=5, sd=sqrt(0.1)),
             rnorm(100000, mean=5, sd=sqrt(2)),
             rnorm(100000, mean=5, sd=sqrt(20)))

## -----------------------------------------------------------------------------
res <- datathin(dat, family="normal", arg=2)
dat.train <- res[,,1]
dat.test <- res[,,2]
cors <- sapply(1:ncol(dat.train), function(u) round(cor(dat.train[,u], dat.test[,u]), 4))
print(paste("Correlation between train and test in column", 1:3, ":", cors))

## -----------------------------------------------------------------------------
good_args <- cbind(rep(0.1, 100000), rep(2, 100000), rep(20,100000))
res <- datathin(dat, family="normal", arg=good_args)
dat.train <- res[,,1]
dat.test <- res[,,2]
cors <- sapply(1:ncol(dat.train), function(u) round(as.numeric(cor(dat.train[,u], dat.test[,u])),4))
print(paste("Correlation between train and test in column", 1:3, ":", cors))

## ---- eval=F------------------------------------------------------------------
#  res <- datathin(dat, family="normal", arg=c(0.1,2,20))

## -----------------------------------------------------------------------------
dat <- matrix(rnorm(10000*10, mean=5, sd=sqrt(2)), nrow=10000)
var(as.vector(dat))
res <- datathin(dat, family="normal-variance", arg=5)
dat.train <- res[,,1]
dat.test <- res[,,2]
mean(dat.train)
mean(dat.test)
as.numeric(cor(as.vector(dat.train), as.vector(dat.test)))

## -----------------------------------------------------------------------------
dat <- rnbinom(100000, size=7, mu = 6)
res <- datathin(dat, family="negative binomial", epsilon = c(0.2,0.8), arg=7)
dat.train <- res[,,1]
dat.test <- res[,,2]
0.2*6
mean(dat.train)
0.8*6
mean(dat.test)
as.numeric(cor(dat.train, dat.test))

## -----------------------------------------------------------------------------
dat <- rbinom(10000, 16, 0.25)
mean(dat)
res <- datathin(dat, family="binomial", arg=16)
dat.train <- res[,,1]
dat.test <- res[,,2]
mean(dat.train)
mean(dat.test)
as.numeric(cor(dat.train, dat.test))

## -----------------------------------------------------------------------------
dat <- 3*rchisq(10000, df=5)
mean(dat)/5
res <- datathin(dat, family="chi-squared", K=5)
dat1 <- res[,,1]
dat2 <- res[,,2]
var(dat1)
var(dat2)
as.numeric(cor(dat1, dat2))

## -----------------------------------------------------------------------------
dat <- rgamma(10000, shape=4, rate=4)
res <- datathin(dat, family="gamma-weibull", K=4, arg=3)
dat1 <- res[,,1]
dat2 <- res[,,2]
print(paste("Expected mean:", (4^(-1/3))*gamma(1+1/3)))
mean(dat1)
mean(dat2)
as.numeric(cor(dat1, dat2))

## -----------------------------------------------------------------------------
dat <- rweibull(100000, 5, 2)
res <- datathin(dat, family="weibull", K=2, arg=5)
dat1 <- res[,,1]
dat2 <- res[,,2]
print(paste("Expected mean:", (1/2)*(2^(5))))
mean(dat1)
mean(dat2)
as.numeric(cor(dat1, dat2))

## -----------------------------------------------------------------------------
dat <- rpareto(100000, 6, 2)
res <- datathin(dat, family="pareto", K=2, arg=2)
dat1 <- res[,,1]
dat2 <- res[,,2]
print(paste("Expected mean:", (1/2)/6))
mean(dat1)
mean(dat2)
as.numeric(cor(dat1, dat2))

## -----------------------------------------------------------------------------
rho <- 0.6
Sig <- matrix(0, nrow=4, ncol=4)
for (i in 1:4) {
  for (j in 1:4) {
    Sig[i,j] <- rho^abs(i-j)
  }
}
dat <- rmvnorm(100000, c(1,2,3,4), Sig)
colMeans(dat)
res <- datathin(dat, family="mvnormal", arg=Sig)
dat.train <- res[,,1]
dat.test <- res[,,2]
colMeans(dat.train)
colMeans(dat.test)
max(abs(cor(dat.train, dat.test)))

## -----------------------------------------------------------------------------
dat <- t(rmultinom(100000, 20, c(0.1,0.2,0.3,0.4)))
colMeans(dat)
res <- datathin(dat, family="multinomial", arg=20)
dat.train <- res[,,1]
dat.test <- res[,,2]
colMeans(dat.train)
colMeans(dat.test)
max(abs(cor(dat.train, dat.test)))

## -----------------------------------------------------------------------------
dat <- 10*rbeta(100000, 7, 1)
res <- datathin(dat, family="scaled-beta", K=2, arg=7)
dat1 <- res[,,1]
dat2 <- res[,,2]
print(paste("Expected mean:", 10*(7/2)/(7/2 + 1)))
mean(dat1)
mean(dat2)
as.numeric(cor(dat1, dat2))

## -----------------------------------------------------------------------------
dat <- 6 + rexp(100000, 2)
res <- datathin(dat, family="shifted-exponential", K=2, arg=2)
dat1 <- res[,,1]
dat2 <- res[,,2]
print(paste("Expected mean:", 6 + 1))
mean(dat1)
mean(dat2)
as.numeric(cor(dat1, dat2))

