## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE
)
knitr::opts_chunk$set(echo = TRUE, message=FALSE, warning=FALSE)

## ----eval=FALSE---------------------------------------------------------------
#  remotes::install_github("anna-neufeld/datathin")

## -----------------------------------------------------------------------------
library(datathin)

## -----------------------------------------------------------------------------
set.seed(1)
dat <- rpois(10000, 7)

## -----------------------------------------------------------------------------
dat.thin <- datathin(dat, family="poisson", epsilon=0.3)
dat.train <- dat.thin$Xtr
dat.test <- dat.thin$Xte

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
dat <- rexp(100000, rate=1/3)
mean(dat)

## -----------------------------------------------------------------------------
folds <- multithin(dat, family="exponential", nfolds=5)

## -----------------------------------------------------------------------------
length(folds)
length(folds[[1]])

## -----------------------------------------------------------------------------
for (m in 1:5) {
  print("-------")
  print(paste("fold", m))
  
  dat.test <- folds[[m]]
  dat.train <- dat - folds[[m]]
  
  print(c(1/5*mean(dat), mean(dat.test)))
  print(c(4/5*mean(dat), mean(dat.train)))

  print(as.numeric(cor(dat.test, dat.train)))
}

## -----------------------------------------------------------------------------
set.seed(3)
dat <- matrix(rnorm(10000*10, mean=5, sd=sqrt(2)), nrow=10000)

## -----------------------------------------------------------------------------
res <- datathin(dat, family="normal", epsilon=0.5, arg=2)
dat.train <- res$Xtr
dat.test <- res$Xte

sapply(1:ncol(dat.train), function(u) cor(dat.train[,u], dat.test[,u]))

## -----------------------------------------------------------------------------
dat <- cbind(rnorm(100000, mean=5, sd=sqrt(0.1)),
             rnorm(100000, mean=5, sd=sqrt(2)),
             rnorm(100000, mean=5, sd=sqrt(20)))

## -----------------------------------------------------------------------------
res <- datathin(dat, family="normal", epsilon=0.5, arg=2)
dat.train <- res$Xtr
dat.test <- res$Xte
sapply(1:ncol(dat.train), function(u) cor(dat.train[,u], dat.test[,u]))

## -----------------------------------------------------------------------------
good_args <- cbind(rep(0.1, 100000), rep(2, 100000), rep(20,100000))
res <- datathin(dat, family="normal", epsilon=0.5, arg=good_args)
dat.train <- res$Xtr
dat.test <- res$Xte
sapply(1:ncol(dat.train), function(u) cor(dat.train[,u], dat.test[,u]))

