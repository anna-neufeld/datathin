#' @importFrom stats rbinom
poissplit <- function(data, epsilon) {
  #Convert vectors to matrices for consistent processing later
  dmat <- as.matrix(data)
  X <- matrix(nrow=dim(dmat)[1], ncol=dim(dmat)[2])
  Y <- matrix(nrow=dim(dmat)[1], ncol=dim(dmat)[2])
  n <- length(dmat)
  
  X[] <- rbinom(n, dmat, epsilon)
  Y <- dmat - X
  
  return(list(Xtr = X, Xte = Y))
}

#' @importFrom VGAM rbetabinom
nbsplit <- function(data, epsilon, b) {
  #Convert vectors to matrices for consistent processing later
  dmat <- as.matrix(data)
  X <- matrix(nrow=dim(dmat)[1], ncol=dim(dmat)[2])
  Y <- matrix(nrow=dim(dmat)[1], ncol=dim(dmat)[2])
  n <- length(dmat)
  
  if (is.null(b)) {
    print("Negative Binomial dispersion parameter missing.")
    return()
  }
  
  X[] <- VGAM::rbetabinom(n, X, epsilon*b, (1-epsilon)*b)
  Y <- dmat - X
  
  return(list(Xtr = X, Xte = Y))
}

#' @importFrom stats rnorm
normsplit <- function(data, epsilon, sigma) {
  #Convert vectors to matrices for consistent processing later
  dmat <- as.matrix(data)
  X <- matrix(nrow=dim(dmat)[1], ncol=dim(dmat)[2])
  Y <- matrix(nrow=dim(dmat)[1], ncol=dim(dmat)[2])
  n <- length(dmat)
  
  if (is.null(sigma)) {
    print("Normal standard deviation missing.")
    return()
  }
  
  X[] <- rnorm(n, mean=epsilon*X, sd=sqrt(epsilon*(1-epsilon))*sigma)
  Y <- dmat - X
  
  return(list(Xtr = X, Xte = Y))
}

#' @importFrom stats rhyper
binomsplit <- function(data, epsilon, pop) {
  #Convert vectors to matrices for consistent processing later
  dmat <- as.matrix(data)
  X <- matrix(nrow=dim(dmat)[1], ncol=dim(dmat)[2])
  Y <- matrix(nrow=dim(dmat)[1], ncol=dim(dmat)[2])
  n <- length(dmat)
  
  if (is.null(pop)) {
    print("Binomial population missing.")
  }
  if (
    (epsilon*pop %% 1) != 0 | 
    ((1-epsilon)*pop %% 1) != 0
    ) {
    print("Hypergeometric counts are non-integers.")
  }
  
  X[] <- rhyper(n, epsilon*pop, (1-epsilon)*pop, dmat)
  Y <- dmat - X
  
  return(list(Xtr = X, Xte = Y))
}

#' @importFrom stats beta
gammasplit <- function(data, epsilon, shape) {
  #Convert vectors to matrices for consistent processing later
  dmat <- as.matrix(data)
  X <- matrix(nrow=dim(dmat)[1], ncol=dim(dmat)[2])
  Y <- matrix(nrow=dim(dmat)[1], ncol=dim(dmat)[2])
  n <- length(dmat)
  
  if (is.null(shape)) {
    print("Gamma shape parameter missing.")
  }
  
  X[] <- rbeta(n, epsilon*shape, (1-epsilon)*shape)
  X <- X * dmat
  Y <- dmat - X
  
  return(list(Xtr = X, Xte = Y))
}

#' Takes a dataset (scalar, vector, or matrix???) and returns a training set and a test set that sum to the original data matrix. 
#' 
#' 
#' @export
#' 
#' @param X A scalar, vector, or matrix of data. 
#' @param family The name of the distribution of the data. Options are "poisson", "negative binomial", "normal" (equivalently "gaussian),
#' "binomial", "exponential", or "gamma". 
#' @param epsilon The tuning parameter for thinning; must be between 0 and 1. Larger values correspond to more information 
#' in the training set and less in the test set.
#' @param arg The extra parameter that must be known in order to split. Not needed for Poisson, but needed for all other distributions.
#' Should be a scalar or should match dimensions of X. 
datathin <- function(data, family, epsilon=0.5, arg=NULL) {
  if (family == "poisson") {
    poissplit(data, epsilon)
  } else if (family == "negative binomial") {
    nbsplit(data, epsilon, arg)
  } else if (family %in% c("normal", "gaussian")) {
    normsplit(data, epsilon, arg)
  } else if (family == "binomial") {
    binomsplit(data, epsilon, arg)
  } else if (family == "exponential") {
    gammasplit(data, epsilon, 1)
  } else if (family == "gamma") {
    gammasplit(data, epsilon, arg)
  } else if (family == "chi-squared") {
    ### question from Anna-- doesn't this not work?? 
    gammasplit(data, epsilon, arg/2)
  }
}

#' Takes a dataset (scalar, vector, or matrix???) and returns multiple folds of data that sum to the original data matrix. 
#' 
#' 
#' @export
#' 
#' @param X A scalar, vector, or matrix of data. 
#' @param family The name of the distribution of the data. Options are "poisson", "negative binomial", "normal" (equivalently "gaussian),
#' "binomial", "exponential", or "gamma". 
#' @param nfolds The number of folds to create from the data.
#' @param arg The extra parameter that must be known in order to split. Not needed for Poisson, but needed for all other distributions.
#' Should be a scalar or should match dimensions of X. 
multithin <- function(data, family, nfolds=5, arg=NULL) {
  family2 <- family
  arg2 <- arg
  if (family == "exponential") {
    family2 <- "gamma"
    arg2 <- 1
  }
  output <- list()
  resdat <- data
  for (i in 1:(nfolds-1)) {
    epsfold <- 1/(nfolds - i + 1)
    temp <- datathin(resdat, family2, epsfold, arg2)
    
    output <- append(output, list(i = temp$Xtr))
    resdat <- temp$Xte
    arg2 <- arg2 * (1-epsfold)
  }
  output <- append(output, list(nfolds = resdat))
  names(output) <- 1:nfolds
  return(output)
}
