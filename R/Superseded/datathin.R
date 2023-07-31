# SUPERSEDED


#' #' Internal function for thinning the Poisson distribution.
#' #' @param data dataset (assumed to follow a Poisson distribution)
#' #' @param epsilon thinning parameter (scalar between 0 and 1).
#' #' @keywords internal
#' #' @noRd
#' #' 
#' #' @importFrom stats rbinom
#' poissplit <- function(data, epsilon) {
#'   #Convert vectors to matrices for consistent processing later
#'   dmat <- as.matrix(data)
#'   X <- matrix(nrow=dim(dmat)[1], ncol=dim(dmat)[2])
#'   Y <- matrix(nrow=dim(dmat)[1], ncol=dim(dmat)[2])
#'   n <- length(dmat)
#'   
#'   X[] <- rbinom(n, dmat, epsilon)
#'   Y <- dmat - X
#'   
#'   return(list(Xtr = X, Xte = Y))
#' }
#' 
#' #' Internal function for thinning the negative binomial distribution.
#' #' @keywords internal
#' #' @noRd
#' #' @param data data (assumed to follow a negative binomial distribution).
#' #' @param epsilon thinning parmaeter (scalar between 0 and 1)
#' #' @param b the negative binomial overdispersion parameter. Must be a scalar, or dimensions must match that of dat. 
#' #' @importFrom VGAM rbetabinom
#' nbsplit <- function(data, epsilon, b) {
#'   #Convert vectors to matrices for consistent processing later
#'   dmat <- as.matrix(data)
#'   ps <- matrix(nrow=dim(dmat)[1], ncol=dim(dmat)[2])
#'   X <- matrix(nrow=dim(dmat)[1], ncol=dim(dmat)[2])
#'   Y <- matrix(nrow=dim(dmat)[1], ncol=dim(dmat)[2])
#'   n <- length(dmat)
#'   
#'   if (is.null(b)) {
#'     print("Negative Binomial dispersion parameter missing.")
#'     return()
#'   }
#'   
#'   ps[] <- rbeta(n,epsilon*b,(1-epsilon)*b)
#'   X[] <- rbinom(n, size=dmat, prob=ps)
#'   Y <- dmat - X
#'   
#'   return(list(Xtr = X, Xte = Y))
#' }
#' 
#' #' @param sigma The standard deviation- not the variance!
#' #' @importFrom stats rnorm
#' #' @keywords internal
#' #' @noRd
#' normsplit <- function(data, epsilon, sigma) {
#'   #Convert vectors to matrices for consistent processing later
#'   
#'   dmat <- as.matrix(data)
#'   X <- matrix(nrow=dim(dmat)[1], ncol=dim(dmat)[2])
#'   Y <- matrix(nrow=dim(dmat)[1], ncol=dim(dmat)[2])
#'   n <- length(dmat)
#'   
#'   if (is.null(sigma)) {
#'     print("Normal standard deviation missing.")
#'     return()
#'   }
#' 
#'   
#'   X[] <- rnorm(n, mean=epsilon*dmat, sd=sqrt(epsilon*(1-epsilon))*sigma)
#'   Y <- dmat - X
#' 
#'   
#'   return(list(Xtr = X, Xte = Y))
#' }
#' 
#' #' @importFrom stats rhyper
#' #' @keywords internal
#' #' @noRd
#' binomsplit <- function(data, epsilon, pop) {
#'   #Convert vectors to matrices for consistent processing later
#'   dmat <- as.matrix(data)
#'   X <- matrix(nrow=dim(dmat)[1], ncol=dim(dmat)[2])
#'   Y <- matrix(nrow=dim(dmat)[1], ncol=dim(dmat)[2])
#'   n <- length(dmat)
#'   
#'   if (is.null(pop)) {
#'     print("Binomial population missing.")
#'   }
#'   
#'   X[] <- rhyper(n, epsilon*pop, (1-epsilon)*pop, dmat)
#'   Y <- dmat - X
#'   
#'   return(list(Xtr = X, Xte = Y))
#' }
#' 
#' #' @importFrom stats rbeta
#' #' @noRd
#' gammasplit <- function(data, epsilon, shape) {
#'   #Convert vectors to matrices for consistent processing later
#'   dmat <- as.matrix(data)
#'   X <- matrix(nrow=dim(dmat)[1], ncol=dim(dmat)[2])
#'   Y <- matrix(nrow=dim(dmat)[1], ncol=dim(dmat)[2])
#'   n <- length(dmat)
#'   
#'   if (is.null(shape)) {
#'     print("Gamma shape parameter missing.")
#'   }
#'   
#'   X[] <- rbeta(n, epsilon*shape, (1-epsilon)*shape)
#'   X <- X * dmat
#'   Y <- dmat - X
#'   
#'   return(list(Xtr = X, Xte = Y))
#' }
#' 
#' #' Takes a dataset (scalar, vector, or matrix) and returns a training set and a test set that sum to the original data matrix. 
#' #' 
#' #' 
#' #' @export
#' #' 
#' #' @param data A scalar, vector, or matrix of data. 
#' #' @param family The name of the distribution of the data. Options are "poisson", "negative binomial", "normal" (equivalently "gaussian"),
#' #' "binomial", "exponential", or "gamma". 
#' #' @param epsilon The tuning parameter for thinning; must be between 0 and 1. Larger values correspond to more information 
#' #' in the training set and less in the test set.
#' #' @param arg The extra parameter that must be known in order to split. Not needed for Poisson or exponential, but needed for all other distributions.
#' #' Should be a scalar or should match dimensions of X. When family="normal", pass in the variance. When family="negative binomial" or "binomial", pass in the size parameter 
#' #' (using the parameterization of rbinom and rnbinom).
#' #' When family="gamma", pass in the shape parameter (using the parameterization of rgamma). 
#' datathin <- function(data, family, epsilon=0.5, arg=NULL) {
#'   
#'   if (!is.null(arg)) {
#'     if (is.numeric(arg)) {
#'       if (length(arg) != 1) {
#'         if (length(arg) != length(data)) {
#'           stop("If `arg` is not a scalar, its dimensions must match those of `data`.")
#'         } 
#'       }
#'     } else {
#'       if (dim(arg) != dim(data)) {
#'         stop("If `arg` is not a scalar, its dimensions must match those of `data`.")
#'       }
#'     }
#'   }
#'   
#'   if (family == "poisson") {
#'     poissplit(data, epsilon)
#'   } else if (family == "negative binomial") {
#'     nbsplit(data, epsilon, arg)
#'   } else if (family %in% c("normal", "gaussian")) {
#'     normsplit(data, epsilon, sqrt(arg))
#'   } else if (family == "binomial") {
#'     if ((epsilon*arg %% 1) != 0 | ((1-epsilon)*arg %% 1) != 0) {
#'       print("Hypergeometric counts are non-integers.")
#'     } else {
#'       binomsplit(data, epsilon, arg)
#'     }
#'   } else if (family == "exponential") {
#'     gammasplit(data, epsilon, 1)
#'   } else if (family == "gamma") {
#'     gammasplit(data, epsilon, arg)
#'   } else {
#'     print("Invalid family argument")
#'   }
#' }
#' 
#' #' Takes a dataset (scalar, vector, or matrix) and returns multiple folds of data that sum to the original data matrix. 
#' #' 
#' #' 
#' #' @export
#' #' 
#' #' @param data A scalar, vector, or matrix of data. 
#' #' @param family The name of the distribution of the data. Options are "poisson", "negative binomial", "normal" (equivalently "gaussian),
#' #' "binomial", "exponential", or "gamma". 
#' #' @param nfolds The number of folds to create from the data.
#' #' @param eps The tuning paramter vector for thinning; must be non-negative, sum to 1, and have the same length as the value of nfolds. 
#' #' Controls the allocation of information between folds. If omitted, an equal allocation among folds is assumed.
#' #' @param arg The extra parameter that must be known in order to split. Not needed for Poisson, but needed for all other distributions.
#' #' Should be a scalar or should match dimensions of X. 
#' multithin <- function(data, family, nfolds=5, eps=NULL, arg=NULL) {
#'   
#'   if (!is.null(arg)) {
#'     if (is.numeric(arg)) {
#'       if (length(arg) != 1) {
#'         if (length(arg) != length(data)) {
#'           stop("If `arg` is not a scalar, its dimensions must match those of `data`.")
#'       } 
#'     }
#'     } else {
#'     if (dim(arg) != dim(data)) {
#'       stop("If `arg` is not a scalar, its dimensions must match those of `data`.")
#'     }
#'     }
#'   }
#'   if (is.null(eps)) {
#'     eps <- rep(1/nfolds, nfolds)
#'   }
#'  
#'   family2 <- family
#'   arg2 <- arg
#'   if (family == "exponential") {
#'     family2 <- "gamma"
#'     arg2 <- 1
#'   }
#'   output <- list()
#'   resdat <- data
#'   for (i in 1:(nfolds-1)) {
#'     epsfold <- eps[i]/sum(eps[i:length(eps)]) 
#'     temp <- datathin(resdat, family2, epsfold, arg2)
#'     
#'     output <- append(output, list(i = temp$Xtr))
#'     resdat <- temp$Xte
#'     arg2 <- arg2 * (1-epsfold)
#'   }
#'   output <- append(output, list(nfolds = resdat))
#'   names(output) <- 1:nfolds
#'   return(output)
#' }
