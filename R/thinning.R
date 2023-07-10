##### ADD IN CALLS TO FIX ARG AND AS.MATRIX DATA IN HERE NOT IN THE HELPERS
#Convert vectors to matrices for consistent processing later
dmat <- as.matrix(data)

#' Takes a dataset (scalar, vector, or matrix) and returns a training set and a test set that sum to the original data matrix. 
#' 
#' 
#' @export
#' 
#' @param data A scalar, vector, or matrix of data. 
#' @param family The name of the distribution of the data. Options are "poisson", "negative binomial", "normal" 
#' (equivalently "gaussian"),
#' "binomial", "exponential", or "gamma". 
#' @param epsilon The tuning parameter for thinning; must be between 0 and 1. Larger values correspond to more information 
#' in the training set and less in the test set.
#' @param arg The extra parameter that must be known in order to split. Not needed for Poisson or exponential, but needed for 
#' all other distributions.
#' Should be a scalar or should match dimensions of X. When family="normal", pass in the variance. When family="negative binomial" 
#' or "binomial", 
#' pass in the size parameter.
#' When family="gamma", pass in the shape parameter. 
datathin <- function(data, family, epsilon=0.5, arg=NULL, tuning=NULL) {
  
  if (!is.null(arg)) {
    if (is.numeric(arg)) {
      if (length(arg) != 1) {
        if (length(arg) != length(data)) {
          stop("If `arg` is not a scalar, its dimensions must match those of `data`.")
        } 
      }
    } else {
      if (dim(arg) != dim(data)) {
        stop("If `arg` is not a scalar, its dimensions must match those of `data`.")
      }
    }
  }
  
  if (family == "poisson") {
    poissplit(data, epsilon)
  } else if (family == "negative binomial") {
    nbsplit(data, epsilon, arg)
  } else if (family %in% c("normal", "gaussian")) {
    normsplit(data, epsilon, sqrt(arg))
  } else if (family == "binomial") {
    if ((epsilon*arg %% 1) != 0 | ((1-epsilon)*arg %% 1) != 0) {
      print("Hypergeometric counts are non-integers.")
    } else {
      binomsplit(data, epsilon, arg)
    }
  } else if (family == "exponential") {
    gammasplit(data, epsilon, 1)
  } else if (family == "gamma") {
    gammasplit(data, epsilon, arg)
  } else {
    print("Invalid family argument")
  }
}

#' Takes a dataset (scalar, vector, or matrix) and returns multiple folds of data that can be recombined into to the original data matrix using some function T.
#' 
#' 
#' @export
#' 
#' @param data A scalar, vector, or matrix of data. 
#' @param decomp The name of the distribution of the data. Options are "poisson", "negbin", "normal-mean" (equivalently "gaussian-mean"),
#' "normal-variance" (equivalently "gaussian-variance"),
#' "binomial", "exponential", or "gamma-rate". 
#' @param nfolds The number of folds to create from the data. Note that in certain decompositions, the number of folds implies a value for one 
#' of the parameters. For example, specifying the number of folds with the "gamma-weibull" or "gamma-chisq" decompositions implies a value for 
#' the shape parameter of the gamma distribution. In the special case of the "beta-gamma" decomposition, where the number of folds is fixed at 2, 
#' the nfolds argument is repurposed as the tuning parameter.
#' refer to \url{https://arxiv.org/abs/2303.12931} for further details.
#' @param arg The extra parameter that must be known in order to split. 
gdt <- function(data, decomp, nfolds=2, arg=NULL) {
  
  if (!is.null(arg)) {
    if (is.numeric(arg)) {
      if (length(arg) != 1) {
        if (length(arg) != length(data)) {
          stop("If `arg` is not a scalar, its dimensions must match those of `data`.")
        } 
      }
    } else {
      if (dim(arg) != dim(data)) {
        stop("If `arg` is not a scalar, its dimensions must match those of `data`.")
      }
    }
  }
  
  if (decomp %in% c("poisson", "negbin", "normal-mean", "gaussian-mean", "binomial", "exponential", "gamma-rate")) {
    family <- case_when(
      decomp == "negbin" ~ "negative binomial",
      decomp %in% c("normal-mean", "gaussian-mean") ~ "normal",
      decomp == "gamma-rate" ~ "gamma",
      TRUE ~ "decomp"
    )
    multithin(data, family, nfolds=nfolds, arg=arg)
  } else if (decomp == "normal-variance") {
    multithin((data-arg)^2, "gamma", nfolds=nfolds, arg=0.5)
  } else if (decomp == "beta-gamma") {
    betagammasplit(data, nfolds, arg)
  } else {
    print("Invalid decomposition argument.")
  }
}