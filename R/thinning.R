##### ADD IN CALLS TO FIX ARG AND AS.MATRIX DATA IN HERE NOT IN THE HELPERS


#' Takes a dataset (scalar, vector, or matrix) and returns a training set and a test set that sum to the original data matrix. 
#' 
#' 
#' @export
#' 
#' @param data A scalar, vector, or matrix of data. 
#' @param family The distribution of the data. Options are "poisson", "negative binomial", "normal" (or "gaussian"), 
#' "normal-variance" (or "gaussian-variance"), "mvnormal" (or "mvgaussian"), "binomial", "multinomial", "exponential",
#' "gamma", "chi-squared", "gamma-weibull", "weibull", "pareto", "shifted-exponential", "scaled-uniform", "scaled-beta".
#' @param K The number of folds. Note that for the "chi-squared" and "gamma-weibull" decompositions, the number of folds implies the
#' degrees of freedom and shape parameters respectively.
#' @param epsilon The tuning parameter for convolution-closed data thinning; must be a simplex vector of length K. Larger values 
#' correspond to more information in the respective fold. Available for "poisson", "negative binomial", "normal" (or "gaussian"),
#' "mvnormal" (or "mvgaussian"), "binomial", "multinomial", "exponential", and "gamma" families. If epsilon is not supplied,
#' rep(1/K, K) is used.
#' @param arg The extra parameter that must be known in order to thin. Either a scalar or a matrix with the same dimensions as data 
#' (excluding "mvnormal" (or "mvgaussian") and "multinomial" families; see below). 
#' Requirements vary by decomposition:
#' * Not needed for "poisson", "exponential", "chi-squared", or "scaled-uniform" distributions.
#' * "negative binomial" requires the size parameter.
#' * "normal" (or "gaussian") requires the variance.
#' * "normal-variance" (or "gaussian-variance") requires the mean.
#' * "mvnormal" (or "mvgaussian") requires the covariance matrix. If the dimensions of data are nxp, arg must be nxpxp.
#' * "binomial" and "multinomial" require the number of trials. For "multinomial", if the dimensions of data are nxp, arg must be a 
#' vector of length n.
#' * "gamma", "gamma-weibull", and "weibull" requires the shape parameter.
#' * "pareto" requires the location paramter.
#' * "shifted-exponential" requires the rate parameter.
#' * "scaled-beta" requires the first shape parameter.  
#' 
#' Please refer to \url{https://arxiv.org/abs/2301.07276} and \url{https://arxiv.org/abs/2303.12931} for further details.
#' 
#' @details See \url{https://anna-neufeld.github.io/datathin/articles/introduction_tutorial.html} for examples of each decomposition.
datathin <- function(data, family, K=2, epsilon=NULL, arg=NULL) {
  #Convert vectors to matrices for consistent processing later
  data <- as.matrix(data)
  np <- dim(data)
  
  #arg dimension check
  if (family %in% c("poisson", "exponential", "chi-squared", "scaled-uniform")) {
    if (!is.null(arg)) {
      warning(paste0("Extra parameter provided was not used in ", family, " thinning."))
    }
  } else {
    if (is.null(arg)) {
      stop("Extra parameter missing.")
    } else if (!is.numeric(arg)) {
      stop("Non-numeric parameter provided.")
    } else {
      if (family %in% c("mvnormal", "mvgaussian")) {
        if (length(dim(arg)) == 2) {
          if (any(dim(arg) != c(np[2], np[2]))) {
            stop("Incorrect dimensions for multivariate normal covariance matrices.")
          }
        } else if (length(dim(arg)) == 3) {
          if (any(dim(arg) != c(np[1], np[2], np[2]))) {
            stop("Incorrect dimensions for multivariate normal covariance matrices.")
          }
        }
      } else if (family == "multinomial") {
        if (!(length(arg) %in% c(1, np[1]))) {
          stop("Incorrect dimensions for multinomial trials parameter.")
        }
      } else {
        if (length(arg) > 1) {
          if (any(dim(as.matrix(arg)) != dim(data))) {
            stop("If `arg` is not a scalar, its dimensions must match those of `data`.")
          }
        }
      }
    }
  }
  
  #Set epsilon
  if (family %in% c("poisson", "negative binomial", "normal", "gaussian", "mvnormal", 
                   "mvgaussian", "binomial", "multinomial", "exponential", "gamma")) {
    if (is.null(epsilon)) {
      epsilon <- rep(1/K, K)
    } else {
      if (sum(epsilon) != 1) {
        stop("Epsilon does not sum to 1.")
      }
      if (length(epsilon) != K) {
        warning("K parameter will be ignored in favour of the length of epsilon.")
      }
    }
  }

  
  
  # Convert scalar arg to matrix
  if (length(arg) == 1) {
    if (family == "multinomial") {
      arg <- rep(arg, np[1])
    } else {
      arg <- matrix(arg, nrow=np[1], ncol=np[2])
    }
  }
  
  if (family == "poisson") {
    poisthin(data, epsilon)
  } else if (family == "negative binomial") {
    nbthin(data, epsilon, arg)
  } else if (family %in% c("normal", "gaussian")) {
    normthin(data, epsilon, arg)
  } else if (family %in% c("normal-variance", "gaussian-variance")) {
    normvarthin(data, arg, K)
  } else if (family %in% c("mvnormal", "mvgaussian")) {
    mvnormthin(data, epsilon, arg)
  } else if (family == "binomial") {
    binomthin(data, epsilon, arg)
  } else if (family == "multinomial") {
    multinomthin(data, epsilon, arg) 
  } else if (family == "exponential") {
    gammathin(data, epsilon, matrix(1, nrow=np[1], ncol=np[2]))
  } else if (family == "gamma") {
    gammathin(data, epsilon, arg)
  } else if (family == "chi-squared") {
    chisqthin(data, K)
  } else if (family == "gamma-weibull") {
    gammaweibullthin(data, K, arg)
  } else if (family == "weibull") {
    weibullthin(data, arg, K)
  } else if (family == "pareto") {
    paretothin(data, arg, K) 
  } else if (family == "shifted-exponential") {
    shiftexpthin(data, arg, K)
  } else if (family == "scaled-uniform") {
    scaledbetathin(data, matrix(1, nrow=np[1], ncol=np[2]), K)
  } else if (family == "scaled-beta") {
    scaledbetathin(data, arg, K)
  } else {
    print("Invalid family argument")
  }
}
