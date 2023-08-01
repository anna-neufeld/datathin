#' Takes a dataset (scalar, vector, or matrix) and returns a training set and a test set that sum to the original data matrix.
#'
#'
#' @export
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
