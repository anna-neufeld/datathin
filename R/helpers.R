## POISSON FAMILY

#' Internal function for thinning the Poisson distribution.
#' @keywords internal
#' @noRd
#'
#' @param data dataset (assumed to follow a Poisson distribution)
#' @param epsilon DTCC thinning parameter (simplex-valued).
#' @importFrom stats rmultinom
poisthin <- function(data, epsilon) {
  if (min(data) < 0) {
    print("Poisson data must be non-negative.")
    return()
  }
  if (all((data - floor(data)) != 0)) {
    print("Poisson data must be integer valued.")
    return()
  }

  X <- apply(data, 1:2, function(x) rmultinom(1, x, epsilon))
  X <- aperm(X, c(2,3,1))

  return(X)
}

#' Internal function for thinning the negative binomial distribution.
#' @keywords internal
#' @noRd
#'
#' @param data data (assumed to follow a negative binomial distribution).
#' @param epsilon DTCC thinning parameter (simplex-valued).
#' @param b negative binomial overdispersion parameter.
#' @importFrom extraDistr rdirmnom
nbthin <- function(data, epsilon, b) {
  if (min(data) < 0) {
    print("Negative binomial data must be non-negative.")
    return()
  }
  if (all((data - floor(data)) != 0)) {
    print("Negative binomial data must be integer valued.")
    return()
  }
  if (min(b) <= 0) {
    print("Overdispersion parameter must be positive.")
    return()
  }

  X <- mapply(function(x,y) rdirmnom(1, x, y*epsilon), data, b)
  X <- aperm(array(X, dim=c(length(epsilon),dim(data))), c(2,3,1))

  return(X)
}

## NORMAL FAMILY

#' Internal function for thinning the normal distribution.
#' @keywords internal
#' @noRd
#'
#' @param data data (assumed to follow a normal distribution).
#' @param epsilon DTCC thinning parameter (simplex-valued).
#' @param sigma normal variance parameter.
#' @importFrom mvtnorm rmvnorm
normthin <- function(data, epsilon, sigma) {
  if (min(sigma) <= 0) {
    print("Variance parameter must be positive.")
    return()
  }

  X <- mapply(function(x,y) rmvnorm(1, x*epsilon, y*(diag(epsilon) - epsilon%*%t(epsilon))), data, sigma)
  X <- aperm(array(X, dim=c(length(epsilon),dim(data))), c(2,3,1))

  return(X)
}

#' Internal function for thinning the normal distribution with unknown variance.
#' @keywords internal
#' @noRd
#'
#' @param data data (assumed to follow a normal distribution).
#' @param mu normal mean parameter.
#' @param K Number of folds.
normvarthin <- function(data, mu, K) {
  X <- gammathin((data - mu)^2, rep(1, K)/K, matrix(1/2, nrow=nrow(data), ncol=ncol(data)))

  return(X)
}

#' Internal function for thinning the normal distribution.
#' @keywords internal
#' @noRd
#'
#' @param data data matrix (each row is assumed to follow a multivariate normal distribution).
#' @param epsilon DTCC thinning parameter (simplex-valued).
#' @param sigma multivariate normal covariance parameter.
#' @importFrom mvtnorm rmvnorm
mvnormthin <- function(data, epsilon, sigma) {
  if (length(dim(sigma)) == 2) {
    temp <- array(0, dim=c(dim(data)[1], dim(sigma)))
    for (i in 1:dim(data)[1]) {
      temp[i,,] <- sigma
    }
    sigma <- temp
  }
  if (dim(sigma)[2] != dim(sigma)[3]) {
    print("Sigma matrices must be square.")
  }

  nfold <- length(epsilon)
  X <- array(0, dim=c(dim(data), nfold))
  resdat <- data
  sigma2 <- sigma

  for (i in 1:(nfold-1)) {
    epsfold <- epsilon[i]/sum(epsilon[i:nfold])

    for (j in 1:nrow(resdat)) {
      X[j,,i] <- rmvnorm(1, resdat[j,]*epsfold, epsfold*(1-epsfold)*sigma2[j,,])
      resdat[j,] <- resdat[j,] - X[j,,i]
    }

    sigma2 <- sigma2 * (1-epsfold)
  }

  X[,,nfold] <- resdat

  return(X)
}

## BINOMIAL FAMILY

#' Internal function for thinning the binomial distribution.
#' @keywords internal
#' @noRd
#'
#' @param data data (assumed to follow a binomial distribution).
#' @param epsilon DTCC thinning parameter (simplex-valued).
#' @param pop binomial population parameter.
#' @importFrom extraDistr rmvhyper
binomthin <- function(data, epsilon, pop) {
  if (min(data) < 0) {
    print("Binomial data must be non-negative.")
    return()
  }
  if (all((data - floor(data)) != 0)) {
    print("Binomial data must be integer valued.")
    return()
  }
  test <- outer(epsilon, pop)
  if (all((test - floor(test)) != 0)) {
    print("Epsilon implies non-integer thinned population parameters.")
    return()
  }

  X <- mapply(function(x,y) rmvhyper(1, y*epsilon, x), data, pop)
  X <- aperm(array(X, dim=c(length(epsilon),dim(data))), c(2,3,1))

  return(X)
}

#' Internal function for thinning the multinomial distribution.
#' @keywords internal
#' @noRd
#'
#' @param data data (each row is assumed to follow a multinomial distribution).
#' @param epsilon DTCC thinning parameter (simplex-valued).
#' @param pop multinomial population parameter (scalar-valued).
#' @importFrom extraDistr rmvhyper
multinomthin <- function(data, epsilon, pop) {
  if (min(data) < 0) {
    print("Multinomial data must be non-negative.")
    return()
  }
  if (all((data - floor(data)) != 0)) {
    print("Multinomial data must be integer valued.")
    return()
  }
  test <- outer(epsilon, pop)
  if (all((test - floor(test)) != 0)) {
    print("Epsilon implies non-integer thinned population parameters.")
    return()
  }

  nfold <- length(epsilon)
  X <- array(0, dim=c(dim(data), nfold))
  resdat <- data
  pop2 <- pop

  for (i in 1:(nfold-1)) {
    epsfold <- epsilon[i]/sum(epsilon[i:nfold])

    for (j in 1:nrow(resdat)) {
      X[j,,i] <- rmvhyper(1, resdat[j,], epsfold*pop2[j])
      resdat[j,] <- resdat[j,] - X[j,,i]
    }

    pop2 <- pop2 * (1-epsfold)
  }

  X[,,nfold] <- resdat

  return(X)
}

## GAMMA FAMILY

#' Internal function for thinning the gamma distribution.
#' @keywords internal
#' @noRd
#'
#' @param data data (assumed to follow a gamma distribution).
#' @param epsilon DTCC thinning parameter (simplex-valued).
#' @param shape gamma shape parameter.
#' @importFrom extraDistr rdirichlet
gammathin <- function(data, epsilon, shape) {
  if (min(data) <= 0) {
    print("Gamma data must be positive.")
    return()
  }
  if (min(shape) <= 0) {
    print("Shape parameter must be positive.")
    return()
  }

  X <- mapply(function(x,y) x*rdirichlet(1, y*epsilon), data, shape)
  X <- aperm(array(X, dim=c(length(epsilon),dim(data))), c(2,3,1))

  return(X)
}

#' Internal function for thinning the scaled chi-squared distribution into normal random variables.
#' @keywords internal
#' @noRd
#'
#' @param data data (assumed to follow a chi-squared distribution).
#' @param K Number of folds.
#' @importFrom mvtnorm rmvnorm
chisqthin <- function(data, K) {
  if (min(data) <= 0) {
    print("Chi-squared data must be positive.")
    return()
  }

  X <- apply(data, 1:2, function(x) {Z <- rmvnorm(1, mean=rep(0, K)); sqrt(x)*Z/sqrt(sum(Z^2))})
  X <- aperm(X, c(2,3,1))

  return(X)
}

#' Internal function for thinning the gamma distribution into weibull random variables.
#' @keywords internal
#' @noRd
#'
#' @param data data (assumed to follow a chi-squared distribution).
#' @param K Number of folds.
#' @param nu Weibull scale parameter
gammaweibullthin <- function(data, K, nu) {
  if (min(data) <= 0) {
    print("Weibull data must be positive.")
    return()
  }

  X <- gammathin(data, rep(1, K)/K, matrix(K, nrow=nrow(data), ncol=ncol(data)))
  for (k in 1:K) {
    X[,,k] <- X[,,k]^(1/nu)
  }

  return(X)
}

#' Internal function for thinning the Weibull distribution.
#' @keywords internal
#' @noRd
#'
#' @param data data (assumed to follow a Weibull distribution).
#' @param nu Weibull scale parameter.
#' @param K Number of folds.
weibullthin <- function(data, nu, K) {
  if (min(data) <= 0) {
    print("Weibull data must be positive.")
    return()
  }
  if (min(nu) <= 0) {
    print("Scale parameter must be positive.")
    return()
  }

  X <- gammathin(data^nu, rep(1, K)/K, matrix(1, nrow=nrow(data), ncol=ncol(data)))

  return(X)
}

#' Internal function for thinning the Pareto distribution.
#' @keywords internal
#' @noRd
#'
#' @param data data (assumed to follow a Pareto distribution).
#' @param nu Pareto scale parameter.
#' @param K Number of folds.
paretothin <- function(data, nu, K) {
  if (min(data) <= 1) {
    print("Pareto data must be greater than 1.")
    return()
  }
  if (min(nu) <= 0) {
    print("Scale parameter must be positive.")
    return()
  }

  X <- gammathin(log(data/nu), rep(1, K)/K, matrix(1, nrow=nrow(data), ncol=ncol(data)))

  return(X)
}

#' Internal function for thinning the shifted exponential distribution.
#' @keywords internal
#' @noRd
#'
#' @param data data (assumed to follow a shifted exponential distribution).
#' @param lambda Exponential rate parameter.
#' @param K Number of folds to generate.
#' @importFrom extraDistr rcat
#' @importFrom stats rexp
#' @keywords internal
#' @noRd
shiftexpthin <- function(data, lambda, K) {
  if (min(lambda) <= 0) {
    print("Rate parameter must be positive.")
    return()

  }

  X <- mapply(function(x,y) x + rexp(K, y/K), data, lambda)
  X <- aperm(array(X, dim=c(K,dim(data))), c(2,3,1))
  C <- matrix(rcat(prod(dim(data)), rep(1,K)/K), nrow=nrow(data))

  for (i in 1:nrow(data)) {
    for (j in 1:ncol(data)) {
      X[i,j,C[i,j]] <- data[i,j]
    }
  }

  return(X)
}

## BETA FAMILY

#' Internal function for thinning the beta distribution into 2 gamma random variables.
#' @keywords internal
#' @noRd
#'
#' @param data data (assumed to follow a beta distribution).
#' @param theta The tuning parameter
#' @param phi The sum of the beta parameters
#' @importFrom stats rgamma
#' @keywords internal
#' @noRd
betagammasplit <- function(data, phi, theta) {
  if (min(data) <= 0 | max(data) >= 1) {
    print("Beta data must be in the (0,1) interval.")
    return()
  }
  if (phi <= 0) {
    print("Sum of beta parameters must be positive.")
    return()
  }
  if (theta <= 0) {
    print("Tuning parameter must be positive.")
    return()

  }
  X <- array(0, dim=c(dim(data), 2))
  X[,,1] <- apply(data, 1:2, function(x) rgamma(1, phi, theta/x))
  X[,,2] <- X[,,1]/data - X[,,1]

  return(X)

  # X[] <- rgamma(n, shape=phi, rate=theta/dmat)
  # Y <- X/Z - X
  #
  # return(list(Xtr = X, Xte = Y))
}

#' Internal function for thinning the scaled beta distribution.
#' @keywords internal
#' @noRd
#'
#' @param data data (assumed to follow a scaled beta distribution).
#' @param alpha First beta parameter.
#' @param K Number of folds to generate.
#' @importFrom extraDistr rcat
#' @importFrom stats rbeta
#' @keywords internal
#' @noRd
scaledbetathin <- function(data, alpha, K) {
  if (min(data) <= 0) {
    print("Scaled beta data must be positive.")
    return()
  }
  if (min(alpha) <= 0) {
    print("First beta parameter must be positive.")
    return()

  }

  X <- mapply(function(x,y) x*rbeta(K, y/K, 1), data, alpha)
  X <- aperm(array(X, dim=c(K,dim(data))), c(2,3,1))

  C <- matrix(rcat(prod(dim(data)), rep(1,K)/K), nrow=nrow(data))

  for (i in 1:nrow(data)) {
    for (j in 1:ncol(data)) {
      X[i,j,C[i,j]] <- data[i,j]
    }
  }

  return(X)
}














