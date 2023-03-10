% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datathin.R
\name{multithin}
\alias{multithin}
\title{Takes a dataset (scalar, vector, or matrix) and returns multiple folds of data that sum to the original data matrix.}
\usage{
multithin(data, family, nfolds = 5, eps = NULL, arg = NULL)
}
\arguments{
\item{data}{A scalar, vector, or matrix of data.}

\item{family}{The name of the distribution of the data. Options are "poisson", "negative binomial", "normal" (equivalently "gaussian),
"binomial", "exponential", or "gamma".}

\item{nfolds}{The number of folds to create from the data.}

\item{eps}{The tuning paramter vector for thinning; must be non-negative, sum to 1, and have the same length as the value of nfolds.
Controls the allocation of information between folds. If omitted, an equal allocation among folds is assumed.}

\item{arg}{The extra parameter that must be known in order to split. Not needed for Poisson, but needed for all other distributions.
Should be a scalar or should match dimensions of X.}
}
\description{
Takes a dataset (scalar, vector, or matrix) and returns multiple folds of data that sum to the original data matrix.
}
