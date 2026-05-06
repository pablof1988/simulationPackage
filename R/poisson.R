#' @title Pseudo-random number generator from Poisson distribution
#'
#' @description
#' Generates pseudo-random numbers from a Poisson distribution with parameter
#' \eqn{\lambda}, using the inverse transform method.
#'
#' @param n Integer. Length of the pseudo-random sample to be generated.
#' @param lambda Numeric. The mean parameter of the Poisson distribution.
#'
#' @details
#' The function builds the cumulative distribution function (CDF) of the Poisson
#' up to \eqn{k = qpois(0.999, \lambda)}.
#' For each uniform random number \eqn{u \sim U(0,1)}, the algorithm finds the
#' corresponding interval in the CDF, obtaining the simulated Poisson value.
#'
#' The last probability is adjusted to ensure that the total sum equals exactly 1.
#'
#' @return
#' A numeric vector of length \code{n} with integer values simulated from
#' Poisson(\eqn{\lambda}).
#' The result also carries an attribute \code{"fx"} containing the vector of
#' probabilities used in the simulation.
#'
#' @examples
#' # Generate a sample of 20 values from Poisson(2)
#' sample <- poisson(20, lambda = 2)
#' sample
#'
#' # Verify that the probabilities sum to 1
#' sum(attr(sample, "fx"))
#'
#' @export
poisson <- function(n, lambda){
  k <- qpois(0.999, lambda)
  x <- 0:k

  fx <- dpois(x, lambda = lambda)
  fx[length(fx)] <- fx[length(fx)] + (1 - sum(fx))

  Fx <- cumsum(fx)
  u <- runif(n)
  res <- findInterval(u, Fx)

  attr(res, "fx") <- fx
  res
}
