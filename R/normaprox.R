#' @title Normal Approximation via Standardized Binomial Simulation
#'
#' @description
#' Generates random numbers approximately following a normal distribution
#' using a standardized binomial random variable and applying a linear
#' transformation to match a specified mean and standard deviation.
#'
#'
#' @param n Integer. Number of observations to generate.
#' @param mu Numeric. Desired mean of the normal distribution (default = 0).
#' @param sigma Numeric. Desired standard deviation of the normal distribution (default = 1).
#'
#' @details
#' This function simulates binomial random variables using \code{binomr}
#' from the simulationPackage with fixed parameters \code{m = 1000} and
#' \code{p = 0.4987}. The simulated values are then standardized using
#' the normal approximation:
#'
#' \deqn{Z = \frac{X - mp}{\sqrt{mp(1-p)}}}
#'
#' Finally, a linear transformation is applied to obtain:
#'
#' \deqn{Y = Z \cdot \sigma + \mu}
#'
#' which approximates a normal distribution \eqn{N(\mu, \sigma^2)}.
#'
#' @return A numeric vector of length \code{n} containing simulated values
#' approximately distributed as \eqn{N(\mu, \sigma^2)}.
#'
#' @examples
#' # Generate 100 standard normal approximations
#' x <- normaprox(100)
#'
#' # Generate 1000 observations with mean 5 and sd 2
#' y <- normaprox(1000, mu = 5, sigma = 2)
#'
#' @import simulationPackage
#' @export
normaprox <- function(n, mu = 0, sigma = 1){
  m <- 1000
  p <- 0.4987
  sampbinom <- as.vector(binomr(n = n, m, p))
  est <- (sampbinom - (m * p)) / sqrt(m * p * (1 -p))
  (est * sigma) + mu
}
