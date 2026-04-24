#' @title Chi-Squared Approximation via Sum of Squared Normal Variables
#'
#' @description
#' Generates random numbers approximately following a chi-squared distribution
#' by summing the squares of independent standard normal random variables.
#'
#' @param n Integer. Number of observations to generate.
#' @param v Integer. Degrees of freedom of the chi-squared distribution.
#'
#' @details
#' This function uses the result that a chi-squared distributed random variable
#' with \code{v} degrees of freedom can be expressed as the sum of squares of
#' \code{v} independent standard normal random variables:
#'
#' \deqn{X = \sum_{i=1}^{v} Z_i^2}
#'
#' where each \eqn{Z_i \sim N(0,1)}.
#'
#' The function relies on \code{normaprox} to generate approximate standard
#' normal random variables, which are then squared and summed to obtain
#' values that approximate a chi-squared distribution with \code{v} degrees
#' of freedom.
#'
#' @return A numeric vector of length \code{n} containing simulated values
#' approximately distributed as \eqn{\chi^2(v)}.
#'
#' @examples
#' # Generate 100 observations from a chi-squared distribution with 5 degrees of freedom
#' x <- chiaprox(100, v = 5)
#'
#' # Generate 1000 observations with 10 degrees of freedom
#' y <- chiaprox(1000, v = 10)
#'
#' @export

chiaprox <- function(n, v){
  replicate(n, {
    sum(normaprox(v)^2)
  })
}

