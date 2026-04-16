#' @title Uniform Distribution Pseudorandom Number Generator
#'
#' @description
#' Generates pseudorandom numbers from a uniform distribution on the interval (a, b).
#'
#' @param n Integer. Number of pseudorandom values to generate.
#' @param a Numeric. Lower bound of the uniform distribution.
#' @param b Numeric. Upper bound of the uniform distribution.
#'
#' @details
#' Transforms standard uniform (0,1) pseudorandom numbers from \code{unifr()} to a
#' uniform distribution on the interval \eqn{(a, b)} using the transformation:
#' \deqn{X = a + U \times (b - a)}
#' where \eqn{U \sim \text{Uniform}(0,1)}.
#'
#' @return
#'
#' @examples
#' # Generate 5 uniform numbers between 0 and 10
#' unif(5, 0, 10)
#'
#' @export
unif <- function(n, a, b){
  u <- unifr(n)
  a + (u * (b - a))
}
