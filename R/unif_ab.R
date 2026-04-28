#' Generate Random Numbers from a Uniform Distribution
#'
#' This function generates \code{n} random numbers from a continuous
#' uniform distribution on the interval \eqn{[a, b]} using a base
#' uniform generator \code{unifr()}.
#'
#' @param n Integer. Number of random observations to generate.
#' @param a Numeric. Lower bound of the distribution.
#' @param b Numeric. Upper bound of the distribution.
#'
#' @return A numeric vector of length \code{n} containing random values
#' drawn from a uniform distribution on \eqn{[a, b]}.
#'
#' @details
#' The function first generates random values \eqn{U ~ Uniform(0,1)}
#' using \code{unifr()}, and then applies the transformation:
#' \deqn{X = a + U (b - a)}
#' to obtain values distributed as \eqn{Uniform(a, b)}.
#'
#' @examples
#' # Generate 100 random values between 2 and 10
#' sample <- unif(100, a = 2, b = 10)
#'
#' # Plot a histogram of the generated values
#' hist(sample, main = "Uniform(2,10)", col = "lightblue")
#'
#' @seealso \code{\link{runif}}
#'
#' @export
unif_ab <- function(n, a, b){
  u <- unifr(n)
  a + (u * (b - a))
}

#' @examples
#' set.seed(123)
#' sample <- unif_ab(100, a = 2, b = 10)
#' hist(sample, main = "Uniform(2,10)", col = "lightblue")
