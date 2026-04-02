#'
#' @title Linear Congruential Generator
#'
#' @description
#' Implements a linear congruential generator to create a sequence of pseudo-random numbers from an uniform distribution U(0, 1).
#'
#' @param n Integer. Sequence length.
#' @param seed Integer. Initial value that determines the starting point of the sequence.
#' @param a Integer. The multiplier used in the generator.
#' @param c Integer, The additive constant. Default is 0.
#' @param m Integer. The modulus used in the generator.
#'
#' @details
#' The linear congruential generator is one of the oldest and best-known algorithms for generating pseudo-random numbers.
#' The quality of the generated sequence depends on the appropriate choice of parameters \code{a}, \code{c}, and \code{m}.
#'
#' The formula used is: \deqn{x_{i+1} = (a x_i + c) \mod m}
#'
#' @return
#' A numeric vector of length \code{n} containing the sequence of the uniform pseudo-random numbers.
#'
#' @examples
#' # Generate a sequence with arbitrary parameters(bad choose)
#' congGenerator(n = 100, seed = 12, a = 64, c = 11, m = 7)
#'
#' # Generate another sequence without an additive constant
#' congGenerator(n = 100, seed = 2003, a = 45, m = 2^7)
#'
#' # Evaluate the quality of a sequence
#' congGenerator(n = 100, seed = 1610, a = 21, c = 25, m = 2^10)
#'
#'
#' @export
congGenerator <- function(n, a, c = 0, m, seed){
  replicate(n, {
    seed <<- ((a * seed) + c) %% m
    seed/m
  })
}

