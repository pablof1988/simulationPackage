#' @title Geometric random variable generator
#'
#' @description
#' This function generates random numbers following a geometric distribution
#' using direct simulation. For each observation, independent trials are repeated
#' until the first success occurs, counting the number of trials required.
#'
#' @usage
#' geom(n, p)
#'
#' @param n Number of random values to generate.
#' @param p Probability of success in each trial (0 < p < 1).
#'
#' @return
#' A numeric vector of length n containing the number of trials required
#' to obtain the first success in each simulation.
#'
#' @references
#' Ross, S. M. (2014). Introduction to Probability Models.
#' Devore, J. L. (2012). Probability and Statistics for Engineering and the Sciences.
#'
#' @examples
#' # Generate 10 values with success probability 0.3
#' # geom(10,0.3)
#' @export

geom <- function(n, p){
  resultado <- c()

  for(i in 1:n){
    k <- 1

    while(runif(1) > p){
      k <- k + 1
    }

    resultado <- c(resultado, k)
  }

  return(resultado)
}

