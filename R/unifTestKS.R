#' This function implements the goodness of fit Kolmogorov-Smirnov test to prove if a sample comes from a uniform distribution U(0, 1)
#'
#'@param x The sample to be tested
#'
#'@return Returns the D statistic and the p-value for the goodness of fit test
#'
#' @details
#' This function contrasts the hypotheses \eqn{H_0: F_n(x) = F(x)} vs \eqn{H_0: F_n(x) \neq F(x)}. Where \eqn{F_n(x)} is the empirical cumulate function distribution and \eqn{F(x)} is the theoretical uniform cumulate distribution function.
#'
#' @examples
#' sample <- runif(100)
#' unifTestKS(sample)
#'
#' @importFrom stats punif
#'
#' @export
unifTestKS <- function(x){
  n <- length(x)
  x <- sort(x)

  Fn <- (1:n)/n
  Fx <- punif(x, 0, 1)
  D <- max(abs(Fn - Fx))

  z <- sqrt(n) * D
  j <- 1:100
  s <- sum((-1)^(j-1) * exp(-2 * (j^2) * (z^2)))
  s2 <- 1 - 2 * s
  p_valor <- 1 - s2

  return(c(
    "D" = D,
    "P-valor" = p_valor
  ))
}
