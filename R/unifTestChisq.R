#' This function implements the goodness of fit chi-square test to prove if a sample comes from a uniform distribution U(0, 1)
#'
#' @param x The sample to be tested
#'
#' @return Returns the chi square statistic, the degree of freedom and the p-value for the goodness of fit test
#'
#' @details
#' This function contrast the hypotheses \eqn{H_0: \pi = \hat{\pi}} Vs. \eqn{H_1: \pi \neq \hat{\pi}}. To facilitate the compute, we always use a partition of 10 intervals to compute the frequencies in the sample, as to compute the theoretical probabilities.
#'
#' @examples
#' sample <- runif(100)
#' unifTestChisq(sample)
#'
#' @import fdth
#'
#' @importFrom stats pchisq
#'
#' @export
unifTestChisq <- function(x){
  n <- length(x)

  phi <- rep(0.1, 10)
  nphi <- n * phi

  tfr <- fdt(x, start = 0, end = 1, h = 0.1)
  nphi_est <- tfr$table$f

  x2 <- sum(((nphi_est - nphi)^2) / nphi)
  df <- length(phi) - 1

  pval <- pchisq(q = x2, df = df, lower.tail = F)

  c("statistic" = x2, "df" = df, "p.val" = pval)
}
