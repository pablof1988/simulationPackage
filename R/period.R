#' This function detects the period for a random sequence
#' @param x The sequence for detecting the period
#'
#' @returns Returns the period of a sequence
#' @examples
#' sample <- runif(100)
#' period(sample)
#'
#' @export
period <- function(x){
  sum(!duplicated(x))
}
