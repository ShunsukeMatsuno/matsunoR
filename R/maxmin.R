#' Censor values within the range \eqn{[0, 1]}
#'
#' This function ensures that each element in `x` is constrained within the interval \eqn{[0, 1]}.
#' If `x` is a scalar or vector, values less than 0 are replaced with 0, and values greater than 1
#' are replaced with 1.
#'
#' @param x A numeric value or numeric vector.
#' @return A numeric value or vector with all elements censored within \eqn{[0, 1]}.
#' @examples
#' maxmin(0.5) # Returns 0.5
#' maxmin(-0.2) # Returns 0
#' maxmin(1.5) # Returns 1
#' maxmin(c(-0.5, 0.2, 1.3)) # Returns c(0, 0.2, 1)
#' @export
maxmin <- function(x) {
  return(pmax(pmin(x, 1, na.rm = TRUE), 0, na.rm = TRUE))
}
