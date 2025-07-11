#' Replaces outliers in a numeric vector with NA.
#'
#' This function replaces outliers in the vector x with NA. Outliers are defined as values
#' that are below the p-th percentile or above the (1-p)-th percentile.
#'
#' @param x A numeric vector.
#' @param p The percentile cutoff to use for defining outliers. Default is 0.01.
#'
#' @return A numeric vector of the same length as x, with outliers replaced by NA.
#'
#' @examples
#' fun.drop_outlier(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
#'
#' @export
#'
fun.drop_outlier <- function(x, p = 0.01) {
  # Compute the p-th and (1-p)-th percentiles
  lower <- quantile(x, p, na.rm = TRUE)
  upper <- quantile(x, 1 - p, na.rm = TRUE)

  # Replace outliers with NA
  x[x < lower | x > upper] <- NA

  return(x)
}
