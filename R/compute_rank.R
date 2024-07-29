#' Compute the rank of each value in a vector based on quantiles.
#'
#' This function computes the rank of each value in a vector based on quantiles.
#'
#' @param x A numeric vector.
#' @param n The number of quantiles to use for ranking. Default is 10.
#'
#' @return A numeric vector of the same length as x, with each value replaced by its rank.
#'
#' @examples
#' fun.compute_rank(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
#'
#' @note If the length of x is smaller than n, the function will stop and return an error.
#'
#' @export

compute_rank <- function(x, n = 10){

  # return error if x is smaller than n
  if(length(x) <= n){
    stop("The length of x is smaller than n")
  }
  
  pts <- seq(0, 1, by = 1/n)
  cutoffs <- quantile(x, pts, na.rm = TRUE)[2:(n+1)]
  
  # assign rank to each value
  # if x_i <= cutoff[i] then rank_i = i
  x_temp <- x
  x[x_temp <= cutoffs[1]] <- 1
  for(i in 1:(n-1)){
    x[x_temp > cutoffs[i] & x_temp <= cutoffs[i+1]] <- i+1
  }
  return(x)
}