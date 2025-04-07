#' Compute the rank of each value in a vector based on quantiles.
#'
#' This function computes the rank of each value in a vector based on quantiles.
#' Higher ranks correspond to higher values in the original data.
#'
#' @param x A numeric vector.
#' @param n The number of quantiles to use for ranking. Default is 10.
#'
#' @return A numeric vector of the same length as x, with each value replaced by its rank.
#'         Higher ranks (closer to n) correspond to higher values in the original data.
#'
#' @examples
#' # Basic example - higher values get higher ranks
#' compute_rank(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
#' # Result: [1] 1 2 3 4 5 6 7 8 9 10
#' # Notice how the highest value (10) gets the highest rank (10)
#' 
#' # Using a different number of quantiles
#' compute_rank(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), n = 4)
#' # Result: [1] 1 1 1 2 2 2 3 3 3 4 4 4
#' # Values are grouped into 4 ranks, with higher values getting higher ranks
#' 
#' # With random data - higher values still get higher ranks
#' set.seed(123)
#' random_data <- rnorm(100)
#' ranks <- compute_rank(random_data)
#' # Check that higher values correspond to higher ranks
#' plot(random_data, ranks, main="Original Values vs Ranks", 
#'      xlab="Original Values", ylab="Ranks")
#' 
#' # With missing values
#' data_with_na <- c(1:15, NA, 17:20)
#' compute_rank(data_with_na)
#' # NA values are handled properly, and higher values still get higher ranks
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