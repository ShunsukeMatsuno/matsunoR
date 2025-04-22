#' Generate a custom grid of values with a concentration effect.
#'
#' This function creates a numeric grid that spans from a specified minimum value to a maximum value,
#' with a specified number of points. A concentration parameter controls how the points are distributed.
#' The grid can be generated with a higher concentration of points around:
#' \itemize{
#'   \item \code{min_val} (by setting \code{center = min_val})
#'   \item \code{max_val} (by setting \code{center = max_val})
#'   \item An arbitrary value between \code{min_val} and \code{max_val} (by setting \code{center} to that value)
#' }
#' For \code{center = min_val} or \code{center = max_val}, a power transformation is used.
#' For an arbitrary \code{center}, the Beta distribution quantile function is used to achieve the desired concentration.
#'
#' @param min_val A numeric value specifying the minimum value of the grid.
#' @param max_val A numeric value specifying the maximum value of the grid.
#' @param length An integer specifying the number of grid points to generate. Default is 100.
#' @param concentration A numeric value controlling the concentration of grid points.
#'   For \code{center = min_val} or \code{center = max_val}, higher values yield more concentration.
#'   For an arbitrary \code{center}, the sum of the Beta distribution parameters is proportional to \code{concentration}.
#'   Default is 2.
#' @param center A numeric value between \code{min_val} and \code{max_val} specifying the value around which
#'   the grid points will be concentrated. Default is \code{min_val}.
#'
#' @return A numeric vector of length \code{n} containing the generated grid values.
#'
#' @examples
#' # Set up parameters
#' min_val <- 0
#' max_val <- 100
#' length <- 100
#' concentration <- 5
#'
#' # Generate grids with different centers
#' grid_min <- make_grid(min_val, max_val, length, concentration, center = min_val)
#' grid_mid <- make_grid(min_val, max_val, length, concentration, center = 70)
#' grid_max <- make_grid(min_val, max_val, length, concentration, center = max_val)
#'
#' # Plotting the grids in separate panels
#' par(mfrow = c(3, 1), mar = c(4, 4, 2, 1))
#'
#' # Grid concentrated near the minimum value
#' plot(grid_min, rep(1, length), type = "b", xlim = c(min_val, max_val), 
#'      xlab = "Grid Values", ylab = "", main = "Concentration around min_val")
#' grid()
#'
#' # Grid concentrated around an arbitrary value (70)
#' plot(grid_mid, rep(1, length), type = "b", xlim = c(min_val, max_val), 
#'      xlab = "Grid Values", ylab = "", main = "Concentration around 70")
#' grid()
#'
#' # Grid concentrated near the maximum value
#' plot(grid_max, rep(1, length), type = "b", xlim = c(min_val, max_val), 
#'      xlab = "Grid Values", ylab = "", main = "Concentration around max_val")
#' grid()
#'
#' @export
make_grid <- function(min_val, max_val, length = 100, concentration = 2, center = min_val) {
  if(center < min_val || center > max_val) {
    stop("center must be between min_val and max_val")
  }
  
  x <- seq(0, 1, length.out = length)
  
  if(center == min_val) {
    # Concentration around min_val: power transformation
    transformed <- x^concentration
  } else if(center == max_val) {
    # Concentration around max_val: reverse power transformation
    transformed <- 1 - (1 - x)^concentration
  } else {
    # For an arbitrary center, use Beta quantile transformation.
    # Normalize center to a value between 0 and 1.
    norm_center <- (center - min_val) / (max_val - min_val)
    # Set shape parameters so that the mode is at norm_center.
    a <- concentration * norm_center + 1
    b <- concentration * (1 - norm_center) + 1
    transformed <- qbeta(x, a, b)
  }
  
  min_val + transformed * (max_val - min_val)
}
