#' Create a grid of values, optionally concentrating around one or more points.
#'
#' If \code{concentration_points} is NULL or empty, returns an even grid.
#' Otherwise we build a simple mixture density: 1 everywhere plus Gaussian bumps
#' at each concentration point (bandwidth = 10% of the total range),
#' with relative height given by \code{strength}.  We then warp the uniform grid
#' by the CDF of that density so points cluster around the specified values.
#'
#' @param min_val Numeric scalar.  Lower bound of the grid.
#' @param max_val Numeric scalar.  Upper bound of the grid.
#' @param length Integer.  Number of points in the returned grid.  Default 100.
#' @param concentration_points Numeric vector of values in [min_val, max_val]
#'   where the grid should cluster.  Default NULL.
#' @param strength Numeric scalar ≥ 0.  Height of the Gaussians at each
#'   concentration point, relative to the uniform baseline.  Larger → more pull.
#'   If 0, the grid is uniform. Default 1.
#'
#' @return A numeric vector of length \code{length}.
#'
#' @examples
#' # Set up parameters
#' min_val <- -1
#' max_val <- 5
#' length <- 100
#'
#' # Uniform grid
#' grid_uniform <- make_grid(min_val, max_val, length)
#'
#' # Grid concentrated around 2 and 4
#' grid_2_4 <- make_grid(
#'   min_val,
#'   max_val,
#'   length,
#'   concentration_points = c(2, 4),
#'   strength = 5
#' )
#'
#' # Plotting the grids in separate panels
#' par(mfrow = c(2, 1), mar = c(4, 4, 2, 1))
#'
#' # Uniform grid
#' plot(
#'   grid_uniform, rep(1, length),
#'   type = "b", xlim = c(min_val, max_val),
#'   xlab = "Grid Values", ylab = "",
#'   main = "Uniform Grid"
#' )
#' grid()
#'
#' # Grid concentrated around 2 and 4
#' plot(
#'   grid_2_4, rep(1, length),
#'   type = "b", xlim = c(min_val, max_val),
#'   xlab = "Grid Values", ylab = "",
#'   main = "Concentration around 2 and 4"
#' )
#' grid()
#'
#' @export
create_grid <- function(min_val, max_val,
                        length = 100,
                        concentration_points = NULL,
                        strength = 1) {
  # basic checks
  if (!is.numeric(min_val) || !is.numeric(max_val) || min_val >= max_val) {
    stop("`min_val` and `max_val` must be numeric with min_val < max_val")
  }
  if (!is.numeric(length) || length < 2) {
    stop("`length` must be an integer ≥ 2")
  }
  if (!is.null(concentration_points)) {
    if (any(concentration_points < min_val | concentration_points > max_val)) {
      stop("All `concentration_points` must lie in [min_val, max_val]")
    }
  }
  # uniform fallback
  if (is.null(concentration_points) || length(concentration_points) == 0) {
    return(seq(min_val, max_val, length.out = length))
  }
  # build raw support and density
  raw_x <- seq(min_val, max_val, length.out = length)
  dens <- rep(1, length)
  bw <- (max_val - min_val) / 10
  for (cpt in concentration_points) {
    dens <- dens + strength * dnorm(raw_x, mean = cpt, sd = bw)
  }
  # compute normalized CDF
  cum_dens <- cumsum(dens)
  F <- (cum_dens - cum_dens[1]) / (cum_dens[length] - cum_dens[1])
  # invert by interpolation
  u <- seq(0, 1, length.out = length)
  grid <- approx(F, raw_x, xout = u, ties = "ordered")$y
  return(grid)
}


#' @title Deprecated Alias for create_grid
#' @description This function is an alias for \code{\link{create_grid}} and will be removed in a future version.
#' @param ... Arguments passed to \code{\link{create_grid}}.
#' @seealso \code{\link{create_grid}}
#' @export
#' @aliases create_grid
make_grid <- function(...) {
  # Issue a one-time warning
  if (!exists(".make_grid_warned", envir = .GlobalEnv)) {
    warning("The function 'make_grid' is deprecated and will be removed in a future version. Please use 'create_grid' instead. This warning will only be shown once.", call. = FALSE)
    assign(".make_grid_warned", TRUE, envir = .GlobalEnv)
  }
  create_grid(...)
}
