#' Preprocess data to handle discontinuities
#'
#' This function introduces `NA` in specified variables (`...`) of a data frame
#' wherever the absolute difference between consecutive values exceeds a threshold.
#'
#' @param data A data frame containing the variables to process.
#' @param ... Unquoted variable names to process for discontinuities.
#' @param threshold A numeric value specifying the threshold for detecting discontinuities.
#' @return A data frame with `NA` introduced in the specified variables where discontinuities are detected.
#' @examples
#' x <- seq(-10, 10, length.out = 500)
#' y <- ifelse(x < 0, sin(x), cos(x)) # Artificial discontinuity at x = 0
#' z <- ifelse(x < 0, cos(x), sin(x)) # Another artificial discontinuity
#' data <- data.frame(x = x, y = y, z = z)
#' 
#' data <- preprocess_discontinuity(data, vars = c("y", "z"), threshold = 0.5)
#' head(data)
#' @export
preprocess_discontinuity <- function(data, ..., threshold = 1e-2) {
  
  # Validate input data
  if (!is.data.frame(data)) stop("Input must be a data frame.")
  
  # Capture the variable names from `...`
  vars <- rlang::enquos(...)
  
  # Ensure all variables are present in the data frame
  vars_names <- purrr::map_chr(vars, rlang::as_name)
  if (!all(vars_names %in% colnames(data))) {
    stop("All specified variables must exist in the data frame.")
  }
  
  # Process each specified variable
  for (var in vars_names) {
    data[[var]][abs(diff(c(NA, data[[var]]))) > threshold] <- NA
  }
  
  return(data)
}

