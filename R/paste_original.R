#' String concatenation operator with expression evaluation
#' 
#' This infix operator concatenates two strings after evaluating any expressions
#' in the arguments. It's useful for building strings that include computed values.
#' 
#' @param s1 First string or expression to concatenate
#' @param s2 Second string or expression to concatenate
#' 
#' @return A character string containing the concatenated result
#' 
#' @details
#' The operator evaluates both arguments as expressions before converting them
#' to characters and concatenating. This allows for dynamic string construction
#' with computed values.
#' 
#' @examples
#' # Basic string concatenation
#' "Hello" %+% " World"  # Returns "Hello World"
#' 
#' # With expression evaluation
#' (1 + 2) %+% " units"  # Returns "3 units"
#' 
#' # With variables
#' x <- 5
#' y <- 10
#' "Result: " %+% (x + y)  # Returns "Result: 15"
#' 
#' # Multiple concatenations
#' "Value " %+% x %+% " is " %+% (x > 3)  # Returns "Value 5 is TRUE"
#' 
#' @note This operator is not exported by default as it may conflict with 
#'   ggplot2's %+% operator. Use matsunoR::`%+%` if needed when both packages are loaded.
#' 
# @export  # Commented out to avoid conflicts with ggplot2

`%+%` <- function(s1, s2) {
  # Capture unevaluated expressions to allow for dynamic evaluation
  s1_expr <- substitute(s1)
  s2_expr <- substitute(s2)
  
  # Evaluate the expressions in the calling environment
  s1_val <- eval(s1_expr, envir = parent.frame())
  s2_val <- eval(s2_expr, envir = parent.frame())
  
  # Convert to character and concatenate
  paste0(as.character(s1_val), as.character(s2_val))
}

