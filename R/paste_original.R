#' Concatenate two strings
#' Can take an expression in argument: (1+2) %+% " unit" returns "3 unit"
#' @export

`%+%` <- function(s1, s2) {
  # Capture unevaluated expressions
  s1_expr <- substitute(s1)
  s2_expr <- substitute(s2)
  
  # Evaluate the expressions
  s1_val <- eval(s1_expr)
  s2_val <- eval(s2_expr)
  
  # Concatenate the values
  paste0(as.character(s1_val), as.character(s2_val))
}

