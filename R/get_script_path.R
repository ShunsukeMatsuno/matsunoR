#' Get the directory path of the current file
#' 
#' This function returns the directory path of the current R script being executed.
#' It works both when the script is run from the command line (using Rscript) and
#' when run interactively in RStudio.
#' 
#' @return A character string containing the directory path of the current file
#' @examples
#' \dontrun{
#' # Get the directory of the current file
#' current_dir <- get_script_path()
#' }
#' @export
get_script_path <- function(){
  this_file <- grep("^--file=", commandArgs(), value = TRUE)
  
  this_file <- gsub("^--file=", "", this_file)
  
  if (length(this_file) == 0) this_file <- rstudioapi::getSourceEditorContext()$path
  
  return(dirname(this_file))
}