#' Update all packages
#'
#' This function updates all packages except for `arrow`.
#'
#' @export
update_pkgs <- function(exclude_pkg = "arrow") {
  # Get list of installed packages
  installed <- installed.packages()[, "Package"]
  
  # Exclude 'arrow' package
  pkgs_to_update <- setdiff(installed, "arrow")
  
  # Update the rest
  update.packages(ask = FALSE, build = TRUE, oldPkgs = pkgs_to_update)
}