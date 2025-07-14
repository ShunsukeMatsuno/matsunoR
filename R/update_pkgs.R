#' Update all packages
#'
#' This function checks for package updates, lists them, and asks for user confirmation.
#'
#' @param exclude_pkg Character vector of package names to exclude from updates
#' @export
update_pkgs <- function(exclude_pkg = "arrow") {
  cat("Checking for package updates...\n")
  
  # Get outdated packages
  outdated <- old.packages()
  
  if (is.null(outdated)) {
    cat("All packages are up to date.\n")
    return(invisible())
  }
  
  # Convert to data frame and exclude specified packages
  outdated_df <- as.data.frame(outdated)
  outdated_df <- outdated_df[!outdated_df$Package %in% exclude_pkg, ]
  
  if (nrow(outdated_df) == 0) {
    cat("All packages are up to date (excluding:", paste(exclude_pkg, collapse = ", "), ").\n")
    return(invisible())
  }
  
  # Display packages to be updated
  cat("\nThe following packages can be updated:\n")
  cat("Package", sprintf("%15s", "Installed"), sprintf("%15s", "Available"), "\n")
  cat(rep("-", 50), "\n", sep = "")
  
  for (i in 1:nrow(outdated_df)) {
    cat(sprintf("%-20s %10s %15s\n", 
                outdated_df$Package[i],
                outdated_df$Installed[i], 
                outdated_df$ReposVer[i]))
  }
  
  cat("\n")
  
  # Ask for confirmation
  response <- readline(prompt = "Do you want to update these packages? [Y/n]: ")
  
  # Default to "Y" if empty response
  if (response == "" || tolower(response) == "y" || tolower(response) == "yes") {
    cat("Updating packages...\n")
    update.packages(ask = FALSE, build = TRUE, oldPkgs = outdated_df$Package)
    cat("Package updates completed.\n")
  } else {
    cat("Package update cancelled.\n")
  }
}
