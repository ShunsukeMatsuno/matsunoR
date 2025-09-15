#' Measure and pretty-print execution time
#'
#' Measures user, system, and elapsed time for an expression, prints a
#' compact human-readable summary, and invisibly returns the raw timings.
#'
#' @param expr Expression to evaluate. Wrap in braces `{}` for multiple statements.
#' @param s_digits Integer. Decimal places when formatting seconds. Default is 2.
#' @param day_digits Integer. Decimal places when formatting days. Default is 1.
#'
#' @return Invisibly returns a list with elements:
#'   \itemize{
#'     \item{times_sec}{Named numeric vector `c(user, system, elapsed)` in seconds}
#'     \item{display}{Data frame printed to console with human-readable times}
#'     \item{speedup}{`user / elapsed` ratio (`Inf` if user time is zero)}
#'   }
#'
#' @details
#' This is a light wrapper around \code{base::system.time()}. Durations are
#' formatted as milliseconds when < 1s, seconds up to a minute, minutes and
#' seconds up to an hour, hours and minutes up to a day, and days otherwise.
#'
#' @examples
#' # simple example
#' exec_time(runif(1e7) |> mad())
#'
#' # checking speedup (parallel) -- heavy; not run on CRAN
#' \dontrun{
#' options(parallelly.fork.enable = TRUE)
#'
#' # function to test
#' f <- function(n) mean(rt(1e6, df = 4))
#' # set up parallel plan
#' future::plan(future::multicore, workers = 4)
#' # time the function with and without parallel
#' exec_time(furrr::future_map(1:8, f, .options = furrr::furrr_options(seed = TRUE)))
#' exec_time(purrr::map(1:8, f))
#' # reset parallel plan
#' future::plan(future::sequential)
#' }
#'
#' @seealso base::system.time
#' @export
exec_time <- function(expr, s_digits = 2, day_digits = 1) {
  t <- system.time(expr)

  # Extract raw timings (in seconds)
  time_user    <- unname(t[[1]] + t[[4]])
  time_system  <- unname(t[[2]] + t[[5]])
  time_elapsed <- unname(t[[3]])

  # Helper to format durations into human-readable strings
  fmt_time <- function(sec) {
    if (is.na(sec)) return(NA_character_)
    if (sec < 1) {
      sprintf("%d ms", round(sec * 1000))
    } else if (sec <= 60) {
      sprintf(paste0("%.", s_digits, "f s"), round(sec, s_digits))
    } else if (sec < 3600) {
      m <- floor(sec / 60)
      s <- round(sec - m * 60)
      if (s == 60) { m <- m + 1; s <- 0 }
      sprintf("%d min %d seconds", m, s)
    } else if (sec < 86400) {
      h <- floor(sec / 3600)
      m <- round((sec - h * 3600) / 60)
      if (m == 60) { h <- h + 1; m <- 0 }
      sprintf("%d hour %d min", h, m)
    } else {
      d <- round(sec / 86400, day_digits)
      sprintf(paste0("%.", day_digits, "f days"), d)
    }
  }

  # Table for display
  df <- data.frame(
    metric = c("system", "user", "elapsed"),
    time   = c(fmt_time(time_system), fmt_time(time_user), fmt_time(time_elapsed)),
    stringsAsFactors = FALSE
  )

  # Ratio of user CPU seconds to elapsed wall seconds
  ratio_user_elapsed <- if (isTRUE(time_elapsed > 0)) time_user / time_elapsed else Inf

  # Print a compact summary as an ASCII table
  w_metric <- max(nchar(c("metric", df$metric), type = "width"))
  w_time   <- max(nchar(c("time",   df$time),   type = "width"))
  header   <- sprintf("%-*s  %-*s", w_metric, "metric", w_time, "time")
  ruler    <- paste0(strrep("-", w_metric), "  ", strrep("-", w_time))
  cat(header, "\n", ruler, "\n", sep = "")
  for (i in seq_len(nrow(df))) {
    cat(sprintf("%-*s  %-*s\n", w_metric, df$metric[i], w_time, df$time[i]))
  }
  cat(sprintf("speedup: %s x\n",
              if (is.finite(ratio_user_elapsed)) format(round(ratio_user_elapsed, s_digits), trim = TRUE) else "Inf"))

  # Return detailed values invisibly for programmatic use
  invisible(list(times_sec = c(user = time_user, system = time_system, elapsed = time_elapsed),
                 display = df, speedup = ratio_user_elapsed))
}

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
    # original package
    try(remotes::install_github("ShunsukeMatsuno/matsunoR", upgrade = "never"))
    # update other packages
    update.packages(ask = FALSE, build = TRUE, oldPkgs = outdated_df$Package)
    cat("Package updates completed.\n")
  } else {
    cat("Package update cancelled.\n")
  }
}
