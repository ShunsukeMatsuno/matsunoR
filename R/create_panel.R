#' Create a balanced panel dataset
#'
#' This function creates a balanced panel dataset with specified numbers of firms,
#' time periods, and optionally groups (industries). All firms are observed in
#' all time periods.
#'
#' @param N Integer. Number of firms (cross-sectional units).
#' @param Time Integer. Number of time periods.
#' @param J Integer. Number of groups (industries). Default is 1.
#'
#' @return A data frame with columns:
#'   \item{i}{Firm identifier (1 to N)}
#'   \item{t}{Time period identifier (1 to Time)}
#'   \item{j}{Group identifier (1 to J), only included if J > 1}
#'
#' @details
#' The function creates a balanced panel where every firm appears in every time period.
#' When J > 1, the panel is replicated for each group, creating J separate groups
#' of N firms each observed over Time periods.
#'
#' @examples
#' # Create a simple panel with 3 firms over 4 time periods
#' panel_simple <- create_panel(N = 3, Time = 4)
#' head(panel_simple)
#'
#' # Create a panel with 2 firms, 3 time periods, and 2 groups
#' panel_groups <- create_panel(N = 2, Time = 3, J = 2)
#' head(panel_groups)
#'
#' # Check the structure
#' nrow(panel_groups) # Should be 2 * 3 * 2 = 12 observations
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr arrange mutate bind_rows select
#' @export
create_panel <- function(N, Time, J = 1) {
  # Input validation
  if (!is.numeric(N) || N <= 0 || N != as.integer(N)) {
    stop("N must be a positive integer")
  }
  if (!is.numeric(Time) || Time <= 0 || Time != as.integer(Time)) {
    stop("Time must be a positive integer")
  }
  if (!is.numeric(J) || J <= 0 || J != as.integer(J)) {
    stop("J must be a positive integer")
  }

  # Create basic panel structure
  df <- tibble::tibble(
    i = rep(1:N, times = Time),
    t = rep(1:Time, each = N)
  ) %>%
    dplyr::arrange(i, t)

  # Add groups if J > 1
  if (J > 1) {
    df_list <- list()
    for (j in 1:J) {
      df_list[[j]] <- df %>% dplyr::mutate(j = j)
    }
    df <- dplyr::bind_rows(df_list) %>%
      dplyr::select(i, j, t) %>%
      dplyr::arrange(i, t, j)
  }

  return(df)
}
