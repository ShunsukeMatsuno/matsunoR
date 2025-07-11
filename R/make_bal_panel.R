#' For a given panel data, make a balanced panel using a set of variables
#'
#' @param df A data frame
#' @param id A unique identifier of the panel data, string
#' @param time A time variable, string
#' @param vars A set of variables to be used to make the panel balanced
#'
#' @example examples/make_bal_panel_example.R
#'
#' @export
make_bal_panel <- function(df, id, time, vars) {
  vars <- rlang::enquo(vars)

  # Convert "" and strings that trim to "." to NA for all character columns
  df_modified <- df |>
    dplyr::mutate(across(
      where(is.character),
      ~ ifelse(trimws(.) %in% c("", "."),
        NA_character_, .
      )
    ))

  df_complete <- df_modified |>
    dplyr::select(tidyselect::all_of(id), time, !!vars) |>
    na.omit()

  # Make the balanced panel
  df_bal <- df_complete |>
    BMisc::make_balanced_panel(idname = id, tname = time) |>
    tibble::tibble()

  # Check if balanced panel is empty and return error if so
  if (nrow(df_bal) == 0) {
    stop("Error: The balanced panel is empty. Check your input data or filtering conditions.")
  }

  # From df, filter observations whose key is in df_bal
  df_key <- df_bal |>
    dplyr::select(!!rlang::sym(id), !!rlang::sym(time))

  return(dplyr::left_join(df_key, df, by = c(id, time)))
}
