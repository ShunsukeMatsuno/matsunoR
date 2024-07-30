#' For a given panel data, make a balanced panel using a set of variables
#' 
#' @param df A data frame
#' @param key A unique identifier of the panel data, string
#' @param vars A set of variables to be used to make the panel balanced
#'
#' @example examples/make_bal_panel_example.R
#' 
#' @export
#' 
make_bal_panel <- function(df, id, time, 
                           vars){
  vars <- rlang::enquo(vars)
  
  df_bal_vars <- df |> 
    dplyr::select(id, time, 
                  !! vars) |> 
    na.omit() |> 
    BMisc::makeBalancedPanel(idname = id, tname = time) |> 
    tibble::tibble()
  
  # From df, filter observations whose key is in df_bal_vars
  df_key <- df_bal_vars |> dplyr::select(id, time)
  return(dplyr::left_join(df_key, df, by = c(id, time)))
}

df <- tribble(
  ~id, ~t, ~x, ~y, ~z1, ~z2, ~z3,
  1, 1, 1, 1, 1, 1, 1
)
df


