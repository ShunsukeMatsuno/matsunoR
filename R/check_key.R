#' For a dataframe, check if a unique key is actually unique.
#' Typically the data frame is a panel data and the identifier is `(year, id)`, but this works for other general cases as well
#' 
#' 
#' @param df A data frame
#' @param key Key to check, e.g., "id", c("id", "year")
#'
#' @example examples/check_key_example.R 
#' 
#' @export

check_key <- function(df, key){
  # check if key is string or a vector of strings
  if(!is.character(key)){
    stop("key must be a string or a vector of strings")
  }
  
  # check if the key is a subset of the column names of the dataframe
  if(!all(key %in% names(df))){
    stop("key must be a subset of the column names of the dataframe")
  }
  
  # check if the key has at least one element
  if(length(key) == 0){
    stop("key must have at least one element")
  }
  
  # check if the key is unique
  df_duplicates <- df |> 
    dplyr::group_by(across(all_of(key))) |> 
    dplyr::summarize(n = dplyr::n()) |>
    dplyr::filter(n > 1) 
  
  if(NROW(df_duplicates) == 0){
    message("The key is unique")
  } else {
    message("The key is not unique. Here are the duplicates:")
    return(df_duplicates)
  }
}


