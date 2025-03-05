#' create a balance panel
#' @param N number of firms
#' @param Time number of periods
#' @param J number of groups (industries)
#' 
#' @export

create_panel <- function(N, Time, J = 1){
  # 
  # N: number of firms 
  # Time: number of periods
  # J: number of groups (industries)
  
  df <- tibble::tibble(
    i = rep(1:N, times = Time),
    t = rep(1:Time, each = N)
    ) %>% 
    dplry::arrange(i, t)
  
  if(J > 1){
    df_list = list()
    for(j in 1:J){
      df_list[[j]] <- df %>% mutate(j = j) 
    }
    df <- dplyr::bind_rows(df_list) %>% 
      dplyr::select(i, j, t) %>% 
      dplyr::arrange(i, t, j)
  }
  return(df)
}