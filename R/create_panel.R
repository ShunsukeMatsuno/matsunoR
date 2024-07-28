create_panel <- function(N, Time, J = 1){
  # create a balance panel
  # N: number of firms 
  # Time: number of periods
  # J: number of groups (industries)
  
  df <- tibble::tibble(
    i = rep(1:N, times = Time),
    t = rep(1:Time, each = N)
    ) %>% 
    arrange(i, t)
  
  if(J > 1){
    df_list = list()
    for(j in 1:J){
      df_list[[j]] <- df %>% mutate(j = j) 
    }
    df <- bind_rows(df_list) %>% 
      select(i, j, t) %>% 
      arrange(i, t, j)
  }
  

  
  return(df)
}