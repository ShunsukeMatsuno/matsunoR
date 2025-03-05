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
  
  df_complete <- df |> 
    dplyr::select(tidyselect::all_of(id), time, !! vars) |> 
    na.omit()
  
  # Make the balanced panel
  df_bal <- df_complete |>
    BMisc::make_balanced_panel(idname = id, tname = time) |> 
    tibble::tibble()
  
  result <- dplyr::semi_join(df_bal, df, by = c(id, time))
  
  # df_bal_vars <- df |> 
  #   dplyr::select(id, time, 
  #                 !! vars) |> 
  #   na.omit() |> 
  #   BMisc::makeBalancedPanel(idname = id, tname = time) |> 
  #   tibble::tibble()
  
  # Get the complete cases for the variables of interest
  # # From df, filter observations whose key is in df_bal_vars
  # df_key <- df_bal_vars |> dplyr::select(id, time)
  # result <- dplyr::left_join(df_key, df, by = c(id, time))
  
  
  return(result)
}


# pacman::p_load(tidyverse, ggplot2)
# # test
# setwd("C:/Users/Matsuno/CBS Dropbox/Shunsuke Matsuno/02_research/03_Innovation/Disaggregation and Proprietary cost/3. Data and Codes/Code/InnovationR")
# path_patent <- "../../Data/03_Create_full_sample/citation_new/full_sample_unbal_patent-level_c.dta"
# 
# df_raw <- haven::read_dta(file = path_patent)
# df_raw <- df_raw |> 
#   filter(fyear <= 2021)
# df_pfy <- df_raw |> 
#   select(permno, gvkey, fyear, conm, gyear = patent_grant_year,
#          at, capx, sale, MTB, cash_holding, ppe, roa, size, Disagg_firm, Post2015, lev,
#          starts_with("rd"),
#          naicsh1, sich1, naics2, sic2, unique_Tag_count, all_Tag_freq,
#          num_of_patent, log_numpatent, avg_citation,
#          patent_filing_year, patent_grant_year, patent_num, cpc_class, cites, cohort_total_cites, cohort_avg_cites, cit_scaled_by_avg, cit_scaled_by_total, xi_nominal, xi_real) |> 
#   mutate(MTB = MTB/1000) 
# 
# df_fy <- df_pfy |> 
#   group_by(gvkey, fyear) |> 
#   summarise(across(c(permno, conm, gyear, naicsh1, sich1, naics2, sic2, unique_Tag_count, all_Tag_freq), ~first(.x)),
#             avg_cite_raw = if_else(!is.na(first(cites)), mean(cites, na.rm = TRUE), 0),
#             avg_cite_scaled_by_avg = if_else(!is.na(first(cites)), mean(cit_scaled_by_avg, na.rm = TRUE), 0),
#             avg_cite_scaled_by_total = if_else(!is.na(first(cites)), mean(cit_scaled_by_total, na.rm = TRUE), 0),
#             total_num_of_patent = if_else(!is.na(first(patent_num)), n(), 0),     # if there is patent, then count the total number of records
#             unique_Tag_count = if_else(!is.na(first(unique_Tag_count)), first(unique_Tag_count), 0),
#             all_Tag_freq = if_else(!is.na(first(all_Tag_freq)), first(all_Tag_freq), 0),
#             across(c(at, capx, sale, MTB, cash_holding, ppe, roa, size, Disagg_firm, Post2015, lev, num_of_patent, log_numpatent, avg_citation, 
#                      starts_with("rd")), ~first(.x)),
#             .groups = "drop")
# 
# df_fy_bal <- df_fy |> 
#   make_bal_panel(id = "permno", time = "fyear",
#                  vars =c(gvkey, fyear, permno, conm, naicsh1, sich1, naics2, sic2,
#                          starts_with("avg_"), total_num_of_patent, at, capx, sale, MTB, cash_holding,
#                          ppe, roa, size, Disagg_firm, Post2015, lev, num_of_patent, log_numpatent))
# 
# df_fy_bal |> 
#   group_by(fyear) |> 
#   summarise(count = n()) 
