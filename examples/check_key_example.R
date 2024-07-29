# A data frame with a unique key (id, year)
df <- tibble::tribble(
  ~id, ~year, ~value,
  1, 2000, 10,
  1, 2001, 20,
  2, 2000, 30,
  2, 2001, 40,
)
check_key(df, c("id", "year"))

# Now, make it non-unique
df <- dplyr::bind_rows(df, tibble::tribble(
  ~id, ~year, ~value,
  2, 2001, 50
))
check_key(df, c("id", "year"))