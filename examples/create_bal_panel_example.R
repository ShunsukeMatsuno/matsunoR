\donttest{
df <- tibble::tribble(
  ~ id, ~ year, ~ x, ~ y, ~ z,
  1, 2000, 1, 2, NA_real_,
  1, 2001, 1, 2, 3,
  2, 2000, 1, NA_real_, 3,
  2, 2001, 1, 2, 3
)

create_bal_panel(df, "id", "year", c(x))
create_bal_panel(df, "id", "year", c(x, y))
}