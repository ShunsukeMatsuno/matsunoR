% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_key.R
\name{check_key}
\alias{check_key}
\title{For a dataframe, check if a unique key is actually unique.}
\usage{
check_key(df, key, showall = FALSE)
}
\arguments{
\item{df}{A data frame}

\item{key}{Key to check, e.g., "id", c("id", "year")}

\item{showall}{Logical. If TRUE, returns all columns for duplicate rows. If FALSE (default), returns only the key columns and count.}
}
\value{
If the key is unique, returns a message. If not unique, returns a dataframe of duplicates.
}
\description{
Typically the data frame is a panel data and the identifier is \verb{(year, id)}, but this works for other general cases as well.
The function returns a message indicating whether the key is unique. If not unique, it returns a dataframe showing the duplicates.
}
\examples{
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
}
