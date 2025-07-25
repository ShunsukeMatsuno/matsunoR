% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/partitions.R
\name{which_interval}
\alias{which_interval}
\alias{get_interval}
\title{Get which interval a number falls into}
\usage{
which_interval(x, partition)

get_interval(x, partition)
}
\arguments{
\item{x}{Number to check. Must be a single numeric value.}

\item{par}{|> tition Partition object created by \code{create_partition}.}
}
\value{
A list with two elements:
\item{index}{The index of the interval in the partition that contains x, or NA if not found}
\item{interval}{The interval itself as a vector of two numbers, or NA if not found}
}
\description{
This function determines which interval in a partition a given number belongs to.
}
\details{
The function checks if x falls within any interval in the partition.
For an interval [a, b), x is considered to be in the interval if a <= x < b.
}
\examples{
p <- create_partition(list(c(-Inf, 1), c(1, 2), c(2, Inf)))
which_interval(0.5, p) # list(index = 1, interval = c(-Inf, 1))
which_interval(1.5, p) # list(index = 2, interval = c(1, 2))
which_interval(2.5, p) # list(index = 3, interval = c(2, Inf))
}
