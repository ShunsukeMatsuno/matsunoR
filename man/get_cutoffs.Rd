% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/partitions.R
\name{get_cutoffs}
\alias{get_cutoffs}
\title{Get the cutoffs of a partition}
\usage{
get_cutoffs(partition)
}
\arguments{
\item{partition}{Partition object created by \code{create_partition}.}
}
\value{
A numeric vector containing the cutoffs of the partition.
}
\description{
This function returns the cutoffs of a given partition. If the partition's endpoints are infinite, then the cutoffs exclude these points.
If the endpoints are finite, then the cutoffs include these points.
}
\examples{
p <- create_partition(list(c(-Inf, 1), c(1, 2), c(2, Inf)))
get_cutoffs(p)  # c(1, 2)

p2 <- create_partition(list(c(0, 1), c(1, 2), c(2, 3)))
get_cutoffs(p2)  # c(0, 1, 2, 3)
}
