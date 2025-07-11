% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/partitions.R
\name{mesh_norm}
\alias{mesh_norm}
\title{Calculate the mesh norm of a partition}
\usage{
mesh_norm(partition)
}
\arguments{
\item{partition}{Partition object created by \code{create_partition}.}
}
\value{
A numeric value representing the mesh norm (maximum interval length).
}
\description{
This function calculates the mesh norm of a partition, which is the maximum
length of any subinterval in the partition. For partitions with infinite
intervals, the mesh norm will be Inf.
}
\examples{
p <- create_partition(list(c(-Inf, 1), c(1, 2), c(2, Inf)))
mesh_norm(p) # Inf (due to infinite intervals)

p2 <- create_partition(list(c(0, 1), c(1, 2), c(2, 3)))
mesh_norm(p2) # 1 (all intervals have length 1)

p3 <- create_partition(list(c(0, 0.5), c(0.5, 1), c(1, 2)))
mesh_norm(p3) # 1 (maximum length is 1)
}
