#' Create a partition of the real line
#' 
#' This function creates a partition of the real line by dividing it into non-overlapping intervals.
#' The intervals must be contiguous (no holes) and non-overlapping.
#' 
#' @param intervals List of intervals, where each interval is a vector of 2 numbers.
#'   Each interval is of the form [a, b) where a is included and b is excluded,
#'   except for the last interval if it ends with Inf.
#'   
#' @return A partition object with class "partition" containing the sorted intervals.
#' @usage
#' intervals <- list(c(-Inf, 1), c(1, 2), c(2, Inf))
#' create_partition(intervals)
#' 
#' @details 
#' The function performs several validations:
#' \itemize{
#'   \item Checks that intervals is a non-empty list
#'   \item Validates that each interval is a numeric vector of length 2
#'   \item Ensures the first element of each interval is less than the second
#'   \item Sorts intervals by their lower bounds
#'   \item Checks for overlaps between adjacent intervals
#'   \item Checks for holes between adjacent intervals
#' }
#' 
#' @example examples/intervals_example.R
#' @export
create_partition <- function(intervals) {
  if (!is.list(intervals) || length(intervals) == 0) {
    stop("Intervals must be a non-empty list.")
  }
  # Validate each interval
  for (interval in intervals) {
    if (!is.numeric(interval) || length(interval) != 2) {
      stop("Each interval must be a numeric vector of length 2.")
    }
    if (interval[1] >= interval[2]) {
      stop("Each interval must have its first element less than its second element.")
    }
  }
  
  # if intervals has only one element (no partition), then return the interval
  if (length(intervals) == 1) {
    return(structure(list(intervals = intervals), class = "partition"))
  }
  
  # Sort intervals by their lower bounds
  sorted_intervals <- intervals[order(sapply(intervals, function(x) x[1]))]
  
  # Check for overlaps and holes
  for (i in 1:(length(sorted_intervals) - 1)) {
    current <- sorted_intervals[[i]]
    next_interval <- sorted_intervals[[i + 1]]
    
    # Check for overlap
    if (current[2] > next_interval[1]) {
      stop("Intervals must not overlap. Found overlap between [", 
           current[1], ", ", current[2], "] and [", 
           next_interval[1], ", ", next_interval[2], "].")
    }
    
    # Check for holes
    if (current[2] != next_interval[1]) {
      stop("Intervals must not have holes. Found hole between [", 
           current[1], ", ", current[2], "] and [", 
           next_interval[1], ", ", next_interval[2], "].")
    }
  }
  
  structure(list(intervals = sorted_intervals), class = "partition")
}

#' Get which interval a number falls into
#' 
#' This function determines which interval in a partition a given number belongs to.
#' 
#' @param x Number to check. Must be a single numeric value.
#' @param par |> tition Partition object created by \code{create_partition}.
#' 
#' @return A list with two elements:
#'   \item{index}{The index of the interval in the partition that contains x, or NA if not found}
#'   \item{interval}{The interval itself as a vector of two numbers, or NA if not found}
#'   
#' @details
#' The function checks if x falls within any interval in the partition.
#' For an interval [a, b), x is considered to be in the interval if a <= x < b.
#' 
#' @examples
#' p <- create_partition(list(c(-Inf, 1), c(1, 2), c(2, Inf)))
#' get_interval(0.5, p)  # list(index = 1, interval = c(-Inf, 1))
#' get_interval(1.5, p)  # list(index = 2, interval = c(1, 2))
#' get_interval(2.5, p)  # list(index = 3, interval = c(2, Inf))
#' @export
get_interval <- function(x, partition) {
  for (i in seq_along(partition$intervals)) {
    interval <- partition$intervals[[i]]
    if (x >= interval[1] && x < interval[2])
      return(list(index = i, interval = interval))
  }
  list(index = NA, interval = NA)
}


#' Get lengths of intervals in a partition
#' 
#' This function calculates the length of each interval in a partition.
#' For intervals with infinite bounds, the length is returned as Inf.
#' 
#' @param partition Partition object created by \code{create_partition}.
#' 
#' @return A numeric vector containing the length of each interval.
#' 
#' @examples
#' p <- create_partition(list(c(-Inf, 1), c(1, 2), c(2, Inf)))
#' get_lengths(p)  # c(Inf, 1, Inf)
#' 
#' p2 <- create_partition(list(c(0, 1), c(1, 2), c(2, 3)))
#' get_lengths(p2)  # c(1, 1, 1)
#' @export
get_lengths <- function(partition) {
  vapply(partition$intervals, function(interval) {
    if (is.infinite(interval[1]) || is.infinite(interval[2])) {
      Inf
    } else {
      interval[2] - interval[1]
    }
  }, numeric(1))
}
