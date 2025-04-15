#' Create a partition of the real line
#' @param intervals List of intervals, where each interval is a vector of 2 numbers
#' @return A partition object
#' @examples examples/intervals_example.R
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
#' @param x Number to check
#' @param partition Partition object
#' @return A list with the index of the interval and the interval itself, or NA if not found
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
