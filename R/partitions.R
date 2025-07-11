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
#' @example examples/partitions_example.R
#' @export
create_partition <- function(intervals, tol = .Machine$double.eps^0.5) {
  if (!is.list(intervals) || length(intervals) == 0) {
    stop("Intervals must be a non-empty list.")
  }
  # Validate each interval
  for (interval in intervals) {
    if (!is.numeric(interval) || length(interval) != 2) {
      stop("Each interval must be a numeric vector of length 2.")
    }
    if ((interval[1] - interval[2]) >= -tol) {
      stop("Each interval must have its first element less than its second element (within floating point tolerance).")
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
    
    # Check for overlap (allowing for floating point tolerance)
    if ((current[2] - next_interval[1]) > tol) {
      stop("Intervals must not overlap. Found overlap between [", 
           current[1], ", ", current[2], "] and [", 
           next_interval[1], ", ", next_interval[2], "].")
    }
    
    # Check for holes (allowing for floating point tolerance)
    if (abs(current[2] - next_interval[1]) > tol) {
      stop("Intervals must not have holes. Found hole between [", 
           current[1], ", ", current[2], "] and [", 
           next_interval[1], ", ", next_interval[2], "].")
    }
  }
  
  structure(list(intervals = sorted_intervals), class = "partition")
}

#' Create a partition with equal-length intervals
#' 
#' This function creates a partition of an interval into N equal-length subintervals.
#' 
#' @param interval A numeric vector of length 2 representing the interval to partition
#' @param width The desired width of each subinterval. Must divide the interval length evenly.
#' @param N The desired number of subintervals.
#' 
#' @return A partition object containing the equal-length intervals
#' 
#' @details
#' The function requires either \code{width} or \code{N} to be specified, but not both.
#' If \code{width} is provided, it must divide the interval length evenly.
#' If \code{N} is provided, the width is calculated as (interval[2] - interval[1]) / N.
#' 
#' @example examples/partitions_example.R
#' @export
create_partition_equal_lengths <- function(interval, width, N) {
  if (!is.numeric(interval) || length(interval) != 2) {
    stop("Interval must be a numeric vector of length 2.")
  }
  if (interval[1] >= interval[2]) {
    stop("Interval must have its first element less than its second element.")
  }
  
  # Check that exactly one of width or N is provided
  if (missing(width) && missing(N)) {
    stop("Either width or N must be provided.")
  }
  if (!missing(width) && !missing(N)) {
    stop("Only one of width or N should be provided.")
  }
  
  # Calculate intervals based on width or N
  if (!missing(width)) {
    # Check if width divides the interval length evenly
    total_length <- interval[2] - interval[1]
    if (!isTRUE(all.equal(total_length %% width, 0))) {
      stop("Width must divide the interval length evenly.")
    }
    N <- as.integer(total_length / width)
  } else {
    # Calculate width based on N
    width <- (interval[2] - interval[1]) / N
  }
  
  # Create N equal-length intervals
  intervals <- lapply(0:(N-1), function(i) {
    c(interval[1] + i * width, interval[1] + (i + 1) * width)
  })
  
  create_partition(intervals)
}

#' Create a partition from cutoff points
#' 
#' This function creates a partition from a vector of cutoff points by creating
#' intervals between consecutive cutoffs.
#' 
#' @param cutoffs A numeric vector of cutoff points that define the boundaries
#'   of the intervals. Must be sorted in ascending order.
#' 
#' @return A partition object with intervals defined by consecutive cutoffs.
#' 
#' @details
#' The function creates intervals of the form [cutoffs[i], cutoffs[i+1]) for
#' i = 1, ..., length(cutoffs)-1. The cutoffs must be sorted in ascending order.
#' Infinite values are allowed for the first and last cutoffs.
#' 
#' @examples
#' # Create partition from cutoffs
#' cutoffs <- c(-Inf, 0, 1, 2, Inf)
#' p <- create_partition_from_cutoffs(cutoffs)
#' #  (-Inf, 0), [0, 1), [1, 2), [2, Inf)
#' 
#' # Finite cutoffs
#' cutoffs2 <- c(0, 1, 2, 3)
#' p2 <- create_partition_from_cutoffs(cutoffs2)
#' #  [0, 1), [1, 2), [2, 3)
#' @export
create_partition_from_cutoffs <- function(cutoffs) {
  if (!is.numeric(cutoffs) || length(cutoffs) < 2) {
    stop("Cutoffs must be a numeric vector of length at least 2.")
  }
  
  # Check if cutoffs are sorted
  if (!all(diff(cutoffs) > 0)) {
    stop("Cutoffs must be sorted in ascending order.")
  }
  
  # Create intervals from consecutive cutoffs
  intervals <- lapply(1:(length(cutoffs) - 1), function(i) {
    c(cutoffs[i], cutoffs[i + 1])
  })
  
  create_partition(intervals)
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
  n <- length(partition$intervals)
  for (i in seq_along(partition$intervals)) {
    iv <- partition$intervals[[i]]
    a <- iv[1]; b <- iv[2]
    # use x == b for the very last interval
    if (x >= a && (x < b || (i == n && x == b))) {
      return(list(index = i, interval = iv))
    }
  }
  stop("Number does not fall within any interval in the partition")
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


#' Get the cutoffs of a partition
#' 
#' This function returns the cutoffs of a given partition, including infinite endpoints
#' when present. Use the endpoints parameter to control whether boundary points are included.
#' 
#' @param partition Partition object created by \code{create_partition}.
#' @param endpoints Logical. If TRUE (default), includes all endpoints (including infinite ones). If FALSE, excludes endpoints.
#' 
#' @return A numeric vector containing the cutoffs of the partition.
#' 
#' @examples
#' p <- create_partition(list(c(-Inf, 1), c(1, 2), c(2, Inf)))
#' get_cutoffs(p)  # c(-Inf, 1, 2, Inf)
#' 
#' p2 <- create_partition(list(c(0, 1), c(1, 2), c(2, 3)))
#' get_cutoffs(p2)  # c(0, 1, 2, 3)
#' get_cutoffs(p2, endpoints = FALSE)  # c(1, 2)
#' @export
get_cutoffs <- function(partition, endpoints = TRUE) {
  if (!inherits(partition, "partition")) {
    stop("Input must be a partition object created by create_partition()")
  }
  
  intervals <- partition$intervals
  n <- length(intervals)
  
  # Initialize vector to store cutoffs
  cutoffs <- numeric(2 * n)
  
  # Extract all endpoints
  for (i in seq_along(intervals)) {
    cutoffs[2*i-1] <- intervals[[i]][1]
    cutoffs[2*i] <- intervals[[i]][2]
  }
  
  # Remove duplicates and sort
  cutoffs <- unique(sort(cutoffs))
  
  # If endpoints = FALSE, remove the first and last cutoffs (including infinite ones)
  if (!endpoints && length(cutoffs) > 2) {
    cutoffs <- cutoffs[2:(length(cutoffs) - 1)]
  }
  
  return(cutoffs)
}


#' Check if one partition is finer than another
#' 
#' This function determines if partition p1 is finer than partition p2.
#' A partition p1 is finer than p2 if every interval in p1 is contained 
#' within some interval in p2.
#' 
#' @param p1 Partition object created by \code{create_partition}.
#' @param p2 Partition object created by \code{create_partition}.
#' 
#' @return Logical value: TRUE if p1 is finer than p2, FALSE otherwise.
#' 
#' @details
#' For p1 to be finer than p2, every interval [a, b) in p1 must be 
#' completely contained within some interval [c, d) in p2, meaning
#' c <= a < b <= d.
#' 
#' @examples
#' p1 <- create_partition(list(c(0, 1), c(1, 2), c(2, 3), c(3, 4)))
#' p2 <- create_partition(list(c(0, 2), c(2, 4)))
#' is_finer(p1, p2)  # TRUE
#' is_finer(p2, p1)  # FALSE
#' @export
is_finer <- function(p1, p2) {
  if (!inherits(p1, "partition") || !inherits(p2, "partition")) {
    stop("Both inputs must be partition objects created by create_partition()")
  }
  
  # Check if every interval in p1 is contained in some interval in p2
  for (interval1 in p1$intervals) {
    a1 <- interval1[1]
    b1 <- interval1[2]
    
    # Check if this interval is contained in any interval of p2
    contained <- FALSE
    for (interval2 in p2$intervals) {
      a2 <- interval2[1]
      b2 <- interval2[2]
      
      # Check if [a1, b1) is contained in [a2, b2)
      if (a2 <= a1 && b1 <= b2) {
        contained <- TRUE
        break
      }
    }
    
    # If any interval in p1 is not contained in p2, p1 is not finer
    if (!contained) {
      return(FALSE)
    }
  }
  
  return(TRUE)
}


#' Create the join (finest common coarsening) of two partitions
#' 
#' This function creates the join of two partitions, which is the finest
#' partition that is coarser than both input partitions. In other words,
#' it finds the finest common coarsening of p1 and p2.
#' 
#' @param p1 Partition object created by \code{create_partition}.
#' @param p2 Partition object created by \code{create_partition}.
#' 
#' @return A partition object representing the join of p1 and p2.
#' 
#' @details
#' The join is computed by finding the finest partition such that both
#' p1 and p2 are refinements of it. This means finding cutoffs that
#' appear in both partitions (common boundaries).
#' 
#' @examples
#' # Example 1
#' p1 <- create_partition(list(c(0, 1), c(1, 3), c(3, 4)))
#' p2 <- create_partition(list(c(0, 2), c(2, 4)))
#' join_p <- create_join(p1, p2)  # Result: [0,4)
#' is_finer(p1, join_p)  # TRUE
#' is_finer(p2, join_p)  # TRUE
#' 
#' # Example 2
#' p3 <- create_partition(list(c(0, 1), c(1, 2), c(2, 3)))
#' p4 <- create_partition(list(c(0, 1), c(1, 3)))
#' join_p2 <- create_join(p3, p4)  # Result: [0,1), [1,3)
#' is_finer(p3, join_p2)  # TRUE
#' is_finer(p4, join_p2)  # TRUE
#' @export
create_join <- function(p1, p2) {
  if (!inherits(p1, "partition") || !inherits(p2, "partition")) {
    stop("Both inputs must be partition objects created by create_partition()")
  }
  
  # Get all cutoffs from both partitions
  cutoffs1 <- get_cutoffs(p1, endpoints = TRUE) |> round(digits = 6)
  cutoffs2 <- get_cutoffs(p2, endpoints = TRUE) |> round(digits = 6)
  
  # Check that partitions have the same domain endpoints
  if (length(cutoffs1) > 0 && length(cutoffs2) > 0) {
    endpoints1 <- cutoffs1[c(1, length(cutoffs1))]
    endpoints2 <- cutoffs2[c(1, length(cutoffs2))]
    if (!all(endpoints1 == endpoints2)) {
      stop("Partitions must have the same domain endpoints")
    }
  }
  
  # Find cutoffs that appear in both partitions (common boundaries)
  common_cutoffs <- intersect(cutoffs1, cutoffs2)
  
  # Use the validated domain bounds
  domain_start <- cutoffs1[1]
  domain_end <- cutoffs1[length(cutoffs1)]
  
  # Create intervals using only common cutoffs
  join_intervals <- list()
  
  if (length(common_cutoffs) == 0) {
    # No common internal boundaries, so join is the entire domain
    join_intervals <- list(c(domain_start, domain_end))
  } else {
    # Sort common cutoffs
    common_cutoffs <- sort(common_cutoffs)
    
    # Create intervals between common cutoffs
    # First interval: from domain start to first common cutoff
    if (domain_start < common_cutoffs[1]) {
      join_intervals <- append(join_intervals, list(c(domain_start, common_cutoffs[1])))
    }
    
    # Intervals between consecutive common cutoffs
    if (length(common_cutoffs) > 1) {
      for (i in 1:(length(common_cutoffs) - 1)) {
        join_intervals <- append(join_intervals, list(c(common_cutoffs[i], common_cutoffs[i + 1])))
      }
    }
    
    # Last interval: from last common cutoff to domain end
    if (common_cutoffs[length(common_cutoffs)] < domain_end) {
      join_intervals <- append(join_intervals, list(c(common_cutoffs[length(common_cutoffs)], domain_end)))
    }
  }
  
  # Handle infinite bounds - use domain endpoints
  if (length(join_intervals) > 0) {
    if (is.infinite(domain_start)) {
      join_intervals[[1]][1] <- -Inf
    }
    
    if (is.infinite(domain_end)) {
      join_intervals[[length(join_intervals)]][2] <- Inf
    }
  }
  
  create_partition(join_intervals)
}


#' Create the meet (coarsest common refinement) of two partitions
#' 
#' This function creates the meet of two partitions, which is the coarsest
#' partition that is finer than both input partitions. In other words,
#' it finds the coarsest common refinement of p1 and p2.
#' 
#' @param p1 Partition object created by \code{create_partition}.
#' @param p2 Partition object created by \code{create_partition}.
#' 
#' @return A partition object representing the meet of p1 and p2.
#' 
#' @details
#' The meet is computed by taking all cutoff points from both partitions
#' and creating the partition that includes all these boundaries. This is
#' the coarsest partition that is finer than both p1 and p2.
#' 
#' @examples
#' # Example 1
#' p1 <- create_partition(list(c(0, 1), c(1, 3), c(3, 4)))
#' p2 <- create_partition(list(c(0, 2), c(2, 4)))
#' meet_p <- create_meet(p1, p2)  # Result: [0,1), [1,2), [2,3), [3,4)
#' is_finer(meet_p, p1)  # TRUE
#' is_finer(meet_p, p2)  # TRUE
#' 
#' # Example 2
#' p3 <- create_partition(list(c(0, 2), c(2, 4)))
#' p4 <- create_partition(list(c(0, 1), c(1, 4)))
#' meet_p2 <- create_meet(p3, p4)  # Result: [0,1), [1,2), [2,4)
#' is_finer(meet_p2, p3)  # TRUE
#' is_finer(meet_p2, p4)  # TRUE
#' @export
create_meet <- function(p1, p2) {
  if (!inherits(p1, "partition") || !inherits(p2, "partition")) {
    stop("Both inputs must be partition objects created by create_partition()")
  }
  
  # Get all cutoffs from both partitions
  cutoffs1 <- get_cutoffs(p1, endpoints = TRUE) |> round(digits = 6)
  cutoffs2 <- get_cutoffs(p2, endpoints = TRUE) |> round(digits = 6)
  
  # Check that partitions have the same domain endpoints
  if (length(cutoffs1) > 0 && length(cutoffs2) > 0) {
    endpoints1 <- cutoffs1[c(1, length(cutoffs1))]
    endpoints2 <- cutoffs2[c(1, length(cutoffs2))]
    if (!all(endpoints1 == endpoints2)) {
      stop("Partitions must have the same domain endpoints")
    }
  }
  
  # Combine and sort all cutoffs (since domains are validated to be same)
  all_cutoffs <- unique(sort(c(cutoffs1, cutoffs2)))
  
  if (length(all_cutoffs) < 2) {
    stop("Cannot create meet: insufficient cutoffs")
  }
  
  # Create intervals from consecutive cutoffs
  meet_intervals <- list()
  for (i in 1:(length(all_cutoffs) - 1)) {
    meet_intervals <- append(meet_intervals, list(c(all_cutoffs[i], all_cutoffs[i + 1])))
  }
  
  # Handle infinite bounds - use validated domain endpoints
  if (length(meet_intervals) > 0) {
    domain_start <- cutoffs1[1]
    domain_end <- cutoffs1[length(cutoffs1)]
    
    if (is.infinite(domain_start)) {
      meet_intervals[[1]][1] <- -Inf
    }
    
    if (is.infinite(domain_end)) {
      meet_intervals[[length(meet_intervals)]][2] <- Inf
    }
  }
  
  create_partition(meet_intervals)
}
