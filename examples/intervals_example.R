# Example 1: Simple partition (-∞, 1), [1, 2], [2, ∞)
partition <- create_partition(list(
  c(-Inf, 1),
  c(1, 2),
  c(2, Inf)
))

# Test get_interval
print(get_interval(0.5, partition))  # list(index = 1, interval = c(-Inf, 1))
print(get_interval(1.5, partition))  # list(index = 2, interval = c(1, 2))
print(get_interval(2.5, partition))  # list(index = 3, interval = c(2, Inf))


# Example 2: Error for overlapping intervals
create_partition(list(
  c(-1, 1),
  c(0.5, 2),  # Overlaps with the first interval
  c(2, Inf)
))

# Example 3: Error for intervals with holes
create_partition(list(
  c(-1, 1),
  c(1.5, 2),  # Has a hole between 1 and 1.5
  c(2, Inf)
))