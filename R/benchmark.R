#' Benchmark dense matrix multiplication
#'
#' Generates two large random matrices and benchmarks their multiplication using
#' `microbenchmark::microbenchmark()`. Matrix dimensions default to
#' 10,000 x 300 and 300 x 1,000, respectively, and a fixed seed is used for
#' reproducibility.
#'
#' @return A `microbenchmark` object containing timing results for the 10
#' repeated matrix multiplications.
#'
#' @examples
#' bench_matrix()
#'
#' @export
bench_matrix <- function(){
  set.seed(1)
  m <- 1e4
  n <- 1e3
  k <- 3e2
  X <- matrix(rnorm(m*k), nrow=m)
  Y <- matrix(rnorm(n*k), ncol=n)
  microbenchmark::microbenchmark({
    Z <- X %*% Y
  }, times=10L)
}
