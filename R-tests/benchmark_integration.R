#!/usr/bin/env Rscript

# Benchmark script comparing integrate_cpp vs stats::integrate
# Tests both performance and accuracy across different function types

library(microbenchmark)
library(ggplot2)

# Load the package (assuming it's installed)
library(matsunoR)

# Test functions with known analytical solutions
test_functions <- list(
  # Simple polynomial: integral of x^2 from 0 to 1 = 1/3
  polynomial = list(
    f = function(x) x^2,
    lower = 0, upper = 1,
    analytical = 1/3,
    name = "x^2"
  ),
  
  # Exponential: integral of exp(-x) from 0 to 2 = 1 - exp(-2)
  exponential = list(
    f = function(x) exp(-x),
    lower = 0, upper = 2,
    analytical = 1 - exp(-2),
    name = "exp(-x)"
  ),
  
  # Trigonometric: integral of sin(x) from 0 to pi = 2
  trigonometric = list(
    f = function(x) sin(x),
    lower = 0, upper = pi,
    analytical = 2,
    name = "sin(x)"
  ),
  
  # Normal density: integral of dnorm(x) from -3 to 3 ≈ 0.9973
  normal = list(
    f = function(x) dnorm(x),
    lower = -3, upper = 3,
    analytical = pnorm(3) - pnorm(-3),
    name = "dnorm(x)"
  ),
  
  # Oscillatory function: more challenging
  oscillatory = list(
    f = function(x) sin(10*x) * exp(-x),
    lower = 0, upper = 2,
    analytical = 10 * (1 - exp(-2) * (cos(20) + 10 * sin(20))) / 101,
    name = "sin(10x)*exp(-x)"
  )
)

# Function to run accuracy tests
test_accuracy <- function() {
  cat("=== ACCURACY COMPARISON ===\n\n")
  
  results <- data.frame(
    Function = character(),
    Method = character(),
    Value = numeric(),
    Error = numeric(),
    AbsError = numeric(),
    RelError = numeric(),
    Subdivisions = integer(),
    stringsAsFactors = FALSE
  )
  
  for(test_name in names(test_functions)) {
    test <- test_functions[[test_name]]
    cat(sprintf("Testing: %s\n", test$name))
    
    # Test stats::integrate with subdivision=2000
    stats_result <- integrate(test$f, test$lower, test$upper, subdivisions = 2000)
    stats_error <- abs(stats_result$value - test$analytical)
    stats_rel_error <- stats_error / abs(test$analytical)
    
    # Test integrate_cpp (simple trapezoidal with 2000 subdivisions to match stats::integrate)
    cpp_result <- integrate_cpp(test$f, test$lower, test$upper, 2000)
    cpp_error <- abs(cpp_result$value - test$analytical)
    cpp_rel_error <- cpp_error / abs(test$analytical)
    
    # Store results
    results <- rbind(results, data.frame(
      Function = test$name,
      Method = "stats::integrate",
      Value = stats_result$value,
      Error = stats_result$abs.error,
      AbsError = stats_error,
      RelError = stats_rel_error,
      Subdivisions = stats_result$subdivisions
    ))
    
    results <- rbind(results, data.frame(
      Function = test$name,
      Method = "integrate_cpp",
      Value = cpp_result$value,
      Error = NA,  # Simple trapezoidal doesn't provide error estimate
      AbsError = cpp_error,
      RelError = cpp_rel_error,
      Subdivisions = cpp_result$subdivisions
    ))
    
    cat(sprintf("  Analytical: %.10f\n", test$analytical))
    cat(sprintf("  stats::integrate: %.10f (error: %.2e, subdivisions: %d)\n", 
                stats_result$value, stats_error, stats_result$subdivisions))
    cat(sprintf("  integrate_cpp:    %.10f (error: %.2e, subdivisions: %d)\n\n", 
                cpp_result$value, cpp_error, cpp_result$subdivisions))
  }
  
  return(results)
}

# Function to run performance tests
test_performance <- function() {
  cat("=== PERFORMANCE COMPARISON ===\n\n")
  
  timing_results <- list()
  
  for(test_name in names(test_functions)) {
    test <- test_functions[[test_name]]
    cat(sprintf("Benchmarking: %s\n", test$name))
    
    # Benchmark both methods
    benchmark_result <- microbenchmark(
      stats_integrate = integrate(test$f, test$lower, test$upper, subdivisions = 2000),
      integrate_cpp = integrate_cpp(test$f, test$lower, test$upper, 2000),
      times = 100
    )
    
    print(benchmark_result)
    cat("\n")
    
    timing_results[[test_name]] <- benchmark_result
  }
  
  return(timing_results)
}

# Function to create summary plots
create_plots <- function(accuracy_results, timing_results) {
  cat("=== CREATING SUMMARY PLOTS ===\n\n")
  
  # Accuracy plot
  accuracy_plot <- ggplot(accuracy_results, aes(x = Function, y = log10(AbsError), fill = Method)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Integration Accuracy Comparison: Simple Trapezoidal vs Adaptive",
         subtitle = "Lower is better (log10 scale)",
         x = "Test Function", 
         y = "log10(Absolute Error)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggsave("integration_accuracy.png", accuracy_plot, width = 10, height = 6)
  
  # Performance plot - combine all timing results
  all_timings <- do.call(rbind, lapply(names(timing_results), function(name) {
    df <- as.data.frame(timing_results[[name]])
    df$Function <- name
    return(df)
  }))
  
  performance_plot <- ggplot(all_timings, aes(x = Function, y = time/1e6, fill = expr)) +
    geom_boxplot() +
    scale_y_log10() +
    labs(title = "Integration Performance Comparison: Simple Trapezoidal vs Adaptive",
         subtitle = "Lower is better (log scale)",
         x = "Test Function", 
         y = "Time (milliseconds)",
         fill = "Method") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggsave("integration_performance.png", performance_plot, width = 10, height = 6)
  
  cat("Plots saved: integration_accuracy.png, integration_performance.png\n\n")
}

# Main execution
main <- function() {
  cat("INTEGRATION BENCHMARK: integrate_cpp (simple trapezoidal) vs stats::integrate(subdivisions=2000)\n")
  cat("=====================================================================\n\n")
  
  # Run accuracy tests
  accuracy_results <- test_accuracy()
  
  # Run performance tests  
  timing_results <- test_performance()
  
  # Create summary plots
  create_plots(accuracy_results, timing_results)
  
  # Summary statistics
  cat("=== SUMMARY ===\n")
  cat("Accuracy (mean absolute error):\n")
  accuracy_summary <- aggregate(AbsError ~ Method, accuracy_results, mean)
  print(accuracy_summary)
  
  cat("\nPerformance (median time in ms):\n")
  all_timings <- do.call(rbind, lapply(timing_results, as.data.frame))
  performance_summary <- aggregate(time ~ expr, all_timings, function(x) median(x)/1e6)
  print(performance_summary)
  
  # Save results
  write.csv(accuracy_results, "accuracy_results.csv", row.names = FALSE)
  saveRDS(timing_results, "timing_results.rds")
  
  cat("\nResults saved: accuracy_results.csv, timing_results.rds\n")
}

# Run the benchmark
main()
