library(dplyr)
library(data.table)
library(tibble)
library(arrow)
library(ggplot2)
library(fst)
library(microbenchmark)

# Parameter for sample size
N_sim <- 1e9

# Simulate a large dataset
set.seed(123)
large_data <- data.frame(
  id = 1:N_sim,
  value = rnorm(N_sim),
  category = sample(letters[1:5], N_sim, replace = TRUE)
)

# Save the dataset in different formats
saveRDS(large_data, "large_data.rds")
write_parquet(large_data, "large_data.parquet")
write_fst(large_data, "large_data.fst")
write.table(large_data, "large_data.csv", sep = ",", row.names = FALSE)
data_table <- as.data.table(large_data)

# Benchmark functions
optimal_filter <- function(file_name, format) {
  if (format == "data.frame") {
    data <- readRDS(file_name)
    return(data[data$value > 1 & data$category == "a", ])
  } else if (format == "tibble") {
    data <- as_tibble(readRDS(file_name))
    return(data %>% filter(value > 1 & category == "a"))
  } else if (format == "data.table") {
    data <- fread("large_data.csv")
    return(data[value > 1 & category == "a"])
  } else if (format == "arrow") {
    data <- open_dataset("large_data.parquet")
    return(data %>% filter(value > 1 & category == "a") %>% collect())
  } else if (format == "fst") {
    data <- read_fst("large_data.fst")
    return(data[data$value > 1 & data$category == "a", ])
  }
}

# Run benchmarks
results <- data.frame()
formats <- c("data.frame", "tibble", "data.table", "arrow", "fst")
file_names <- list(
  data.frame = "large_data.rds",
  tibble = "large_data.rds",
  data.table = "large_data.csv",
  arrow = "large_data.parquet",
  fst = "large_data.fst"
)

for (fmt in formats) {
  file_name <- file_names[[fmt]]
  
  save_times <- numeric(5)
  read_times <- numeric(5)
  filter_times <- numeric(5)
  
  for (i in 1:5) {
    save_times[i] <- system.time({
      if (fmt == "data.frame") saveRDS(large_data, file_name)
      if (fmt == "tibble") saveRDS(large_data, file_name)
      if (fmt == "data.table") fwrite(data_table, file_name)
      if (fmt == "arrow") write_parquet(large_data, file_name)
      if (fmt == "fst") write_fst(large_data, file_name)
    })[3]
    
    read_times[i] <- system.time({
      if (fmt == "data.frame") readRDS(file_name)
      if (fmt == "tibble") as_tibble(readRDS(file_name))
      if (fmt == "data.table") fread(file_name)
      if (fmt == "arrow") open_dataset(file_name)
      if (fmt == "fst") read_fst(file_name)
    })[3]
    
    filter_times[i] <- system.time(optimal_filter(file_name, fmt))[3]
  }
  
  results <- rbind(results, data.frame(
    format = fmt,
    operation = "Save",
    time = mean(save_times)
  ))
  
  results <- rbind(results, data.frame(
    format = fmt,
    operation = "Read",
    time = mean(read_times)
  ))
  
  results <- rbind(results, data.frame(
    format = fmt,
    operation = "Filter",
    time = mean(filter_times)
  ))
}

# Clean up written files
file.remove("large_data.rds", "large_data.parquet", "large_data.fst", "large_data.csv")

# Plot the results
results$operation <- factor(results$operation, levels = c("Save", "Read", "Filter"))

ggplot(results, aes(x = format, y = time, fill = operation)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Performance Comparison of Data Handling Packages",
       x = "Data Format",
       y = "Average Time (seconds)",
       fill = "Operation") +
  theme_minimal()