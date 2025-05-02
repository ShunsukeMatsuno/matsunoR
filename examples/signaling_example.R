pacman::p_load(tidyverse, ggplot2, matsunoR)

# Example 1: Simple linear cost function --------------------------------- 

# Define payoff function for first example: U = theta_belief - a/theta
payoff_eg1 <- function(theta, theta_belief, a) {
  if (theta <= 0) return(NA) # Basic error check
  return(theta_belief - a / theta)
}

# Solve the model using the general solver
solution_eg1 <- solve_signaling_ode(
  fun.U = payoff_eg1,
  theta_range = c(1.0, 5.0),
  initial_a = 0.0
)

# Print and plot results
print(head(solution_eg1))
# Add analytical solution for comparison
# For this example, analytical solution is alpha(theta) = 0.5 * (theta^2 - theta_lower^2)
theta_lower_eg1 <- attr(solution_eg1, "theta_range")[1]
solution_eg1$analytical <- 0.5 * (solution_eg1$theta^2 - theta_lower_eg1^2)

# Create plot comparing numerical and analytical solutions
ggplot(solution_eg1, aes(x = theta)) +
  geom_line(aes(y = alpha_theta, color = "Numerical"),
            linewidth = 1,
            alpha = .7) +
  geom_point(aes(y = analytical, color = "Analytical"), 
             alpha = .7) +
  scale_color_manual(values = c("Numerical" = "blue", "Analytical" = "red")) +
  labs(
    x = expression(paste("Type ", theta)),
    y = "a = alpha(theta)",
    title = "U = theta_belief - a/theta",
    color = "Solution"
  ) +
  theme_minimal() +
  theme(legend.position = "top")


# Example 2: Quadratic cost function ================================= 

# Define payoff function with quadratic cost: U = theta_belief - cost * a^2/theta
payoff_eg2 <- function(theta, theta_belief, a, cost) {
  if (theta <= 0) return(NA) # Basic safety check for invalid type
  # Payoff = Receiver's Belief - Cost of Signal
  # Cost = cost_factor * a^2 / theta
  return(theta_belief - cost * a^2 / theta)
}

# Set up parameters
theta_range_eg2 <- c(1.0, 5.0) # Type space theta is in [1, 5]
initial_a_eg2 <- 0.0         # Initial signal alpha(1.0) = 0
cost_low <- 0.5              # Low cost parameter
cost_high <- 2               # High cost parameter

# Solve for low cost case
solution_eg2_lowcost <- solve_signaling_ode(
  fun.U = payoff_eg2,         # Pass the payoff function
  theta_range = theta_range_eg2, # Pass the type range
  initial_a = initial_a_eg2,   # Pass the initial condition
  cost = cost_low   # additional parameter
)

# Solve for high cost case
solution_eg2_highcost <- solve_signaling_ode(
  fun.U = payoff_eg2,         # Pass the payoff function
  theta_range = theta_range_eg2, # Pass the type range
  initial_a = initial_a_eg2,   # Pass the initial condition
  cost = cost_high  # additional parameter
)

# Prepare data frames for plotting
# Format numerical solution for low cost
df_lowcost <- as_tibble(solution_eg2_lowcost) |> 
  mutate(type = "Numerical",
         cost = cost_low)
         
# Calculate analytical solution for low cost
# For quadratic cost, analytical solution is alpha(theta) = sqrt((theta^2 - theta_lower^2)/(2*cost))
df_lowcost_analytical <- as_tibble(solution_eg2_lowcost) |> 
  mutate(alpha_theta = sqrt( pmax(0, (theta^2 - theta_range_eg2[1]^2) / (2 * cost_low)) ),
         type = "Analytical",
         cost = cost_low)
         
# Format numerical solution for high cost
df_highcost <- as_tibble(solution_eg2_highcost) |> 
  mutate(type = "Numerical",
         cost = cost_high)
         
# Calculate analytical solution for high cost
df_highcost_analytical <- as_tibble(solution_eg2_highcost) |> 
  mutate(alpha_theta = sqrt( pmax(0, (theta^2 - theta_range_eg2[1]^2) / (2 * cost_high)) ),
         type = "Analytical",
         cost = cost_high)
         
# Combine all data frames
df <- bind_rows(df_lowcost, df_lowcost_analytical, df_highcost, df_highcost_analytical) |> 
  mutate(cost = as.factor(cost))

# Create plot comparing solutions with different cost parameters
ggplot(df, aes(x = theta)) +
  geom_line(aes(y = alpha_theta, color = cost),
            linewidth = 1,
            alpha = .7,
            data = df |> filter(type == "Numerical")) +
  geom_point(aes(y = alpha_theta, color = cost), 
             alpha = .7,
             data = df |> filter(type == "Analytical")) +
  labs(
    x = expression(paste("Type ", theta)),
    y = "a = alpha(theta)",
    title = "U = theta_belief - c * a^2 / theta", 
    subtitle = "Comparing different cost parameters",
    color = "Cost parameter"
  ) +
  theme_minimal() +
  theme(legend.position = "top")



# Example 3: No unique solution ------------------------------------------------------

# TODO: This does not run. Add error handling case when payoff_eg3 is NA.
# Define payoff function with no unique solution
payoff_eg3 <- function(theta, theta_belief, a, beta) {
  if (theta <= 0) return(NA_real_) # Basic safety check for invalid type
  if (a >= 1/beta - 1e-3) return(NA_real_) # Basic safety check for invalid signal
  return(theta_belief + theta * log(- beta * a + 1))
}

# Set up parameters
theta_range_eg3 <- c(0.1, 5.0) # Type space theta is in [0, 5]
initial_a_eg3 <- 0.1         # Initial signal alpha(1.0) = 0

# Solve for low cost case
solution_eg3 <- solve_signaling_ode(
  fun.U = payoff_eg3,         # Pass the payoff function
  theta_range = theta_range_eg3, # Pass the type range
  initial_a = initial_a_eg3,   # Pass the initial condition
  beta = 0.1
)

# Solve for multiple initial conditions
df <- tibble(initial_a = seq(1e-2, 5, length = 20) |> round(2)) |> 
  mutate(solution = map(initial_a, ~solve_signaling_ode(payoff_eg3, theta_range_eg3, .x, beta = 0.1))) |> 
  unnest(solution)

# Plot the solutions
ggplot(df, aes(x = theta, y = alpha_theta, color = as.factor(initial_a))) +
  geom_line() +
  theme_minimal() +
  labs(x = "Type theta", y = "Signal alpha(theta)", color = "Initial signal")

         