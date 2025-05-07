# ---------------------------------------------------------------
# General ODE Solver for Signaling Models based on U(theta, theta', a)
# ---------------------------------------------------------------

#' Solve the signaling ODE for strategy \eqn{\alpha(\theta)}
#'
#' Numerically solves the ODE \deqn{\alpha'(\theta) = -U_{\hat{\theta}} / U_a,} derived from
#' the first-order condition of the signaling game with payoff \eqn{U(\theta, \hat{\theta}, a)}.
#' Uses numerical differentiation to compute the gradient of U when not provided.
#'
#' Be sure to read the documentation for the `deSolve` package and `?deSolve::ode` for more details.
#' 
#'
#' @param fun.U Function \eqn{U(\theta, \hat{\theta}, a, \text{params})} defining the payoff.
#'        This MUST be defined as `fun.U(theta, theta_belief, a, params)` where 
#'        `theta`, `theta_belief`, `a` are numeric scalars, and `params` is a list or `NULL`.
#'        Should return a single numeric value (the payoff). 
#'        \itemize{
#'           \item{`theta`}{: the true type, \eqn{\theta}}
#'           \item{`theta_belief`}{: the belief of the receiver, \eqn{\hat{\theta}}}
#'           \item{`a`}{: action/signal}
#'           \item{`params`}{: additional parameters (optional)}
#'         }
#' @param theta_range Numeric vector `c(theta_lower, theta_upper)` defining the type space.
#' @param initial_a Numeric initial condition for the signal: `alpha(theta_lower) = a_0`.
#' @param ... Further arguments to be passed to fun.U.
#' @param n_grid Integer, the number of points for the output grid spanning theta_range. Default 101.
#' @param method String, the integration method for `deSolve::ode (e.g., "lsoda", "rk4")`. Default "lsoda".
#' @param zero_tol Numeric, the tolerance below which the denominator `U_deriv_a` is considered zero. Default 1e-9.
#' @return A data frame with columns `theta` and `alpha_theta`.
#'         The value `alpha_theta` corresponds to the signaling function \eqn{\alpha(\theta)}.
#'         Returns NULL if the ODE solver fails. Includes attributes with input parameters.
#' @example examples/signaling_example.R
#' @import deSolve
#' @seealso [deSolve::ode()]]
#' @export
solve_signaling_ode <- function(fun.U,
                                theta_range,
                                initial_a,
                                ...,
                                fun.U_grad = NULL,  # Optional gradient function for fun.U
                                n_grid = 100,
                                method = "lsoda",
                                zero_tol = 1e-9) {
  
  # --- Input Validation ---
  if (!is.function(fun.U)) stop("'fun.U' must be a function.")
  # Check arity (number of arguments) - should be 4
  if (length(formals(fun.U)) < 3) {
    warning("Expected 'fun.U' to have arguments (theta, theta_belief, a) or (theta, theta_belief, a, params).")
  }
  if (!is.numeric(theta_range) || length(theta_range) != 2 || theta_range[1] >= theta_range[2]) {
    stop("'theta_range' must be a numeric vector c(lower, upper) with lower < upper.")
  }
  if (!is.numeric(initial_a) || length(initial_a) != 1) {
    stop("'initial_a' must be a single numeric value.")
  }
  if (!is.null(fun.U_grad) && !is.function(fun.U_grad)) {
    stop("'fun.U_grad' must be a function or NULL.")
  }
  
  # Capture additional parameters from ...
  U_addl_params <- list(...)
  
  # Check if params is empty and set to NULL if it is
  if (length(U_addl_params) == 0) {
    U_addl_params <- NULL
  }

  # If fun.U_grad is not provided, use numerical differentiation to find the gradient 
  if (is.null(fun.U_grad)) {
    fun.U_grad <- function(theta, theta_belief, a, ...) {
      fun.U_wrapper <- function(x) {
        U_addl_params <- list(...)
        do.call(fun.U, c(list(theta = x[1],
                              theta_belief = x[2],
                              a = x[3]), 
                         U_addl_params))
      }
      numDeriv::grad(
        func = fun.U_wrapper,
        x = c(theta, theta_belief, a), 
        method = "simple"
      )
    }
  }

  # --- Define the ODE function for deSolve ---
  # This internal function calculates d(alpha)/d(theta) = - U_deriv_theta_belief / U_deriv_a
  # 'alpha' is vector of current values of alpha(theta). This corresponds to 'state' in the deSolve package.
  # The name of the vector inside of alpha is `alpha_theta`. So, `alpha_theta` inside of this function uses the value of current alpha.
  # 'parms' is a list containing the user's U function and parameters
  # In particular, `parms` contains `fun.U`, `U_addl_params`, and `zero_tol`.
  # See `?deSolve::ode` for more details
  ode_func_internal <- function(theta,
                                alpha, 
                                parms) {
    with(as.list(c(alpha, parms)), {
      # --- Calculate numerical gradient [U_theta_belief, U_a] ---
      # Evaluate at (theta = theta, theta_belief = theta, a = alpha_theta)
      U_grad <- do.call(fun.U_grad, c(list(theta = theta,
                                          theta_belief = theta,
                                          a = alpha_theta),
                                      U_addl_params))
      U_deriv_theta_belief <- U_grad[2]   # Be careful about the index
      U_deriv_a <- U_grad[3]
      
      # --- Calculate Slope alpha'(theta) = -U_theta_belief / U_deriv_a ---
      slope <- NA_real_ # Default to NA
      
      # Check if U_deriv_a (denominator) is effectively zero
      if (abs(U_deriv_a) < zero_tol) {
        warning(paste("Denominator U_deriv_a is near zero (val=", signif(U_deriv_a, 4),
                      ") at theta =", round(theta, 4),
                      ". Check model validity or boundary conditions."), immediate. = TRUE)
      } else {
        # Calculate the slope normally if U_deriv_a is non-zero
        slope <- - U_deriv_theta_belief / U_deriv_a
        message(paste("Slope at theta =", round(theta, 4), "is", round(slope, 4)))
      }
      
      # Return the derivative(s) as a list (deSolve expects this format)
      # "The return value of func should be a list, whose first element is a vector 
      # containing the derivatives of y with respect to time, and whose next elements 
      # are global values that are required at each point in times."
      return(list(c(alpha_theta_deriv = slope)))
    })
  } # End of ode_func_internal definition
  
  # --- Prepare for Solver ---
  # Grid of theta values for output
  theta_values <- seq(from = theta_range[1], to = theta_range[2], length.out = n_grid)
  
  # Initial state (must be named matching the state variable in ode_func_internal)
  # This defines the starting value of the dependent variable alpha(theta_lower)
  alpha <- c(alpha_theta = initial_a)
  
  # Parameters list to pass THROUGH the ODE solver TO ode_func_internal
  solver_params <- list(
    fun.U = fun.U,
    U_addl_params = U_addl_params,
    zero_tol = zero_tol
  )
  
  # --- Solve the ODE ---
  solution <- tryCatch({
    deSolve::ode(
      y = alpha,
      times = theta_values,
      func = ode_func_internal,
      parms = solver_params,
      method = method
    )
  }, error = function(e) {
    message("ODE solver 'deSolve::ode' failed.")
    message("Error message: ", e$message)
    # Consider checking single-crossing property if solver fails.
    return(NULL) # Return NULL on failure
  })
  
  # --- Process and Return Results ---
  if (is.null(solution)) {
    return(NULL)
  } else {
    solution_df <- as.data.frame(solution)
    colnames(solution_df)[1] <- "theta" # Rename 'time' column from ode output
    # Check if any NAs were produced during solving (e.g., due to U_deriv_a=0 or grad errors)
    if(any(!is.finite(solution_df$signal_a))) {
      warning("NA or Infinite values produced during ODE solution. Results may be incomplete or invalid.", call.=FALSE)
    }
    # Add input parameters as attributes for context
    attr(solution_df, "theta_range") <- theta_range
    attr(solution_df, "initial_a") <- initial_a
    attr(solution_df, "U_addl_params") <- U_addl_params
    return(solution_df)
  }
  
} # End of solve_signaling_ode function