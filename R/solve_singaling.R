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
                                fun.U_grad = NULL, # Optional gradient function for fun.U
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
        do.call(fun.U, c(
          list(
            theta = x[1],
            theta_belief = x[2],
            a = x[3]
          ),
          U_addl_params
        ))
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
  #
  # deSolve::ode() requires a specific function signature: func(time, y, parms)
  # In our case:
  # - 'theta' plays the role of 'time' (the independent variable we integrate over)
  # - 'alpha' plays the role of 'y' (the dependent variable/state vector)
  # - 'parms' contains our user-provided functions and parameters
  #
  # The 'alpha' argument is a NAMED vector. We initialize it as c(alpha_theta = initial_a)
  # so inside this function, alpha_theta refers to the current value of the signal function
  # at the current theta value.
  #
  # The function must return a list where the first element is a vector of derivatives
  # with the same names as the input state vector.
  ode_func_internal <- function(theta,
                                alpha,
                                parms) {
    # Extract variables from the state vector and parameters
    # with() makes alpha_theta, fun.U, U_addl_params, zero_tol available in scope
    with(as.list(c(alpha, parms)), {
      # --- Calculate numerical gradient [U_theta, U_theta_belief, U_a] ---
      # We need partial derivatives of U with respect to its arguments
      # The gradient function returns a vector: [dU/dtheta, dU/dtheta_belief, dU/da]
      # We evaluate at the "truthful" equilibrium point: theta_belief = theta
      # and at the current signal level: a = alpha_theta
      U_grad <- do.call(fun.U_grad, c(
        list(
          theta = theta,
          theta_belief = theta, # Truthful belief
          a = alpha_theta
        ), # Current signal
        U_addl_params
      ))

      # Extract the relevant partial derivatives
      # Index [1] = dU/dtheta (not needed for the ODE)
      # Index [2] = dU/dtheta_belief (this is what we need in numerator)
      # Index [3] = dU/da (this is what we need in denominator)
      U_deriv_theta_belief <- U_grad[2]
      U_deriv_a <- U_grad[3]

      # --- Calculate Slope: alpha'(theta) = -U_theta_belief / U_a ---
      # This comes from the first-order condition of the signaling game
      # The sender chooses signal 'a' to maximize U(theta, receiver's belief, a)
      # In equilibrium, the receiver's belief equals the true type: theta_belief = theta
      slope <- NA_real_ # Default to NA in case of division by zero

      # Check if U_deriv_a (denominator) is effectively zero
      # If the payoff function is not sensitive to the signal, we can't solve the ODE
      if (abs(U_deriv_a) < zero_tol) {
        warning(paste(
          "Denominator U_deriv_a is near zero (val=", signif(U_deriv_a, 4),
          ") at theta =", round(theta, 4),
          ". Check model validity or boundary conditions."
        ), immediate. = TRUE)
      } else {
        # Calculate the slope normally if U_deriv_a is non-zero
        slope <- -U_deriv_theta_belief / U_deriv_a
        message(paste("Slope at theta =", round(theta, 4), "is", round(slope, 4)))
      }

      # --- Return derivatives in deSolve format ---
      # deSolve expects: list(c(dy1/dt, dy2/dt, ...), global_values...)
      # We have only one state variable alpha_theta, so we return its derivative
      # The name "alpha_theta_deriv" is just for clarity; deSolve uses position matching
      return(list(c(alpha_theta_deriv = slope)))
    })
  } # End of ode_func_internal definition

  # --- Prepare for Solver ---
  # Create grid of theta values where we want to evaluate the solution
  # This becomes the 'times' argument for deSolve::ode
  theta_values <- seq(from = theta_range[1], to = theta_range[2], length.out = n_grid)

  # Initial state vector - MUST be named to match what ode_func_internal expects
  # This defines the boundary condition: alpha(theta_lower) = initial_a
  # The name "alpha_theta" will be used inside ode_func_internal via with()
  alpha <- c(alpha_theta = initial_a)

  # Parameters list to pass to ode_func_internal
  # deSolve::ode will pass this 'parms' argument to our internal function
  # Include everything the internal function needs: user's U function, parameters, tolerance
  solver_params <- list(
    fun.U = fun.U, # User's payoff function
    fun.U_grad = fun.U_grad, # Gradient function (analytical or numerical)
    U_addl_params = U_addl_params, # Additional parameters for U function
    zero_tol = zero_tol # Tolerance for denominator check
  )

  # --- Solve the ODE ---
  # Call deSolve::ode with our carefully constructed arguments
  # y = initial state vector (alpha_theta = initial_a)
  # times = theta values where we want the solution
  # func = our internal ODE function
  # parms = parameters for the internal function
  # method = numerical integration method
  solution <- tryCatch(
    {
      deSolve::ode(
        y = alpha,
        times = theta_values,
        func = ode_func_internal,
        parms = solver_params,
        method = method
      )
    },
    error = function(e) {
      message("ODE solver 'deSolve::ode' failed.")
      message("Error message: ", e$message)
      message("Common causes: invalid payoff function, numerical instability, or boundary conditions.")
      return(NULL) # Return NULL on failure
    }
  )

  # --- Process and Return Results ---
  if (is.null(solution)) {
    return(NULL)
  } else {
    # Convert solution matrix to data frame
    solution_df <- as.data.frame(solution)

    # deSolve::ode returns a matrix with first column named "time"
    # We rename it to "theta" since that's our independent variable
    colnames(solution_df)[1] <- "theta"

    # Check if any NAs or infinite values were produced during solving
    # This can happen if U_deriv_a = 0 or if there are numerical issues
    if (any(!is.finite(solution_df$alpha_theta))) {
      warning("NA or Infinite values produced during ODE solution. Results may be incomplete or invalid.", call. = FALSE)
    }

    # Add input parameters as attributes for reference
    # This helps users understand what parameters were used
    attr(solution_df, "theta_range") <- theta_range
    attr(solution_df, "initial_a") <- initial_a
    attr(solution_df, "U_addl_params") <- U_addl_params
    attr(solution_df, "method") <- method
    attr(solution_df, "n_grid") <- n_grid

    return(solution_df)
  }
} # End of solve_signaling_ode function
