#' Integrate an ODE with an arbitrary initial time
#'
#' A wrapper around `deSolve::ode` that lets you specify the initial condition
#' `y` at some interior time point `init_time` within your `times` grid.
#' The function splits the integration into a backward run (from `init_time` down
#' to `min(times)`) and a forward run (from `init_time` up to `max(times)`),
#' then stitches the two results into a single, ascending-time solution.
#'
#' @param y Numeric vector. The state of the system at `init_time`.
#' @param times Numeric vector. All time points at which you want the solution
#'              returned.  Must cover `init_time` (i.e. `min(times) <= init_time <= max(times)`).
#' @param func Function defining the ODE right-hand side.  Must have signature
#'             `func(t, y, parms)` and return a `list` of derivatives.
#' @param parms Numeric or list.  Parameters passed to `func`.
#' @param init_time **This is the only difference from `deSolve::ode`**.
#'                  Numeric scalar.  The time within `times` at which
#'                  the initial condition `y` applies.
#' @param method See `deSolve::ode`.
#' @param ...  Additional arguments passed on to `deSolve::ode`.
#'
#' @return A numeric matrix whose first column is `time` (in ascending order)
#'         and whose remaining columns are the state variables.
#'
#' @seealso [deSolve::ode()]
#'
#' @examples
#' library(deSolve); library(dplyr); library(ggplot2)
#' # simple ODE: dy/dt = -0.5 * y
#' f <- function(t, y, parms) list(-0.5 * y)
#' # define time grid and interior init time
#' times <- seq(0, 10, by = 0.1)
#' init_time <- 4
#' init_y_vec <- seq(0, 3, by = 0.5)
#'
#' # solve for each initial y in init_y_vec, stitch results, and plot
#' df <- tibble(init_y = init_y_vec) |> 
#'   mutate(
#'     res = purrr::map(
#'       init_y,
#'       ~ as_tibble(
#'         ode_extended(
#'           y         = c(y = .x),
#'           times     = times,
#'           func      = f,
#'           parms     = NULL,
#'           init_time = init_time
#'         )
#'       )
#'     )
#'   ) |> 
#'   tidyr::unnest(res)
#'
#' df |> 
#'   ggplot(aes(x = time, y = y, color = factor(init_y))) +
#'     geom_line() +
#'     geom_vline(xintercept = init_time, lty = 2) +
#'     geom_hline(yintercept = init_y_vec, lty = 3) +
#'     labs(color = "init_y") +
#'     theme_minimal()
#'
#' @export
ode_extended <- function(y, times, func, parms, init_time, 
                   method = c("lsoda", "lsode", "lsodes", 
                                    "lsodar", "vode", "daspk",
                                    "euler", "rk4", "ode23",
                                    "ode45", "radau", "bdf",
                                    "bdf_d", "adams", "impAdams",
                                    "impAdams_d", "iteration"), ...) {
  # init_time must be one of the requested output times
  # Check if init_time is in the range of times
  if (!(init_time >= min(times) && init_time <= max(times))) {
    stop("'init_time' must be in the range of 'times'")
  } else{
    times <- sort(c(times, init_time))
  }
  # split the time‐grid at init_time
  times_back <- sort(times[times <= init_time], decreasing = TRUE)
  times_fwd  <- sort(times[times >= init_time],  decreasing = FALSE)
  
  # 1) backward integration (if there is anything before init_time)
  if (length(times_back) > 1) {
    out_back <- deSolve::ode(
      y     = y,
      times = times_back,
      func  = func,
      parms = parms,
      method = method,
      ...
    )
    # reverse so times ascend, and drop the duplicate init_time row
    out_back <- out_back[nrow(out_back):1, , drop = FALSE]
    out_back <- out_back[-nrow(out_back), , drop = FALSE]
  } else {
    out_back <- NULL
  }
  
  # 2) forward integration
  out_fwd <- deSolve::ode(
    y     = y,
    times = times_fwd,
    func  = func,
    parms = parms,
    method = method,
    ...
  )
  
  # 3) stitch (and clear row names)
  if (!is.null(out_back)) {
    out <- rbind(out_back, out_fwd)
  } else {
    out <- out_fwd
  }
  rownames(out) <- NULL
  return(out)
}
