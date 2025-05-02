#' Integrate an ODE with an arbitrary initial time
#'
#' A wrapper around \code{deSolve::ode} that lets you specify the initial condition
#' \code{y} at some interior time point \code{init_time} within your \code{times} grid.
#' The function splits the integration into a backward run (from \code{init_time} down
#' to \code{min(times)}) and a forward run (from \code{init_time} up to \code{max(times)}),
#' then stitches the two results into a single, ascending-time solution.
#'
#' @param y Numeric vector. The state of the system at \code{init_time}.
#' @param times Numeric vector. All time points at which you want the solution
#'              returned.  Must cover \code{init_time} (i.e.\ 
#'              \code{min(times) <= init_time <= max(times)}).
#' @param func Function defining the ODE right-hand side.  Must have signature
#'             \code{func(t, y, parms)} and return a \code{list} of derivatives.
#' @param parms Numeric or list.  Parameters passed to \code{func}.
#' @param init_time **This is the only difference from \code{deSolve::ode}**.
#'                  Numeric scalar.  The time within \code{times} at which
#'                  the initial condition \code{y} applies.
#' @param method See \code{deSolve::ode}.
#' @param ...  Additional arguments passed on to \code{deSolve::ode}.
#'
#' @return A numeric matrix whose first column is \code{time} (in ascending order)
#'         and whose remaining columns are the state variables.
#'
#' @seealso \code{\link[deSolve]{ode}}
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
