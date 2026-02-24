#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL

#' Set global variables to use throughout, silencing notes during check.
#'
#' These variables are generally those expected in the standard data formats
#' used, such as GTFS and TIDES. Whenever one of these variable names must be
#' used, checks are performed to ensure they are present in the input data.
#'
#' @name set_globals
utils::globalVariables(c(
  # GTFS
  "agency_id", "service_id", "roud_id", "stop_id", "stop_lat", "stop_lon",
  "stop_sequence", "trip_id", "shape_pt_lat", "shape_pt_lon",
  # TIDES
  "trip_id_performed", "event_timestamp", "vehicle_id", "location_ping_id",
  "operator_id", "speed", "latitude", "longitude",
  # Internal to transittraj functions
  "delta_dist", "delta_time", "max_dist", "min_dist", "max_time", "min_time",
  "trip_distance", "trip_distance", "duration", "max_dist_gap", "max_t_gap",
  "dist_ok", "dist_gap_ok", "t_gap_ok"
  ))

#' Calculates numerical inverse of a trajectory function
#'
#' Not intended for external use
#'
#' @param f Direct traj function
#' @param lower lower distance range
#' @param upper upper distance range
#' @param inv_tol tolerance for numeric inverse
#' @return function for inverse trajectory
get_inverse_traj <- function(f, lower, upper, inv_tol) {
  Vectorize(function(distance) {
    stats::uniroot(f = function(x) {f(x) - distance},
            lower = lower, upper = upper, tol = inv_tol)$root
  })
}

#' Corrects speeds to Fristch-Carlson constraints, recursively.
#'
#' Internal function. Not intended for external use.
#'
#' @param m_0 A numeric vector of initial slopes (observed velocities)
#' @param deltas A numeric vector of initial FC delta values
#' @return A numeric vector of m_0 adjusted to FC constraints
correct_speeds_fun <- function(m_0, deltas) {

  # validate
  if (length(m_0) < 2) {
    rlang::abort(message = "Must have at least two observations to correct speeds.",
                 class = "error_avlclean_fc")
  }

  # Algorithm is recursive -- loop through each of m_0
  for (iter in 1:(length(m_0) - 1)) {
    # Get initial values
    m_i = m_0[iter]
    m_i1 = m_0[iter + 1]
    delta_i = deltas[iter]

    # Calculate FC params
    alpha_i = m_i / delta_i
    beta_i = m_i1 / delta_i
    ab_sq = (alpha_i^2) + (beta_i^2)

    if (ab_sq > 9) {
      # If FC constraint not satisfied
      tau_i = 3 / sqrt(ab_sq)

      new_m_i = tau_i * alpha_i * delta_i
      new_m_i1 = tau_i * beta_i * delta_i

      # Replace slope
      m_0[iter] <- new_m_i
      m_0[iter + 1] <- new_m_i1
    }
    # Otherwise, can leave slope as is
  }
  return(m_0)
}
