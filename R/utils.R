#' Set global variables to use throughout, silencing notes during check.
#'
#' These variables are generally those expected in the standard data formats
#' used, such as GTFS and TIDES. Whenever one of these variable names must be
#' used, checks are performed to ensure they are present in the input data.
utils::globalVariables(
  # GTFS
  "agency_id", "service_id", "roud_id", "stop_id", "stop_lat", "stop_lon",
  "stop_sequence", "trip_id", "shape_pt_lat", "shape_pt_lon",
  # TIDES
  "trip_id_performed", "event_timestamp", "vehicle_id", "location_ping_id",
  "operator_id", "speed", "latitude", "longitude"
  )

#' Calculates numerical inverse of a trajectory function
#'
#' Not intended for external use
get_inverse_traj <- function(f, lower, upper, inv_tol) {
  Vectorize(function(distance) {
    stats::uniroot(f = function(x) {f(x) - distance},
            lower = lower, upper = upper, tol = inv_tol)$root
  })
}
