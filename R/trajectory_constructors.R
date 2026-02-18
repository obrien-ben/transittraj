#' Constructor for grouped trajectory class
#'
#' Not intended for external use.
#' This superclass holds a single, combined trajectory function.
#' Plus vectors of all trip IDs described by that function, and vectors of the time & distance ranges of those trips.
new_avltrajectory_group <- function(trip_id_performed = character(),
                                    traj_fun, inv_traj_fun = NULL,
                                    min_dist, max_dist, min_time, max_time,
                                    traj_type, inv_tol = NULL, max_deriv = 0,
                                    used_speeds = FALSE,
                                    ..., class = character()) {

  structure(trip_id_performed, class = c(class, "avltrajectory_group"),
            traj_fun = traj_fun,
            inv_traj_fun = inv_traj_fun,
            min_dist = min_dist,
            max_dist = max_dist,
            min_time = min_time,
            max_time = max_time,
            traj_type = traj_type,
            inv_tol = inv_tol,
            max_deriv = max_deriv,
            used_speeds = used_speeds)
}

#' Constructor for single trajectory class
#'
#' Not intended for external use.
#' This is a subclass (special case) of a grouped trajectory, in which there is only one trip.
#' Trajectory function is inteded to describe only one trip.
#' Associated trip properties (ID & ranges) should describe only that trip.
new_avltrajectory_single <- function(trip_id_performed = character(),
                                     traj_fun, inv_traj_fun = NULL,
                                     min_dist, max_dist, min_time, max_time,
                                     traj_type, inv_tol = NULL, max_deriv = 0,
                                     used_speeds = FALSE) {

  # Should take in only a single trip ID
  stopifnot(length(trip_id_performed) == 1)

  # Call parent class constructor
  new_avltrajectory_group(trip_id_performed,
                          traj_fun = traj_fun,
                          inv_traj_fun = inv_traj_fun,
                          min_dist = min_dist,
                          max_dist = max_dist,
                          min_time = min_time,
                          max_time = max_time,
                          traj_type = traj_type,
                          inv_tol = inv_tol,
                          max_deriv = max_deriv,
                          used_speeds = used_speeds,
                          class = "avltrajectory_single")
}

#' Fits a continuous function of distance versus time interpolating curve.
#'
#' This function fits a continuous vehicle trajectory function to observed AVL points, returning a trajectory object.
#' Interpolation can be done linearly (interp_method = "linear"), or via any method support by stats::splinefun().
#' By default (interp_method = "monoH.FC" and use_speeds = TRUE), a velocity-informed Fritsch-Carlson piecewise cubic polynomial will be fit. Input data must be striclty monotonic.
#' By default, both a trajectory (distance ~ time) and inverse trajectory (time ~ distance) will be fit. Input data must be striclty monotonic.
#' If using speeds, interp_method should be monoH.FC. If speeds are not available, ensure use_speeds = FALSE.
#'
#' @param distance_df Dataframe of linear AVL distances. Must include: "trip_id_performed", "event_timestamp", and numeric "distance".
#' @param interp_method Optional. The type of interpolation function to be fit. Either "linear", or a spline method from stats::splinefun(). Default is "monoH.FC" (stats::splinefunH()).
#' @param use_speeds Optional. A boolean, should curves be constrained by observed AVL speeds? Should only be used with interp_method = "monoH.FC", but monoH.FC does not require speeds. Default is TRUE.
#' @param return_group_function Optional. A boolean, should the returned trajectory object be grouped into a single function? If FALSE, will return a list (named by trip_id_performed) of single trajectory objects. Default is TRUE.
#' @param find_inverse_function Optional. A boolean, should the numeric inverse function (time ~ distance) be calculated? Default is TRUE.
#' @param inv_tol Optional. A numeric in the units of distance_df$distance, the tolerance used when calculating the numeric inverse function. Default is 0.01.
#' @return If return_group_function = TRUE, a grouped trajectory object. If FALSE, a list of single trajectory objects, named by their trip_id_performed.
#' @export
get_trajectory_fun <- function(distance_df, interp_method = "monoH.FC",
                               use_speeds = TRUE, return_group_function = TRUE,
                               find_inverse_function = TRUE, inv_tol = 0.01) {

  # Check input parameters
  # Set derivative
  if (interp_method == "linear") {
    # If linear
    max_deriv = 0
  } else {
    # If spline
    max_deriv = 3
  }
  # Check speed & method
  if (use_speeds) {
    # If using speeds
    if ("speed" %in% names(distance_df)) {
      # Check if speeds are in distance_df
      if (interp_method != "monoH.FC") {
        # if so, check that method is appropriate
        if (interp_method == "linear") {
          warning("Speeds cannot be used for linear interpolation. Ignoring speeds.")
        } else {
          warning("Using speeds for spline interpolation requires method monoH.FC. monoH.FC will be used unless use_speeds set to FALSE.")
        }
      }
    } else {
      stop("use_speeds set to TRUE, but no speeds column found in distance_df.")
    }
  }

  # Perform calculations for trip bounds
  trip_bounds <- distance_df %>%
    dplyr::group_by(trip_id_performed) %>%
    dplyr::summarise(min_dist = min(distance),
                     max_dist = max(distance),
                     min_time = min(as.numeric(event_timestamp)),
                     max_time = max(as.numeric(event_timestamp)))

  # Set up loop
  trips <- trip_bounds$trip_id_performed
  num_trips <- length(trips)
  traj_functions <- vector("list")
  traj_inv_functions <- vector("list")

  # Loop through each trip
  for (index in 1:num_trips) {
    # Filter to current trip
    current_trip <- trips[index]
    trip_df <- distance_df %>%
      dplyr::filter(trip_id_performed == current_trip) %>%
      dplyr::mutate(event_timestamp = as.numeric(event_timestamp)) %>%
      dplyr::arrange(event_timestamp)

    # Trajectory fitting
    if (interp_method == "linear") {
      # If fit a linear function
      current_fun <- stats::approxfun(x = trip_df$event_timestamp,
                                      y = trip_df$distance,
                                      method = "linear", rule = 1)
    } else {
      # Otherwise, use spline
      if (use_speeds) {
        # If using speeds, fit PCHIP
        current_fun <- stats::splinefunH(x = trip_df$event_timestamp,
                                         y = trip_df$distance,
                                         m = trip_df$speed)

      } else {
        # If not using speeds
        current_fun <- stats::splinefun(x = trip_df$event_timestamp,
                                        y = trip_df$distance,
                                        method = interp_method)
      }
    }

    # Saving fit trajectory
    traj_functions[[current_trip]] <- current_fun
  }

  # Calculate inverse functions
  # Must occur outside for loop
  if (find_inverse_function) {
    # If calculating inverse function
    traj_inv_functions <- lapply(names(traj_functions), function(trip_id_performed) {
      # Set bounds
      lwr <- trip_bounds$min_time[which(trip_bounds$trip_id_performed == trip_id_performed)]
      upr <- trip_bounds$max_time[which(trip_bounds$trip_id_performed == trip_id_performed)]
      # Find inverse
      get_inverse_traj(f = traj_functions[[trip_id_performed]],
                       lower = lwr, upper = upr, inv_tol = inv_tol)
    })
    names(traj_inv_functions) <- names(traj_functions)
  } else {
    # Otherwise, set to NULL
    traj_inv_functions <- NULL
    inv_tol <- NULL
  }

  # If grouping into a single function
  if (return_group_function) {
    # Create object
    grouped_traj <- new_avltrajectory_group(trip_id_performed = trips,
                                            traj_fun = traj_functions,
                                            inv_traj_fun = traj_inv_functions,
                                            min_dist = trip_bounds$min_dist,
                                            max_dist = trip_bounds$max_dist,
                                            min_time = trip_bounds$min_time,
                                            max_time = trip_bounds$max_time,
                                            traj_type = interp_method,
                                            inv_tol = inv_tol,
                                            max_deriv = max_deriv,
                                            used_speeds = use_speeds)

    return(grouped_traj)
  } else {
    # If not grouping, create list of individual
    # Must occur outside for loop bc inverse calculation must occur outside for loop
    single_traj_list <- lapply(names(traj_functions), function(trip_id_performed) {
      # Get index
      current_index <- which(trip_bounds$trip_id_performed == trip_id_performed)
      # Create object
      current_obj <- new_avltrajectory_single(trip_id_performed = trip_id_performed,
                                              traj_fun = traj_functions[[trip_id_performed]],
                                              inv_traj_fun = traj_inv_functions[[trip_id_performed]],
                                              min_dist = trip_bounds$min_dist[current_index],
                                              max_dist = trip_bounds$max_dist[current_index],
                                              min_time = trip_bounds$min_time[current_index],
                                              max_time = trip_bounds$max_time[current_index],
                                              traj_type = interp_method,
                                              max_deriv = max_deriv,
                                              inv_tol = inv_tol,
                                              used_speeds = use_speeds)
      return(current_obj)
    })
    names(single_traj_list) <- names(traj_functions)
    return(single_traj_list)
  }
}

#' Fits a continuous function of distance versus time.
#'
#' Uses scheduled stop times (via `stop_times`) to fit an interpolating scheduled trajectory.
#' Trajectories will be created for each trip on each day, referenced to the time during that day.
#' Operates via get_trajectory_fun(). See this for more details.
#'
#' @param route_gtfs A GTFS for a single route.
#' @param shape_geometry Optional. The SF (not shape_id) to use for distances. If NULL, each trip will be referenced to its assigned shape. Default is NULL.
#' @param project_crs Optional. A CRS numeric. The projection to use when performing spatial calculations (stops to routes). Consider setting to a Euclidian projection, such as the appropriate UTM zone. Default is 4326 (WGS 84 ellipsoid).
#' @param date_min Optional. A date object. The earliest date, in calendar.txt, to create a trip trajectory for. Default is NULL, where the first date in calendar.txt will be used.
#' @param date_max Optional. A date object. The latest date, in calendar.txt, to create a trip trajectory for. Default is NULL, where the final date in calendar.txt will be used.
#' @param agency_timezone Optional. A timezone string. The appropriate timezone for the stop times. Default is NULL, where the timezone in agency.txt will be used.
#' @param use_stop_time Optional. A string. Which stop time column should be used for the timepoint? Must be one of "arrival" (use arrival_time), "departure" (use departure_time), or "both", (timepoints will be created at both the stop arrival and departure). Default is "departure".
#' @param add_stop_dwell Optional. A numeric. If using both arrival and departure times, but they are scheduled to be equal (i.e., no dwell), how many seconds of dwell should be added? This will adjust forward the departure_time. Default is 0.
#' @param add_distance_error Optional. A numeric. If > 0, will correct for strict monotonicty, if desired. See make_monotonic() and get_trajectory_fun(). Default is 0.
#' @param interp_method Optional. A string, Which interpolation method should be used? See get_trajectory_fun(). Default is "linear".
#' @param inverse_fun Optional. A boolean, should the inverse trajectory function (time ~ distance) be returned? See get_trajectory_fun(). Default is FALSE.
#' @param return_single_fun Optional. A boolean, should a single function for all trips be returned (distance ~ f(trip, time))? See get_trajectory_fun(). Deafult is TRUE
#' @param inv_tol Optional. A numeric. What is the tolerance for numeric inverse calculations? See get_trajectory_fun(). Default is 0.01.
#' @return Either a single function, taking in trip, time, and derivative; or a list of single functions by trip, each taking in time and derivative.
get_gtfs_trajectory_fun <- function(route_gtfs, shape_geometry = NULL, project_crs = 4326,
                                    date_min = NULL, date_max = NULL, agency_timezone = NULL,
                                    use_stop_time = "departure", add_stop_dwell = 0, add_distance_error = 0,
                                    interp_method = "linear", find_inverse_function = TRUE, return_group_function = TRUE,
                                    inv_tol = 0.01) {

  # Get bounds for min & max date
  # Set to bounds of input data if not provided
  if (is.null(date_min)) {
    date_min <- min(as.Date(route_gtfs$calendar$date))
  }
  if (is.null(date_max)) {
    date_max <- max(as.Date(route_gtfs$calendar$date))
  }
  # If TZ not povided, pull from input GTFS
  if(is.null(agency_timezone)) {
    agency_timezone <- route_gtfs$agency$agency_timezone[1]
  }

  # Get stop distances
  stop_dist_df <- get_stop_distances(route_gtfs = route_gtfs,
                                     shape_geometry = shape_geometry,
                                     project_crs = project_crs)

  # Get time by desired schedule time
  if (use_stop_time == "departure") {
    # If using departure times, pull that
    trip_timepoints <- route_gtfs$stop_times %>%
      dplyr::arrange(trip_id, stop_sequence) %>%
      dplyr::select(trip_id, stop_id, departure_time) %>%
      tidyr::pivot_longer(cols = c("departure_time"),
                          names_to = "in_out",
                          values_to = "time")
  } else if (use_stop_time == "arrival") {
    # If using arrival times, pull that
    trip_timepoints <- route_gtfs$stop_times %>%
      dplyr::arrange(trip_id, stop_sequence) %>%
      dplyr::select(trip_id, stop_id, departure_time) %>%
      tidyr::pivot_longer(cols = c("arrival_time"),
                          names_to = "in_out",
                          values_to = "time")
  } else if (use_stop_time == "both") {
    # If using both, start by pulling dwell times
    # must make sure unique (time, distance) points -- if there are zero-second dwells, this won't be true
    trip_dwells <- route_gtfs$stop_times %>%
      dplyr::select(trip_id, stop_id, arrival_time, departure_time) %>%
      dplyr::mutate(dwell_time = as.numeric(difftime(departure_time, arrival_time, units = "secs")))

    # Calculate number of zero-second dwell times
    num_zero_dwells <- sum(trip_dwells$dwell_time == 0)
    if(num_zero_dwells > 0) {
      # If there are zero-second dwell times
      if(is.null(add_stop_dwell)) {
        # Stop dwell not provided
        stop("Zero-second stop dwells detected, but no stop dwell addition provided. Please either: change stop time method, or provide stop dwell time to add.")
      } else {
        # Use provided dwell time to adjust forward departure times
        trip_dwells_adj <- trip_dwells %>%
          dplyr::mutate(dwell_time = dplyr::if_else(condition = (dwell_time == 0),
                                                    true = add_stop_dwell,
                                                    false = dwell_time),
                        departure_time = hms::as_hms(as.numeric(arrival_time) + dwell_time))

        # Replace GTFS departure times with adjusted. Arrivals stay the same.
        # Correct in the provided GTFS object
        route_gtfs$stop_times$departure_time <- trip_dwells_adj$departure_time
      }
    }

    # Get corrected times (or uncorrected if it was not necessary)
    trip_timepoints <- route_gtfs$stop_times %>%
      dplyr::arrange(trip_id, stop_sequence) %>%
      dplyr::select(trip_id, stop_id, arrival_time, departure_time) %>%
      tidyr::pivot_longer(cols = c("arrival_time", "departure_time"),
                          names_to = "in_out",
                          values_to = "time")
  }

  # Get timetable, time-distance pairs
  trip_distances <- trip_timepoints %>%
    dplyr::left_join(y = (route_gtfs$trips %>% dplyr::select(trip_id, shape_id, service_id)),
                     by = "trip_id", relationship = "many-to-many") %>%
    dplyr::left_join(y = stop_dist_df, by = c("stop_id", "shape_id")) %>%
    dplyr::left_join(y = (route_gtfs$calendar %>% dplyr::select(service_id, date)),
                     by = "service_id", relationship = "many-to-many") %>%
    dplyr::mutate(trip_id = paste(date, trip_id, sep = "-"),
                  date = as.Date(date)) %>%
    # Filter to desired date range
    dplyr::filter((date >= date_min) & (date <= date_max)) %>%
    dplyr::mutate(hour_num = as.numeric(substr(time, start = 1, stop = 2)),
                  # If past midnight, increment date
                  date = dplyr::if_else(condition = (hour_num >= 24),
                                        true = (date + 1),
                                        false = date),
                  # If past midnight, adjust hour back down
                  hour_num = dplyr::if_else(condition = (hour_num >= 24),
                                            true = (hour_num - 24),
                                            false = hour_num),
                  # Format time string
                  time = paste(sprintf("%02d", hour_num), substr(time, start = 3, stop = 8),
                               sep = ""),
                  # Convert time string to date type
                  event_timestamp = as.POSIXct(paste(date, time, sep = " "),
                                               format = "%Y-%m-%d %H:%M:%S",
                                               tz = agency_timezone)) %>%
    dplyr::select(-c(date, hour_num, time)) %>%
    dplyr::rename(trip_id_performed = trip_id)

  # Correct for monotonicity
  if (add_distance_error > 0) {
    trip_distances <- make_monotonic(distance_df = trip_distances,
                                     correct_speed = FALSE,
                                     add_distance_error = add_distance_error)
  }

  # Get trajectory function object
  traj_funs <- get_trajectory_fun(distance_df = trip_distances,
                                  interp_method = interp_method,
                                  find_inverse_function = find_inverse_function,
                                  return_group_function = return_group_function,
                                  inv_tol = inv_tol,
                                  use_speeds = FALSE)
  return(traj_funs)
}
