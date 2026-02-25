#' Constructor for grouped trajectory class
#'
#' This superclass holds a single, combined trajectory function. Plus vectors of
#' all trip IDs described by that function, and vectors of the time & distance
#' ranges of those trips. Not intended for external use.
#'
#' @param trip_id_performed Character if trip ids.
#' @param traj_fun List or single trajectory functions.
#' @param inv_traj_fun List or single inverse trajectory functions.
#' @param min_dist Vector if minimum distance values.
#' @param max_dist Vector of maximum distance values.
#' @param min_time Vector of minimum time values.
#' @param max_time Vector of maximum time values.
#' @param traj_type Interp method character string
#' @param inv_tol Tolerance used in numeric inverse
#' @param max_deriv Max derivative allowed
#' @param used_speeds Whether speeds were used
#' @param ... Other inputs
#' @param class Object class
#' @return Grouped trajectory object
#' @keywords internal
new_avltrajectory_group <- function(trip_id_performed = character(),
                                    traj_fun, inv_traj_fun = NULL,
                                    min_dist, max_dist, min_time, max_time,
                                    traj_type, inv_tol = NULL, max_deriv = 0,
                                    used_speeds = FALSE, agency_tz,
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
            used_speeds = used_speeds,
            agency_tz = agency_tz)
}

#' Constructor for single trajectory class
#'
#' This is a subclass (special case) of a grouped trajectory, in which there is
#' only one trip. Trajectory function is inteded to describe only one trip.
#' Associated trip properties (ID & ranges) should describe only that trip. Not
#' intended for external use.
#'
#' @inheritParams new_avltrajectory_group
#' @return Single trajectory object
#' @keywords internal
new_avltrajectory_single <- function(trip_id_performed = character(),
                                     traj_fun, inv_traj_fun = NULL,
                                     min_dist, max_dist, min_time, max_time,
                                     traj_type, inv_tol = NULL, max_deriv = 0,
                                     used_speeds = FALSE, agency_tz) {

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
                          agency_tz = agency_tz,
                          class = "avltrajectory_single")
}

#' Fits continuous trajectory interpolating curves from transit AVL data.
#'
#' @description
#' This function fits a continuous vehicle trajectory function to observed AVL
#' points, returning a trajectory object. Interpolation can be done linearly
#' (`interp_method = "linear"`), or via any method supported by
#' `stats::splinefun()`.
#'
#' @details
#'
#' ## Interpolating Methods
#'
#' The goal of this function is to fit a continuous function representing a
#' vehicle's distance traveled as a function of time, for each trip. This
#' function supports to types of interpolating curves:
#'
#' - Linear interpolation, for `interp_method = "linear"`. This will fit a
#' simple linear function, ignorant of recorded `speed` values.
#'
#' - Spline interpolation, for `interp_method` set to any method supported by
#' `stats::splinefun()` (i.e., `"fmm"`, `"natural"`, `"periodic"`, `"monoH.FC"`,
#' or `"hyman"`.) Only `interp_method = "monoH.FC"` supports use of recorded
#' `speed` values.
#'
#' By default, `interp_method = "monoH.FC"` and `use_speeds = TRUE`. This will
#' yield a continuous, differentiable, monotonic, and invertible trajectory,
#' and is the standard transit trajectory interpolation technique. If the input
#' `distance` and `speed` values satisfy Fritsch-Carlson, the interpolating
#' function is guaranteed to be montonic. See `make_monotonic()` and
#' `validate_monotonicity()`.
#'
#' Note that `use_speeds = TRUE` requires `interp_method = "monoH.FC"`, but
#' `interp_method = "monoH.FC"` does not require `use_speeds = TRUE`. In this
#' scenario, a "velocity-ignorant' Fritsch-Carlson interpolating function
#' can be created. If input `distance` values are monotonic, this curve is
#' guaranteed to be monotonic.
#'
#' ## Inverse Functions
#'
#' Often times, we are concerned not with the position of a vehicle at a
#' particular time, but when a vehicle crosses a specific point in space. This
#' can be accomplished by computing an inverse trajectory function. If
#' `find_inverse_function = TRUE` (the default), a numeric inverse to the fit
#' trajectory function will be found, with a tolerance controlled by `inv_tol`.
#'
#' Because the inverse function is numerical, it can be found for any type of
#' interpolating curve (linear or spline). However, the input data must be
#' strictly monotonic for the trajectory curve to be invertible. If
#' `find_inverse_function = TRUE`, this will be verified before proceeding (see
#' `validate_monotonicity()`).
#'
#' ## The Trajectory Object
#'
#' A trajectory function does not exist by itself; rather, it requires the
#' context about the trip it describes, as well as its inverse function. As
#' such, `get_trajectory_fun()` returns an AVL trajectory object. If
#' `return_group_function = TRUE` (the default), the function will return a
#' single object containing:
#'
#' - A vector of `trip_id_performed`s present in `distance_df`.
#'
#' - A list of fit trajectory functions, indexed by their `trip_id_performed`.
#'
#' - A list of fit inverse trajectory functions, indexed by their
#' `trip_id_performed`.
#'
#' - Information about how the trajectory and inverse trajectory functions were
#' fit, including `interp_method`, `use_speeds`, and `inv_tol`.
#'
#' - A vector each for the minimum distances, maximum distances, minimum times,
#' and maximum times of each trip. These inform the domain and range of the
#' trajectory function and its inverse, preventing extrapolation beyond the
#' time or distance range actually served by a trip.
#'
#' Alternatively, if `return_group_function = FALSE`, a separate trajectory
#' object will be fit for each trip. `get_trajectory_fun()` will return a list
#' of trajectory objects indexed by their `trip_id_performed`.
#'
#' More information about the trajectory object classes and how to use them is
#' available at (xyz).
#'
#' @param distance_df A dataframe of linearized AVL data. Must include
#' `trip_id_performed`, `event_timestamp`, and `distance`. If
#' `use_speed = TRUE`, must also include `speed`.
#' @param interp_method Optional. The type of interpolation function to be fit.
#' Either `"linear"`, or a spline method from `stats::splinefun()`. Default is
#' `"monoH.FC"`.
#' @param use_speeds Optional. A boolean, should curves be constrained by
#' observed AVL speeds? Should only be used with `interp_method = "monoH.FC"`,
#' but `monoH.FC` does not require speeds. Default is `TRUE`.
#' @param return_group_function Optional. A boolean, should the returned
#' trajectory object be grouped into a single function? If FALSE, will return a
#' list (indexed by `trip_id_performed`) of single trajectory objects. Default
#' is `TRUE`.
#' @param find_inverse_function Optional. A boolean, should the numeric inverse
#' function (time ~ distance) be calculated? Default is `TRUE`.
#' @param inv_tol Optional. A numeric in the units of input `distance`, the
#' tolerance used when calculating the numeric inverse function. Default is
#' 0.01.
#' @return If `return_group_function = TRUE`, a grouped trajectory object. If
#' `FALSE`, a list of single trajectory objects, index by their
#' `trip_id_performed`.
#' @export
get_trajectory_fun <- function(distance_df,
                               interp_method = "monoH.FC", use_speeds = TRUE,
                               find_inverse_function = TRUE, inv_tol = 0.01,
                               return_group_function = TRUE) {

  # --- Validation of all inputs ---
  # Fields
  if (use_speeds) {
    needed_fields <- c("trip_id_performed", "event_timestamp", "distance",
                       "speed")
  } else {
    needed_fields <- c("trip_id_performed", "event_timestamp", "distance")
  }
  validate_input_to_tides(needed_fields, distance_df)
  # Monotonicty -- must be strict if finding inverse
  mono_check <- validate_monotonicity(distance_df,
                                      check_speed = TRUE)
  if (use_speeds) { # If using speeds, check all three bools; otherwise, only need first two
    max_mono_check <- 3
  } else {
    max_mono_check <- 2
  }
  checked_conditions <- mono_check[1:max_mono_check]
  if (find_inverse_function) {
    # If finding an inverse function, must error if montonicity not met
    if (!all(checked_conditions)) {
      stop(paste(c("The following monotonicity conditions are not satisfied:",
                   names(checked_conditions)[!checked_conditions],
                   "\nMonotonicity required inverse function."),
                 collapse = " "))
    }
  } else {
    # If not finding inverse function, can proveed but will give warning
    if (!all(checked_conditions)) {
      warning(paste(c("The following monotonicity conditions are not satisfied:",
                      names(checked_conditions)[!checked_conditions],
                      "\nMonotonicity not required for non-inverse function. Proceeding with direct function fitting."),
                    collapse = " "))
    }
  }
  # Methods
  if (use_speeds) { # If using speeds
    if (interp_method != "monoH.FC") {
      # If method is monoH.FC
      if (interp_method == "linear") {
        warning("Speeds cannot be used for linear interpolation. Ignoring speeds and performing linear interpolation.")
      } else {
        warning("Using speeds for spline interpolation requires method monoH.FC. monoH.FC will be used unless use_speeds set to FALSE.")
      }
    }
  }

  # Set derivative
  if (interp_method == "linear") {
    # If linear
    max_deriv = 0
  } else {
    # If spline
    max_deriv = 3
  }

  # Get timezone
  current_tz <- attr(distance_df$event_timestamp, "tzone")

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
                                            used_speeds = use_speeds,
                                            agency_tz = current_tz)

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
                                              used_speeds = use_speeds,
                                              agency_tz = current_tz)
      return(current_obj)
    })
    names(single_traj_list) <- names(traj_functions)
    return(single_traj_list)
  }
}

#' Fits continuous trajectory interpolating curves from GTFS schedule data.
#'
#' @description
#' This function fits a continuous vehicle trajectory function to scheduled GTFS
#' `stop_times`, returning a trajectory object. Interpolation can be done
#' linearly (`interp_method = "linear"`), or via any method supported by
#' `stats::splinefun()`.
#'
#' @details
#'
#' ## Stops, Dwells, and Monotonicity
#'
#' To fit an interpolating trajectory function, each observation must include
#' distance and timestamp pairs throughout each trip. While `stop_times` does
#' include a `shape_dist_traveled` field, this is optional and often left
#' empty by agencies. Additionally, small distortions in spatial projections
#' mean that projected GPS points may not align perfectly with the agency's
#' calculated `shape_dist_traveled`. As such, this function uses
#' `get_stop_distances()` to get the distance of each stop along each shape for
#' each trip. Alternatively, all stops and trips can be referenced to the
#' same spatial feature using `shape_geometry`. Consider setting `project_crs`
#' to the same spatial projection used to linearize AVL GPS points.
#'
#' The trajectory functions are fit using the times a trip is scheduled to
#' serve each stop. There is some ambiguity here: should a stop's timestamp
#' be when the vehicle arrives, or departs? This can be controlled using
#' `use_stop_time`, set to `"departure"` for `departure_time`, `"arrival"` for
#' `arrival_time`, or `"both"` to include both `departure_time` and
#' `arrival_time` as distinct observations (i.e., distance & timestamp pairs).
#'
#' Often times, however, a GTFS schedule will not have different `arrival_time`
#' and `departure_time` values, especially if the timetable was not developed
#' considering stop-level dwell times. In this scenario, it may be best to use
#' only one of `departure_time` or `arrival_time`. If a dwell is desired, use
#' `add_stop_dwell` to simulate a dwell time at each stop. This will increase
#' the `departure_time` by the number of seconds specified.
#'
#' Adding dwells opens a new consideration, however: the trajectory will no
#' longer be strictly monotonic, as the vehicle will hold at a constant distance
#' for some period of time. This is only a concern if
#' `find_inverse_function = TRUE`, which requires strictly monotonic input data.
#' If both dwell times and an inverse function are desired, consider setting
#' `add_distance_error > 0` to restore strict monotonicity. See
#' `make_monotonic()` for more details.
#'
#' ## Interpolating Methods
#'
#' The goal of this function is to fit a continuous function representing a
#' GTFS trip's scheduled distance traveled as a function of time. This
#' function supports to types of interpolating curves:
#'
#' - Linear interpolation, for `interp_method = "linear"`. This will fit a
#' simple linear function, ignorant of recorded `speed` values.
#'
#' - Spline interpolation, for `interp_method` set to any method supported by
#' `stats::splinefun()` (i.e., `"fmm"`, `"natural"`, `"periodic"`, `"monoH.FC"`,
#' or `"hyman"`.)
#'
#' By default, `interp_method = linear`, and linear interpolation is the
#' recommended method for schedule trajectories. This is because timetable
#' development typically assumes a constant running speed over a corridor, so
#' linearly connecting stop times will best reflect a trip's scheduled
#' trajectory.
#'
#' ## Inverse Functions
#'
#' Often times, we are concerned not with the position of a vehicle at a
#' particular time, but when a vehicle crosses a specific point in space. This
#' can be accomplished by computing an inverse trajectory function. If
#' `find_inverse_function = TRUE` (the default), a numeric inverse to the fit
#' trajectory function will be found, with a tolerance controlled by `inv_tol`.
#'
#' Because the inverse function is numerical, it can be found for any type of
#' interpolating curve (linear or spline). However, the input data must be
#' strictly monotonic for the trajectory curve to be invertible. If
#' `find_inverse_function = TRUE`, this will be verified before proceeding (see
#' `validate_monotonicity()`).
#'
#' ## The Trajectory Object
#'
#' A trajectory function does not exist by itself; rather, it requires the
#' context about the trip it describes, as well as its inverse function. As
#' such, `get_trajectory_fun()` returns an AVL trajectory object. If
#' `return_group_function = TRUE` (the default), the function will return a
#' single object containing:
#'
#' - A vector of `trip_id_performed`s, from the `trip_id`s found in `trips`.
#'
#' - A list of fit trajectory functions, indexed by their `trip_id_performed`.
#'
#' - A list of fit inverse trajectory functions, indexed by their
#' `trip_id_performed`.
#'
#' - Information about how the trajectory and inverse trajectory functions were
#' fit, including `interp_method`, `use_speeds`, and `inv_tol`.
#'
#' - A vector each for the minimum distances, maximum distances, minimum times,
#' and maximum times of each trip. These inform the domain and range of the
#' trajectory function and its inverse, preventing extrapolation beyond the
#' time or distance range actually served by a trip.
#'
#' Alternatively, if `return_group_function = FALSE`, a separate trajectory
#' object will be fit for each trip. `get_trajectory_fun()` will return a list
#' of trajectory objects indexed by their `trip_id_performed`.
#'
#' More information about the trajectory object classes and how to use them is
#' available at (xyz).
#'
#' @inheritParams get_stop_distances
#' @inheritParams get_trajectory_fun
#' @inheritParams make_monotonic
#' @param date_min Optional. A date object. The earliest date in
#' `calendar.txt` to create a trip trajectory for. Default is `NULL`, where the
#' first date in `calendar.txt` will be used.
#' @param date_max Optional. A date object. The latest date in
#' `calendar.txt` to create a trip trajectory for. Default is `NULL`, where the
#' last date in `calendar.txt` will be used.
#' @param agency_timezone Optional. A timezone string (see `OlsonNames()`)
#' indicating he appropriate timezone for the stop times. Default is `NULL`,
#' where the timezone in `agency.txt` will be used.
#' @param use_stop_time Optional. A string, which stop time column should be
#' used for the timepoint? Must be one of `"arrival"` (use `arrival_time`),
#' `"departure"` (use `departure_time`), or `"both"`,
#' (timepoints will be created at both the stop arrival and departure). Default
#' is `"departure"`.
#' @param add_stop_dwell Optional. A numeric. If `use_stop_time = "both"`,
#' but scheduled arrival and departure times are equal (i.e., no dwell), how
#' many seconds of dwell should be added? This will adjust forward the
#' `departure_time`. Default is 0.
#' @param interp_method Optional. The type of interpolation function to be fit.
#' Either `"linear"`, or a spline method from `stats::splinefun()`. Default is
#' `"linear"`.
#' @return If `return_group_function = TRUE`, a grouped trajectory object. If
#' `FALSE`, a list of single trajectory objects, index by their
#' `trip_id_performed`.
get_gtfs_trajectory_fun <- function(gtfs,
                                    shape_geometry = NULL, project_crs = 4326,
                                    date_min = NULL, date_max = NULL,
                                    agency_timezone = NULL,
                                    use_stop_time = "departure",
                                    add_stop_dwell = 0, add_distance_error = 0,
                                    interp_method = "linear",
                                    find_inverse_function = TRUE,
                                    return_group_function = TRUE,
                                    inv_tol = 0.01) {

  # --- Validate GTFS ---
  # Only need to validate the files & fields used by this function uniquely
  # Others will be validated in get_stop_distances()
  if (!("tidygtfs" %in% class(gtfs))) {
    rlang::abort(message = "Provided GTFS not a tidygtfs object.",
                 class = "error_gtfsval_not_tidygtfs")
  }
  # calendar: service_id, date
  validate_gtfs_input(gtfs,
                      table = "calendar",
                      needed_fields = c("date", "service_id"))
  # stop_times: trip_id, stop_id, stop_sequence; others depend on timepoint used
  if (use_stop_time == "departure") {
    stop_times_fields <- c("trip_id", "stop_id", "stop_sequence",
                           "departure_time")
  } else if (use_stop_time == "arrival") {
    stop_times_fields <- c("trip_id", "stop_id", "stop_sequence",
                           "arrival_time")
  } else if (use_stop_time == "both") {
    stop_times_fields <- c("trip_id", "stop_id", "stop_sequence",
                           "departure_time", "arrival_time")
  } else {
    rlang::abort(message = "Input use_stop_time not recognized. Please input \"departure\", \"arrival\", or \"both\".",
                 class = "error_gtfstraj_stoptime")
  }
  validate_gtfs_input(gtfs,
                      table = "stop_times",
                      needed_fields = stop_times_fields)

  # trips: service_id (shape_id, trip_id will be validated by get_stop_distances())
  validate_gtfs_input(gtfs,
                      table = "trips",
                      needed_fields = c("service_id"))

  # Get bounds for min & max date
  # Set to bounds of input data if not provided
  if (is.null(date_min)) {
    date_min <- min(as.Date(gtfs$calendar$date))
  }
  if (is.null(date_max)) {
    date_max <- max(as.Date(gtfs$calendar$date))
  }
  # If TZ not povided, pull from input GTFS
  if(is.null(agency_timezone)) {
    agency_timezone <- gtfs$agency$agency_timezone[1]
  }

  # Get stop distances
  stop_dist_df <- get_stop_distances(gtfs = gtfs,
                                     shape_geometry = shape_geometry,
                                     project_crs = project_crs)

  # Get time by desired schedule time
  if (use_stop_time == "departure") {
    # If using departure times, pull that
    trip_timepoints <- gtfs$stop_times %>%
      dplyr::arrange(trip_id, stop_sequence) %>%
      dplyr::select(trip_id, stop_id, departure_time) %>%
      tidyr::pivot_longer(cols = c("departure_time"),
                          names_to = "in_out",
                          values_to = "stp_time")
  } else if (use_stop_time == "arrival") {
    # If using arrival times, pull that
    trip_timepoints <- gtfs$stop_times %>%
      dplyr::arrange(trip_id, stop_sequence) %>%
      dplyr::select(trip_id, stop_id, departure_time) %>%
      tidyr::pivot_longer(cols = c("arrival_time"),
                          names_to = "in_out",
                          values_to = "stp_time")
  } else if (use_stop_time == "both") {
    # If using both, start by pulling dwell times
    # must make sure unique (time, distance) points -- if there are zero-second dwells, this won't be true
    trip_dwells <- gtfs$stop_times %>%
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
        gtfs$stop_times$departure_time <- trip_dwells_adj$departure_time
      }
    }

    # Get corrected times (or uncorrected if it was not necessary)
    trip_timepoints <- gtfs$stop_times %>%
      dplyr::arrange(trip_id, stop_sequence) %>%
      dplyr::select(trip_id, stop_id, arrival_time, departure_time) %>%
      tidyr::pivot_longer(cols = c("arrival_time", "departure_time"),
                          names_to = "in_out",
                          values_to = "stp_time")
  }

  # Get timetable, time-distance pairs
  trip_distances <- trip_timepoints %>%
    dplyr::left_join(y = (gtfs$trips %>% dplyr::select(trip_id, shape_id, service_id)),
                     by = "trip_id", relationship = "many-to-many") %>%
    dplyr::left_join(y = stop_dist_df, by = c("stop_id", "shape_id")) %>%
    dplyr::left_join(y = (gtfs$calendar %>% dplyr::select(service_id, date)),
                     by = "service_id", relationship = "many-to-many") %>%
    dplyr::mutate(trip_id = paste(date, trip_id, sep = "-"),
                  date = as.Date(date)) %>%
    # Filter to desired date range
    dplyr::filter((date >= date_min) & (date <= date_max)) %>%
    dplyr::mutate(hour_num = as.numeric(substr(stp_time, start = 1, stop = 2)),
                  # If past midnight, increment date
                  date = dplyr::if_else(condition = (hour_num >= 24),
                                        true = (date + 1),
                                        false = date),
                  # If past midnight, adjust hour back down
                  hour_num = dplyr::if_else(condition = (hour_num >= 24),
                                            true = (hour_num - 24),
                                            false = hour_num),
                  # Format stp_time string
                  stp_time = paste(sprintf("%02d", hour_num), substr(stp_time, start = 3, stop = 8),
                               sep = ""),
                  # Convert time string to date type
                  event_timestamp = as.POSIXct(paste(date, stp_time, sep = " "),
                                               format = "%Y-%m-%d %H:%M:%S",
                                               tz = agency_timezone)) %>%
    dplyr::select(-c(date, hour_num, stp_time)) %>%
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
