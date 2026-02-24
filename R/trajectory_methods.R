#' Summary function for AVL trajectories.
#'
#' @description
#' This function prints a summary for grouped or single trajectory object
#' If the input is a single trajectory, the trip's ID and distance & time range
#' will be printed. If the input is a grouped trajectory, the number of trips
#' and the distance & time range across all trips will be printed. For both,
#' the interpolating curve methods will be printed.
#'
#' @param object A single or grouped trajectory object.
#' @param ... Other parameters (not used).
#' @return A summary character string.
#' @export
summary.avltrajectory_group <- function(object, ...) {
  num_trips <- length(object)
  min_dist <- min(attr(object, "min_dist"))
  max_dist <- max(attr(object, "max_dist"))
  min_time <- min(attr(object, "min_time"))
  max_time <- max(attr(object, "max_time"))

  is_traj <- is.function(attr(object, "traj_fun")[[1]])
  traj_type <- attr(object, "traj_type")
  max_deriv <- attr(object, "max_deriv")
  is_inv <- is.function(attr(object, "inv_traj_fun")[[1]])
  inv_tol <- attr(object, "inv_tol")
  used_speeds <-attr(object, "used_speeds")

  response <- cat("------",
                  "\nAVL Group Trajectory Object",
                  "\n------",
                  "\nNumber of trips: ", num_trips,
                  "\nTotal distance range: ", min_dist, " to ", max_dist,
                  "\nTotal time range: ", min_time, " to ", max_time,
                  "\n------",
                  "\nTrajectory function present: ", is_traj,
                  "\n   --> Trajectory interpolation method: ", traj_type,
                  "\n   --> Maximum derivative: ", max_deriv,
                  "\n   --> Fit with speeds: ", used_speeds,
                  "\nInverse function present: ", is_inv,
                  "\n   --> Inverse function tolerance: ", inv_tol,
                  "\n------",
                  sep = "")

  invisible(object)
}

#' @rdname summary.avltrajectory_group
#' @export
summary.avltrajectory_single <- function(object, ...) {
  trip_id <- unclass(object)
  min_dist <- attr(object, "min_dist")
  max_dist <- attr(object, "max_dist")
  min_time <- attr(object, "min_time")
  max_time <- attr(object, "max_time")

  is_traj <- is.function(attr(object, "traj_fun"))
  traj_type <- attr(object, "traj_type")
  max_deriv <- attr(object, "max_deriv")
  is_inv <- is.function(attr(object, "inv_traj_fun"))
  inv_tol <- attr(object, "inv_tol")
  used_speeds <- attr(object, "used_speeds")

  response <- cat("------",
                  "\nAVL Single Trajectory Object",
                  "\n------",
                  "\nTrip ID: ", trip_id,
                  "\nTrip distance range: ", min_dist, " to ", max_dist,
                  "\nTrip time range: ", min_time, " to ", max_time,
                  "\n------",
                  "\nTrajectory function present: ", is_traj,
                  "\n   --> Trajectory interpolation method: ", traj_type,
                  "\n   --> Maximum derivative: ", max_deriv,
                  "\n   --> Fit with speeds: ", used_speeds,
                  "\nInverse function present: ", is_inv,
                  "\n   --> Inverse function tolerance: ", inv_tol,
                  "\n------",
                  sep = "")

  invisible(object)
}

#' Print function for AVL trajectories
#'
#' @description
#' This function prints a one-line report for grouped or single trajectory
#' objects. For a single trajectory, the trip ID will be printed. For grouped
#' trajectories, the number of trips will be printed.
#'
#' @param x A single or grouped trajectory object.
#' @param ... Other parameters (not used).
#' @return A printing character string.
#' @export
print.avltrajectory_group <- function(x, ...) {
  print(paste("AVL group trajectory with ", length(x), " trips.",
              sep = ""))
}

#' @rdname print.avltrajectory_group
#' @export
print.avltrajectory_single <- function(x, ...) {
  print(paste("AVL single trajectory for trip ID ", unclass(x),
              sep = ""))
}

#' Interpolate time or distance points using AVL trajectories.
#'
#' @description
#' Using a function stored in a grouped or single trajectory object, new points
#' will be interpolated along a trajectory. Depending on whether new_times or
#' new_distances is provided, the function will utilize the direct or inverse
#' trajectory function.
#'
#' @details
#' This function is the recommended way to use a fit trajectory function. It has
#' a few key features:
#'
#' ## Interpolate for Distance or Time
#'
#' If `new_times` is provided, the function will find the `distance` of each
#' trip at each new time using the direct trajectory function. Conversely, if
#' `new_distances` is provided, the function will find the `event_timestamp` at
#' which each trip crossed that distance using the inverse trajectory function.
#' If an input trajectory object does not contain an inverse function, an error
#' will be thrown.
#'
#' These new points can be either vectors or dataframes:
#'
#' - If the input is a vector, the returned value will be a dataframe
#' associating each trip with each new point and a column `interp` indicating
#' the interpolated value.
#'
#' - If the input is a dataframe, it must contain a column names
#' `event_timestamp` or `distance`, depending on whether it is put into
#' `new_times` or `new_distances`. All other columns will be preserved in the
#' output. Each row will be duplicated for each trip, and a column `interp` will
#' be added indicating the interpolated value.
#'
#' The latter option is particularly useful for finding crossing times of
#' particular features, or the positions at notable points in time. For
#' instance, `new_distance` may be a dataframe of corridors, with a column
#' `name` for the corridor name and `inout` for whether each row is the
#' entrance or exit to the corridor. The returned value will append the column
#' `trip_id_performed` for each trip that crosses those distances, and `interp`
#' for the time the trip passes the entrance or exit.
#'
#' ## Finding Derivatives
#'
#' Depending on the `interp_method` used when fitting the trajectory object, a
#' its derivative may be able to be found:
#'
#' - `interp_method = "linear"`. This will not allow derivatives. This is
#' because, at each observation, the piecewise linear function is not
#' differentiable.
#'
#' - `interp_method` is a spline from `stats::splinefun()`. This will typically
#' be differentiable up to the third degree.
#'
#' The derivative returned (as column `interp`) is the derivative of distance
#' with respect to time. This means the first derivative is velocity, second is
#' acceleration, and third is jerk. The derivative is taken from the direct
#' trajectory, not the inverse, and the inverse trajectory cannot be used to
#' find derivatives. This means that if `new_distances` is provided, `deriv `
#' must equal 0. If starting from distance values, but derivatives are desired,
#' consider interpolating for timepoints first, then using these as `new_times`
#' to find the derivative.
#'
#' ## Prevents Extrapolation
#'
#' By default, many fit interpolating curves will allow extrapolation (i.e.,
#' the input of an `event_timestamp` beyond the original time domain of the
#' trip). This is especially true for splines fit using `stats::splinefun()`.
#' In general, this will not be reasonable for transit vehicles: time points
#' should be constrained by the time that a trip has actually been observed,
#' and distances should be constrained to the part of a route a trip actually
#' ran.
#'
#' This function uses the maximum and minimum time and distance values stored
#' in the trajectory object to identify if an input `new_times` or
#' `new_distances` is beyond the domain/range of each trip individually. The
#' returned output will only include `interp` values for trips within the
#' domain/range of the input.
#'
#' ## Accessing the Raw Trajectory Function
#'
#' Because of the above features and protections, it is recommend that these
#' `predict()` functions are used to access the fit trajectory and inverse
#' trajectory functions. However, if the raw function itself is desired,
#' it can be accessed using `attr(trajectory, "traj_fun")` or
#' `attr(trajectory, "inv_traj_fun")`. For a group trajectory object, these
#' will return lists of individual trip functions indexed by
#' `trip_id_performed`; for single trajectory objects, these will return the
#' single function for that trip.
#'
#' @param object The single or grouped trajectory object.
#' @param new_times Optional. A vector of numeric timepoints, or a dataframe
#' with at least the column `"event_timestamp"` of new timepoints to interpolate
#' at. Default is `NULL`.
#' @param new_distances Optional. A vector of numeric distances, or a dataframe
#' with at least the column `"distance"` of new distances to interpolate at.
#' Default is `NULL`.
#' @param deriv Optional. The derivative with which to calculate at. Default is
#' 0.
#' @param trips Optional. A vector of `trip_id_performed`s to interpolate for.
#' Default is `NULL`, which will use all trips found in the trajectory object.
#' @param ... Other parameters (not used).
#' @return The input dataframe, with an additional column `"interp"` of the
#' interpolated values requested, and an additional `"trip_id_performed"`
#' column will all trips for which that point is within range.
#' @export
predict.avltrajectory_group <- function(object, new_times = NULL, new_distances = NULL,
                                        deriv = 0, trips = NULL, ...) {

  # Check DFs provided
  if ((!is.null(new_times)) & (!is.null(new_distances))) {
    # Only allow one of time or distance
    stop("Please provide only one of new_times or new_distances.")
  } else if (is.null(new_times) & is.null(new_distances)) {
    # Require one of new_times or new_distances
    stop("Please provide one of new_times or new_distances")
  }

  all_trips <- unclass(object)
  # Check for trips
  if (is.null(trips)) {
    # If not provided, use all
    trips <- all_trips
  } else {
    # If trips are provided, check that they are in traj functions
    trips_check <- trips %in% all_trips
    if (!all(trips_check)) {
      # If at least one trip is not supported by the function
      stop(paste("The following requested trips are not in this trajectory function:\n",
                 trips[!trips_check], sep = ""))
    }
  }

  # Get required data for all methods
  # Dist & time extremes
  trip_extremes <- data.frame(trip_id_performed = all_trips,
                              min_dist = attr(object, "min_dist"),
                              max_dist = attr(object, "max_dist"),
                              min_time = attr(object, "min_time"),
                              max_time = attr(object, "max_time")) %>%
    dplyr::filter(trip_id_performed %in% trips)

  if (!is.null(new_times)) {
    # If interpolating for distance

    # Convert data types
    if (is.vector(new_times)) {
      # If a vector is provided, convert it to dataframe
      new_times_df <- data.frame(event_timestamp = new_times)
    } else if (is.data.frame(new_times)) {

      # If a DF is provided
      if (!("event_timestamp" %in% names(new_times))) {
        # Check if new_times has needed column
        stop("Please provide event_timestamp column in new_times")
      } else {
        new_times_df <- new_times
      }
    } else {
      stop("Please provide either a vector or dataframe of new_times.")
    }

    if (deriv > attr(object, "max_deriv")) {
      # Check if derivative is above function's allowed limit
      stop(paste("Derivative too high. Maximum for this function is ",
                 attr(object, "max_deriv"), sep = ""))
    }

    # Function
    trajectory_function <- attr(object, "traj_fun")

    # Call internal function to do interpolation
    interpolate_distances_group(trip_extremes = trip_extremes,
                                new_times = new_times_df,
                                trajectory_function = trajectory_function,
                                deriv = deriv)
  } else {
    # If interpolating for times

    # Convert data types
    if (is.vector(new_distances)) {
      # If a vector is provided, convert it to dataframe
      new_distances_df <- data.frame(distance = new_distances)
    } else if (is.data.frame(new_distances)) {

      # If a DF is provided
      if (!("distance" %in% names(new_distances))) {
        # Check if new_distances has needed column
        stop("Please provide distance column in new_distances")
      } else {
        new_distances_df <- new_distances
      }
    } else {
      stop("Please provide either a vector or dataframe of new_distances.")
    }

    if (deriv > 0) {
      # Check if derivative provided
      stop("Derivative not allowed for inverse function. Please find timepoints first, then derivatives at timepoints.")
    }

    # Get inverse trajectory function
    inv_trajectory_function <- attr(object, "inv_traj_fun")
    if (is.null(inv_trajectory_function)) {
      # Check that inverse function actually exists
      stop("Trajectory object does not contain inverse function. Please create one using get_trajectory_function().")
    }

    # Call internal function to do interpolation
    interpolate_times_group(trip_extremes = trip_extremes,
                            new_distances = new_distances_df,
                            inv_trajectory_function = inv_trajectory_function)
  }
}

#' @rdname predict.avltrajectory_group
#' @export
predict.avltrajectory_single <- function(object, new_times = NULL, new_distances = NULL,
                                         deriv = 0, ...) {

  # Check DFs provided
  if ((!is.null(new_times)) & (!is.null(new_distances))) {
    # Only allow one of time or distance
    stop("Please provide only one of new_times or new_distances.")
  } else if (is.null(new_times) & is.null(new_distances)) {
    # Require one of new_times or new_distances
    stop("Please provide one of new_times or new_distances")
  }

  # Get required data for all methods
  # Dist & time extremes
  trip_extremes <- c("min_dist" = attr(object, "min_dist"),
                     "max_dist" = attr(object, "max_dist"),
                     "min_time" = attr(object, "min_time"),
                     "max_time" = attr(object, "max_time"))

  if (!is.null(new_times)) {
    # If interpolating for distances

    # Convert data types
    if (is.vector(new_times)) {
      # If a vector is provided, convert it to dataframe
      new_times_df <- data.frame(event_timestamp = new_times)
    } else if (is.data.frame(new_times)) {

      # If a DF is provided
      if (!("event_timestamp" %in% names(new_times))) {
        # Check if new_times has needed column
        stop("Please provide event_timestamp column in new_times")
      } else {
        new_times_df <- new_times
      }
    } else {
      stop("Please provide either a vector or dataframe of new_times.")
    }

    # Check derivative value
    if (deriv > attr(object, "max_deriv")) {
      # Check if derivative is above function's allowed limit
      stop(paste("Derivative too high. Maximum for this function is ",
                 attr(object, "max_deriv"), sep = ""))
    }

    # Function
    trajectory_function <- attr(object, "traj_fun")

    # Call internal function to do interpolation
    interpolate_distances_single(trip_extremes = trip_extremes,
                                 new_times = new_times_df,
                                 trajectory_function = trajectory_function,
                                 deriv = deriv)
  } else {
    # If interpolating for times

    # Convert data types
    if (is.vector(new_distances)) {
      # If a vector is provided, convert it to dataframe
      new_distances_df <- data.frame(distance = new_distances)
    } else if (is.data.frame(new_distances)) {

      # If a DF is provided
      if (!("distance" %in% names(new_distances))) {
        # Check if new_distances has needed column
        stop("Please provide distance column in new_distances")
      } else {
        new_distances_df <- new_distances
      }
    } else {
      stop("Please provide either a vector or dataframe of new_distances.")
    }

    if (deriv > 0) {
      # Check if derivative provided
      stop("Derivative not allowed for inverse function. Please find timepoints first, then derivatives at timepoints.")
    }

    # Get inverse trajectory function
    inv_trajectory_function <- attr(object, "inv_traj_fun")
    if (is.null(inv_trajectory_function)) {
      # Check that inverse function actually exists
      stop("Trajectory object does not contain inverse function. Please create one using get_trajectory_function().")
    }

    # Call internal function to do interpolation
    interpolate_times_single(trip_extremes = trip_extremes,
                             new_distances = new_distances_df,
                             inv_trajectory_function = inv_trajectory_function)
  }
}

#' Distance interpolation for group trajectories
#'
#' Not intended for external use.
#'
#' @param trip_extremes DF of max and min distance values
#' @param new_times DF of new time points
#' @param trajectory_function trajectory function list
#' @param deriv derivative to use
#' @return DF of interpolated values
interpolate_distances_group <- function(trip_extremes, new_times, trajectory_function, deriv) {

  use_trips <- trip_extremes$trip_id_performed
  new_times_trips <- new_times %>%
    dplyr::mutate(event_timestamp = as.numeric(event_timestamp)) %>%
    tidyr::uncount(weights = length(use_trips)) %>%
    dplyr::mutate(trip_id_performed = rep(use_trips, nrow(new_times))) %>%
    dplyr::left_join(y = trip_extremes, by = "trip_id_performed") %>%
    dplyr::filter(((event_timestamp >= min_time) & (event_timestamp <= max_time))) %>% # Remove extrapolated points
    dplyr::select(-c(min_time, max_time, min_dist, max_dist))

  if (dim(new_times_trips)[1] == 0) {
    stop("No trips provided within the range of new_times.")
  }

  if (deriv == 0) {
    # If deriv is 0, do not pass it
    # Deriv should always default to 0. If function does not take in deriv at all, we would get an error if trying to pass it
    int_df <- new_times_trips %>%
      dplyr::mutate(interp = purrr::map2_dbl(trip_id_performed, event_timestamp,
                                             function(trip_id_performed, event_timestamp) {
                                               trajectory_function[[trip_id_performed]](event_timestamp) }))
  } else {
    int_df <- new_times_trips %>%
      dplyr::mutate(interp = purrr::map2_dbl(trip_id_performed, event_timestamp,
                                             function(trip_id_performed, event_timestamp) {
                                               trajectory_function[[trip_id_performed]](event_timestamp,
                                                                                        deriv = deriv) }))
  }

  return(int_df)
}

#' Distance interpolation for single trajectories
#'
#' Not intended for external use.
#'
#' @param trip_extremes DF of max and min distance values
#' @param new_times DF of new time points
#' @param trajectory_function trajectory function
#' @param deriv derivative to use
#' @return DF of interpolated values
interpolate_distances_single <- function(trip_extremes, new_times, trajectory_function, deriv) {
  # Filter to allowed range
  filt_df <- new_times %>%
    dplyr::mutate(event_timestamp = as.numeric(event_timestamp)) %>%
    dplyr::filter((event_timestamp >= trip_extremes["min_time"]) &
                    (event_timestamp <= trip_extremes["max_time"]))

  if (dim(filt_df)[1] == 0) {
    # Check that points remain
    stop("Trip not within the range of new_times.")
  }

  if (deriv == 0) {
    # Interpolate
    int_df <- filt_df %>%
      dplyr::mutate(interp = trajectory_function(event_timestamp))
  } else {
    # Interpolate
    int_df <- filt_df %>%
      dplyr::mutate(interp = trajectory_function(event_timestamp,
                                                 deriv = deriv))
  }

  return(int_df)
}

#' Time interpolation for grouped trajectories
#'
#' Not intended for external use.
#'
#' @param trip_extremes DF of max and min time values
#' @param new_distances DF of new distance points
#' @param inv_trajectory_function Inverse trajectory function list
#' @return DF of interpolated values
interpolate_times_group <- function(trip_extremes, new_distances, inv_trajectory_function) {

  use_trips <- unique(trip_extremes$trip_id_performed)
  num_trips <- length(use_trips)
  new_dist_trips <- new_distances %>%
    tidyr::uncount(weights = num_trips) %>%
    dplyr::mutate(trip_id_performed = rep(use_trips, nrow(new_distances))) %>%
    dplyr::left_join(y = trip_extremes, by = "trip_id_performed") %>%
    dplyr::filter(((distance >= min_dist) & (distance <= max_dist))) %>% # Remove extrapolated points
    dplyr::select(-c(min_time, max_time, min_dist, max_dist))

  if (dim(new_dist_trips)[1] == 0) {
    stop("No trips provided within the range of new_distances.")
  }

  int_df <- new_dist_trips %>%
    dplyr::mutate(interp = purrr::map2_dbl(trip_id_performed, distance,
                                           function(trip_id_performed, distance) {
                                             inv_trajectory_function[[trip_id_performed]](distance)}))

  return(int_df)
}

#' Time interpolation for single trajectories
#'
#' Not intended for external use.
#'
#' @param trip_extremes DF of max and min time values
#' @param new_distances DF of new distance points
#' @param inv_trajectory_function Inverse trajectory function
#' @return DF of interpolated values
interpolate_times_single <- function(trip_extremes, new_distances, inv_trajectory_function) {
  # Filter to allowed range
  filt_df <- new_distances %>%
    dplyr::filter((distance >= trip_extremes["min_dist"]) &
                    (distance <= trip_extremes["max_dist"]))

  if (dim(filt_df)[1] == 0) {
    # Check that points remain
    stop("Trip not within the range of new_distances")
  }

  # Interpolate
  int_df <- filt_df %>%
    dplyr::mutate(interp = inv_trajectory_function(distance))
  return(int_df)
}

#' #' Quickly plots an AVL trajectory.
#'
#' This function generates a quick plot of a single or grouped trajectory
#' object. Using the trajectory function, the entire trajectory will be plotted
#' at a temporal resolution of 10 seconds. For grouped trajectories, a maximum
#' of 50 trips will be plotted. For more control over plotting and
#' formatting, see `plot_trajectory()`.
#'
#' @param x A trajectory object.
#' @param ... Other parameters (not used).
#' @return A ggplot2 object.
#' @export
plot.avltrajectory_group <- function(x, ...) {
  # Get trips to plot
  # Constrain to first 50 only. User can use more customizable function if they want more.
  if (length(x) > 50) {
    warning("Many trajectories detected. Plotting first 50 only. See plot_trajectory() for additional controls.")
    plot_trips <- unclass(x)[1:50]
  } else {
    plot_trips <- unclass(x)
  }

  # Get DF
  plot_seq <- seq(from = min(attr(x, "min_time")),
                  to = max(attr(x, "max_time")),
                  by = 10)
  plot_df <- predict.avltrajectory_group(object = x, new_times = plot_seq, trips = plot_trips) %>%
    dplyr::rename(distance = interp)

  # Generate color palette
  # Will be Viridis inferno. In ggplot, sample() is used to randomize each trajectory
  col_vector <- viridis::inferno(n = length(plot_trips))

  # Create plot
  traj_plot <- ggplot2::ggplot(data = plot_df) +
    ggplot2::geom_line(ggplot2::aes(x = event_timestamp, y = distance,
                                    color = factor(trip_id_performed)),
                       linewidth = 0.6, alpha = 0.8) +
    ggplot2::scale_color_manual(values = sample(col_vector, length(plot_trips)),
                                guide = "none") +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = "Epoch Time (sec)",
                  y = "Distance",
                  title = "Many AVL Trajectories",
                  subtitle = paste("Trips ", plot_trips[1], " through ", plot_trips[length(plot_trips)],
                                   "\n(", length(plot_trips), " total)",
                                   sep = ""))
  traj_plot
}

#' @rdname plot.avltrajectory_group
#' @export
plot.avltrajectory_single <- function(x, ...) {
  # Creat DF for plotting
  plot_seq <- seq(from = attr(x, "min_time"),
                  to = attr(x, "max_time"),
                  by = 10)
  plot_df <- predict.avltrajectory_single(object = x, new_times = plot_seq) %>%
    dplyr::rename(distance = interp)

  # Create & return plot
  traj_plot <- ggplot2::ggplot(data = plot_df) +
    ggplot2::geom_line(ggplot2::aes(x = event_timestamp, y = distance),
                       linewidth = 1, color = "coral") +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = "Epoch Time (s)",
                  y = "Distance (m)",
                  title = "Single AVL Trajectory",
                  subtitle = paste("Trip ", unclass(x), sep = ""))
  traj_plot
}
