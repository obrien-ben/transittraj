#' Get GTFS route IDs
#'
#' GTFS uses route IDs that may differ from the short or long route names we use day-to-day.
#' This function returns the GTFS route IDs from a given short or long route name.
#'
#' @param gtfs A complete GTFS object.
#' @param route_short_name GTFS route short name, as a numeric.
#' @param route_long_name GTFS route long name, as a character string.
#' @return Vector of numeric route IDs.
#' @export
get_route_ids <- function(gtfs, route_short = NULL, route_long = NULL) {
  if (!is.null(route_short)) {
    raw_ids <- gtfs$routes %>%
      dplyr::filter(route_short_name %in% route_short) %>%
      dplyr::mutate(route_id = route_id %>% as.numeric()) %>%
      dplyr::pull(route_id)
  } else if (!is.null(route_long)) {
    raw_ids <- gtfs$routes %>%
      dplyr::filter(route_long_name %in% route_long) %>%
      dplyr::mutate(route_id = route_id %>% as.numeric()) %>%
      dplyr::pull(route_id)
  } else {
    stop("No short or long route provided.")
  }
  return(raw_ids)
}

#' Filter GTFS for a single route.
#'
#' GTFS information is spread across many files, making it difficult to filter for the information you actually need.
#' This function returns a new GTFS object with only the information relevant to your desired routes.
#' Many functions in this package are designed to work with only one route in one direction. Use this function to create that object.
#'
#' @param gtfs A complete GTFS object.
#' @param route_ids A numeric vector, or single numeric, containing the desired route ID(s).
#' @param dir_id Optional. A numeric containing the disired direction number. Not filtered by defeault, but recommended.
#' @return A complete GTFS object containing only information relevant to the desired route.
#' @export
filter_by_route <- function(gtfs, route_ids, dir_id = NULL) {
  # Get new route and agencies
  new_routes <- gtfs$routes %>%
    dplyr::filter(route_id %in% route_ids) %>%
    dplyr::mutate(agency_id = agency_id %>% as.numeric())
  new_agency_id <- new_routes %>%
    dplyr::pull(agency_id)

  # Get new trips
  new_trips <- gtfs$trips %>%
    dplyr::filter(route_id %in% route_ids) %>%
    {
      if (!is.null(dir_id)) dplyr::filter(., direction_id == dir_id) else .
    } #%>%
  #dplyr::mutate(trip_id = trip_id %>% as.numeric())
  new_trip_ids <- new_trips %>%
    dplyr::pull(trip_id)
  new_shape_ids <- new_trips %>%
    #dplyr::mutate(shape_id = shape_id %>% as.numeric()) %>%
    dplyr::pull(shape_id)

  # Get new stop times
  new_stop_times <- gtfs$stop_times %>%
    dplyr::filter(trip_id %in% new_trip_ids) #%>%
  #dplyr::mutate(stop_id = stop_id %>% as.numeric())
  new_stop_ids <- new_stop_times %>%
    dplyr::pull(stop_id)

  # Get new stops
  new_stops <- gtfs$stops %>%
    dplyr::filter(stop_id %in% new_stop_ids)

  # Get new agency
  new_agency <- gtfs$agency %>%
    dplyr::filter(agency_id %in% new_agency_id)

  # Get new shapes
  new_shapes <- gtfs$shapes %>%
    dplyr::filter(shape_id %in% new_shape_ids)

  # Compile all GTFS files as list
  new_gtfs <- list(agency = new_agency, calendar = gtfs$calendar, routes = new_routes,
                   shapes = new_shapes, stop_times = new_stop_times, stops = new_stops,
                   trips = new_trips)
  return(new_gtfs)
}

#' Get unique identifiers for each between-stop segment
#'
#' This function uses a GTFS of a single route in a single direction to find two things:
#' first, the numeric stop ID corresponding to the starting stop of that route and direction;
#' and second, a vector of numeric IDs corresponding to the segments between all successive stops.
#' This segment ID is the concatenation of the "from" stop ID with the "to" stop ID.
#'
#' @param route_gtfs A GTFS for a single route in a single direction. Must contain at least stop times file.
#' @return List containing: an integer of the starting stop ID; a vector with numeric IDs for each segment between two stops.
#' @export
get_segment_ids <- function(route_gtfs) {
  # Get list of complete trip IDs
  longest_trip_ids <- route_gtfs$stop_times %>%
    dplyr::count(trip_id) %>%
    dplyr::filter(n == max(n)) %>%
    dplyr::arrange(trip_id)

  # Get a complete trip ID
  longest_trip_id <- as.numeric(longest_trip_ids$trip_id[1])

  # Get that complete trip
  longest_trip <- route_gtfs$stop_times %>%
    dplyr::filter(trip_id == longest_trip_id) %>%
    dplyr::arrange(stop_sequence) %>%
    dplyr::select(stop_id)

  # Start stop
  start_stop_id <- longest_trip$stop_id[1]

  # Get ids for each segment
  segment_ids = c()
  for (stop_index in 1:(length(longest_trip$stop_id) - 1)) {
    current_from <- longest_trip$stop_id[stop_index]
    current_to <- longest_trip$stop_id[stop_index + 1]
    current_id <- as.numeric(paste(current_from, current_to, sep = ""))
    segment_ids <- append(segment_ids, current_id)
  }

  return(list(start_stop_id = start_stop_id,
              segment_ids = segment_ids))
}

#' Get the length of each segment in meters, i.e. the distance between all adjacent stop pairs.
#'
#' This function returns
#' @param route_gtfs A GTFS for a single route in a single direction. Must contain at least stop times file.
#' @param start_stop A numeric for the starting stop ID.
#' @param segment_ids A vector of all stop IDs expected along this route and direction.
#' @return List containing two dataframes: segment_distances, distance between each segment; and start_distances, distance from each stop to the route start.
get_segment_distances <- function(route_gtfs, start_stop, segment_ids) {
  # Get distances between all stop pairs along the route
  all_distances <- tidytransit::stop_distances(route_gtfs$stops) %>%
    dplyr::mutate(segment_id = as.numeric(paste0(from_stop_id, to_stop_id, sep = "")))

  # Get distances along each segment
  segment_distances <- all_distances %>%
    dplyr::filter(segment_id %in% segment_ids)  %>%
    dplyr::select(distance, segment_id)

  # Get distances between the route start and each stop
  start_distances <- all_distances %>%
    dplyr::filter(from_stop_id == start_stop) %>%
    dplyr::select(to_stop_id, distance) %>%
    dplyr::mutate(to_stop_id = as.numeric(to_stop_id)) %>%
    dplyr::rename(stop_id = to_stop_id)

  return(list(segment_distances = segment_distances,
              start_distances = start_distances))
}

#' Get a series of spatial waypoints along a route alignment.
#'
#' This function generates a series of points along a route ("waypoints") from GTFS shapes. Each waypoint also includes the distance of that waypoint from the line's terminus.
#'
#' @param gtfs A GTFS with a complete shape file.
#' @param shape Optional. The GTFS shape_id to use. Can be a single value, or a vector. Default is NULL, where all shape_ids will be used.
#' @param n_int Optional. The number of points to split the route into. Default is NULL, where the GTFS "shape" waypoints will be used. Specify a higher value to increase the resolution (or smoothness) of vehicle animations.
#' @param project_crs Optional. The projection to use when performing spatial calculations. Consider setting to a Euclidian projection, such as the appropriate UTM zone. Default is 4326 (WGS 84).
#' @return A dataframe with a numeric column "X", a numeric column "Y", numeric "seq" sequence of waypoint along route, and numeric "distance" cumulative distance of the waypoint along its shape.
#' @export
get_waypoints <- function(gtfs, shape = NULL, n_int = NULL, project_crs = 4326) {

  # If shape not provided, use all unique shape IDs
  if (is.null(shape)) {
    shape = unique(gtfs$shapes$shape_id)
  }

  # Get raw waypoints from GTFS
  shape_waypoints <- gtfs$shapes %>%
    dplyr::filter(shape_id %in% shape) %>%
    dplyr::rename(lat = shape_pt_lat,
                  lon = shape_pt_lon,
                  seq = shape_pt_sequence) %>%
    dplyr::arrange(shape_id, seq)

  # Convert raw waypoints to SF
  shape_sf <- sf::st_as_sf(shape_waypoints,
                           coords = c("lon", "lat"),
                           crs = 4326) %>%
    sf::st_transform(crs = project_crs) %>%
    dplyr::group_by(shape_id) %>%
    dplyr::summarize(do_union = FALSE) %>%
    sf::st_cast("LINESTRING") %>%
    sf::st_cast("MULTILINESTRING")

  # Initialize global DF
  waypoints_df <- data.frame()

  # Loop through each shape to calculate distance along each
  for (current_shape in unique(shape_waypoints$shape_id)) {

    # Filter SF for just current shape
    current_shape_sf <- shape_sf %>%
      filter(shape_id == current_shape)

    # Get non-normalized distances along shape
    current_shape_len <- sf::st_length(current_shape_sf)

    # Turn route SF into SFC for interpolation
    current_shape_sfc <- sf::st_geometry(current_shape_sf)

    if(is.null(n_int)) {

      # Filter to current shape's GTFS waypoints
      current_waypoints <- shape_waypoints %>%
        dplyr::filter(shape_id == current_shape) %>%
        mutate(seq = row_number())

      # Get GTFS waypoints SFC
      current_waypoints_sfc <- sf::st_as_sf(current_waypoints,
                                            coords = c("lon", "lat"),
                                            crs = 4326) %>%
        sf::st_transform(crs = project_crs) %>%
        sf::st_geometry()

      # Project waypoints onto line to get distances
      dist_norm <- sf::st_line_project(line = current_shape_sfc, point = current_waypoints_sfc,
                                       normalized = TRUE)
      waypoints_dist = dist_norm * current_shape_len
      units(waypoints_dist) <- NULL

      # Add distance column
      current_waypoints <- sf::st_coordinates(current_waypoints_sfc) %>%
        as.data.frame() %>%
        dplyr::mutate(seq = row_number(),
                      shape_id = current_shape,
                      distance = waypoints_dist)

    } else {
      waypoint_seq <- seq(from = 0, to = 1, by = 1/n_int)

      # Get distances along shape
      current_shape_dist <- waypoint_seq * current_shape_len
      units(current_shape_dist) <- NULL

      # Turn route SF into SFC for interpolation
      current_shape_sfc <- sf::st_geometry(current_shape_sf)

      # Interpolate along line
      int_waypoints <- sf::st_line_interpolate(line = current_shape_sfc,
                                               dist = waypoint_seq,
                                               normalized = TRUE)

      # Turn coords into dataframe
      current_waypoints <- sf::st_coordinates(int_waypoints) %>%
        as.data.frame() %>%
        dplyr::mutate(seq = row_number(),
                      shape_id = current_shape,
                      distance = current_shape_dist)
    }

    # Bind to global DF
    waypoints_df <- rbind(waypoints_df, current_waypoints)

  }

  return(waypoints_df)

}

#' Get the geometry of a route shape.
#'
#' This function returns an SF multilinestring of the route alignments from GTFS shapes.
#' Similar to tidytransit's get_geometry(), but allows filtering by shape_id and projection to a new coordinate system.
#'
#' @param gtfs A GTFS with a complete shape file.
#' @param shape Optional. The GTFS shape_id to use. Can be a single value, or a vector. Default is NULL, where all shape_ids will be used.
#' @param project_crs Optional. The projection to use when performing spatial calculations. Consider setting to a Euclidian projection, such as the appropriate UTM zone. Default is 4326 (WGS 84 ellipsoid).
#' @return An SF multilinestring collection, with one multilinestring object per GTFS shape_id.
#' @export
get_shape_geometry <- function(gtfs, shape = NULL, project_crs = 4326) {

  # If shape not provided, use all unique shape IDs
  if (is.null(shape)) {
    shape = unique(gtfs$shapes$shape_id)
  }

  # Get raw waypoints from GTFS
  shape_waypoints <- gtfs$shapes %>%
    dplyr::filter(shape_id %in% shape) %>%
    dplyr::rename(lat = shape_pt_lat,
                  lon = shape_pt_lon,
                  seq = shape_pt_sequence) %>%
    dplyr::arrange(shape_id, seq)

  # Convert raw waypoints to SF
  shape_sf <- sf::st_as_sf(shape_waypoints,
                           coords = c("lon", "lat"),
                           crs = 4326) %>%
    sf::st_transform(crs = project_crs) %>%
    dplyr::group_by(shape_id) %>%
    dplyr::summarize(do_union = FALSE) %>%
    sf::st_cast("LINESTRING") %>%
    sf::st_cast("MULTILINESTRING")

  return(shape_sf)
}

#' Projects points to linear distances along a route shape.
#'
#' This function takes spatial points and projects them onto a route, returning the linear distance from the terminus of the route.
#'
#' @param gtfs A GTFS with a complete shape file.
#' @param points Can be either: a dataframe representing point coordinates, with fields "lon" and "lat"; or, an SF point object.
#' @param shape Optional. The GTFS shape_id to use. Must be a single value. Default is NULL, where the shape_id which appears first will be used.
#' @param original_crs Optional. If a dataframe is provided for "points", will be used to define the coordinate system of the lon/lat.
#' @param project_crs Optional. The projection to use when performing spatial calculations. Consider setting to a Euclidian projection, such as the appropriate UTM zone. Default is 4326 (WGS 84 ellipsoid).
#' @return The "points" input (either dataframe or SF) with an appended column for the linear distance along the route.
#' @export
project_onto_route <- function(gtfs, points, shape, original_crs = 4326, project_crs = 4326) {

  if (length(shape) != 1) {
    stop("Please provide exactly one shape ID.")
  }

  # Get points SFC
  if(!inherits(points, "sf")) {
    points_sf <- points %>%
      sf::st_as_sf(coords = c("lon", "lat"),
                   crs = original_crs) %>%
      sf::st_transform(crs = project_crs)
  } else {
    points_sf <- points
  }
  points_sfc <- sf::st_geometry(points_sf)

  # Get route line SFC
  line_sf <- get_shape_geometry(gtfs, shape = shape, project_crs = project_crs)
  line_sfc <- sf::st_geometry(line_sf)
  line_len <- sf::st_length(line_sfc)

  # Project and calculate distance
  dist_norm <- sf::st_line_project(line = line_sfc, point = points_sfc,
                                   normalized = TRUE)
  dist = dist_norm * line_len
  units(dist) <- NULL

  # Bind distance column
  points_dist <- points %>%
    dplyr::mutate(distance = dist)

  return(points_dist)
}

#' Get the distances between stops along the route.
#'
#' This function returns the linear distance of each stop along one route in one direction.
#'
#' @param route_gtfs A GTFS for a single route in a single direction. Must contain at least shape and stop files.
#' @param shape Optional. The GTFS shape_id to use. Must be a single value. Default is NULL, where the shape_id which appears first will be used.
#' @param project_crs Optional. The projection to use when performing spatial calculations. Consider setting to a Euclidian projection, such as the appropriate UTM zone. Default is 4326 (WGS 84 ellipsoid).
#' @return The GTFS stops file, with an appended column for the distance of each stop from the route's terminus.
#' @export
get_stop_distances <- function(route_gtfs, shape = NULL, project_crs = 4326) {

  # Set shape
  # If not provided, use first in GTFS
  if(is.null(shape)) {
    all_shapes <- unique(route_gtfs$shapes$shape_id)
    shape = all_shapes[1]
    if (length(all_shapes) > 1) {
      warning(paste("Multiple shapes found, and no shape_id provided. Using first shape_id: ",
                    shape, sep = ""))
    }
  } else if (length(shape) != 1) {
    stop("Please provide exactly one shape_id.")
  }

  # Get stops
  stops_df <- route_gtfs$stops %>%
    rename(lat = stop_lat,
           lon = stop_lon)

  # Calculate stop distances along route
  stops_dist <- project_onto_route(gtfs = route_gtfs,
                                   points = stops_df,
                                   shape = shape,
                                   original_crs = 4326,
                                   project_crs = project_crs)

  return(stops_dist)
}

#' Fits a continuous function of distance versus time.
#'
#' Uses scheduled stop times (via stop_times.txt) to fit an interpolating scheduled trajectory.
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

  # Get geometry
  if (is.null(shape_geometry)) {
    # If a specific shape has not been provided,
    # extract geometries from those present in trips.txt
    used_shapes <- unique(route_gtfs$trips$shape_id)
    shape_geometry <- get_shape_geometry(route_gtfs,
                                         shape = used_shapes,
                                         project_crs = project_crs)
  }

  # Get stop points
  stop_points <- route_gtfs$stops %>%
    dplyr::select(stop_id, stop_lat, stop_lon) %>%
    sf::st_as_sf(coords = c("stop_lon", "stop_lat"),
                 crs = 4326) %>%
    sf::st_transform(crs = project_crs) %>%
    sf::st_geometry()

  # Get distances at each stop point
  num_shapes <- length(shape_geometry$shape_id)
  shape_sfc <- sf::st_geometry(shape_geometry)
  shape_len <- sf::st_length(shape_sfc)
  if (num_shapes == 1) {
    # If only one shape
    dist_norm <- sf::st_line_project(line = shape_sfc, point = stop_points,
                                     normalized = TRUE)
    dist = dist_norm * shape_len
    units(dist) <- NULL

    stop_dist_df <- data.frame(stop_id = route_gtfs$stops$stop_id,
                               distance = dist,
                               shape_id = shape_geometry$shape_id[1])
  } else {
    # If multiple shapes, must project onto one shape at a time
    # Initialize list
    stop_dist_list <- vector("list", num_shapes)
    # Loop through shapes
    for (iter in 1:num_shapes) {
      current_shape <- shape_sfc[iter]
      dist_norm <- sf::st_line_project(line = current_shape, point = stop_points,
                                       normalized = TRUE)
      dist = dist_norm * shape_len[iter]
      units(dist) <- NULL

      stop_dist_list[[iter]] <- data.frame(stop_id = route_gtfs$stops$stop_id,
                                           distance = dist,
                                           shape_id = shape_geometry$shape_id[iter])
    }
    # Merge list into single dataframe
    stop_dist_df <- purrr::list_rbind(stop_dist_list)
  }

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
                  date = if_else(condition = (hour_num >= 24),
                                 true = (date + 1),
                                 false = date),
                  # If past midnight, adjust hour back down
                  hour_num = if_else(condition = (hour_num >= 24),
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
