#' Filter GTFS to a desired route(s) and direction(s).
#'
#' @description
#' This function returns a new `tidygtfs` object with only the information
#' relevant to your desired routes and directions. All fields included in the
#' input `gtfs` will be filtered. See `Details` for more information about
#' required files and fields
#'
#' @details
#' The following files and fields are required for this function:
#'
#' - `routes`: with `route_id` and `agency_id`
#'
#' - `agency`: with `agency_id`
#'
#' - `trips`: with `route_id`, `direction_id`, `shape_id`, `service_id`, and
#' `trip_id`
#'
#' - `stop_times`: with `stop_id` and `trip_id`
#'
#' The following files are optional. If they are included, the must include
#' the listed fields:
#'
#' - `stops`: with `stop_id`
#'
#' - `shapes`: with `shape_id`
#'
#' - `calendar`: with `service_id`
#'
#' - `calendar_dates`: with `service_id`
#'
#' - `transfers`: with `trip_id` and `stop_id`
#'
#' - `frequencies`: with `trip_id`
#'
#' - `fare_rules`: with `route_id`
#'
#' - `feed_info`
#'
#' For these optional files, the function will detect whether they are present.
#' If so, they will be filtered; if not, they will be left `NULL` in the new
#' GTFS. If any required file or field is missing, an error will be thrown
#' describing what is missing.
#'
#' @param gtfs A tidygtfs object.
#' @param route_ids A numeric vector or single numeric containing the desired
#' route ID(s).
#' @param dir_id Optional. A numeric vector or single numeric containing the
#' desired direction ID(s).
#' @return A tidygtfs object containing only information relevant to the desired
#'  route and direction.
#' @export
filter_by_route <- function(gtfs, route_ids, dir_id = NULL) {

  # --- Check GTFS is tidygtfs object ---
  if (!("tidygtfs" %in% class(gtfs))) {
    rlang::abort(message = "Provided GTFS not a tidygtfs object.",
                 class = "error_gtfsval_not_tidygtfs")
  }
  gtfs_val <- attr(gtfs, "validation_result")

  # --- Validate fields ---
  # routes: route_id, agency_id
  validate_gtfs_input(gtfs = gtfs,
                      table = "routes",
                      needed_fields = c("route_id", "agency_id"))

  # agency: agency_id
  validate_gtfs_input(gtfs = gtfs,
                      table = "agency",
                      needed_fields = c("agency_id"))

  # trips: route_id, direction_id, shape_id, trip_id, service_id
  validate_gtfs_input(gtfs = gtfs,
                      table = "trips",
                      needed_fields = c("route_id", "direction_id", "shape_id",
                                 "trip_id", "service_id"))

  # stop_times: route_id, agency_id
  validate_gtfs_input(gtfs = gtfs,
                      table = "stop_times",
                      needed_fields = c("stop_id", "trip_id"))

  # stops: stop_id (optional)
  stops_present <- all(gtfs_val %>%
                         dplyr::filter(file == "stops") %>%
                         dplyr::pull(file_provided_status))
  if (stops_present) {
    validate_gtfs_input(gtfs = gtfs,
                        table = "stops",
                        needed_fields = c("stop_id"))
  }

  # shapes: shape_id (optional)
  shapes_present <- all(gtfs_val %>%
                         dplyr::filter(file == "shapes") %>%
                         dplyr::pull(file_provided_status))
  if (shapes_present) {
    validate_gtfs_input(gtfs = gtfs,
                        table = "shapes",
                        needed_fields = c("shape_id"))
  }

  # calendar: service_id (optional)
  calendar_present <- all(gtfs_val %>%
                         dplyr::filter(file == "calendar") %>%
                         dplyr::pull(file_provided_status))
  if (calendar_present) {
    validate_gtfs_input(gtfs = gtfs,
                        table = "calendar",
                        needed_fields = c("service_id"))
  }

  # calendar_dates: service_id (optional)
  calendar_dates_present <- all(gtfs_val %>%
                         dplyr::filter(file == "calendar_dates") %>%
                         dplyr::pull(file_provided_status))
  if (calendar_dates_present) {
    validate_gtfs_input(gtfs = gtfs,
                        table = "calendar_dates",
                        needed_fields = c("service_id"))
  }

  # transfers: trip_id, stop_id (optional)
  transfers_present <- all(gtfs_val %>%
                         dplyr::filter(file == "transfers") %>%
                         dplyr::pull(file_provided_status))
  if (transfers_present) {
    validate_gtfs_input(gtfs = gtfs,
                        table = "transfers",
                        needed_fields = c("trip_id", "stop_id"))
  }

  # frequencies: service_id (optional)
  frequencies_present <- all(gtfs_val %>%
                         dplyr::filter(file == "frequencies") %>%
                         dplyr::pull(file_provided_status))
  if (frequencies_present) {
    validate_gtfs_input(gtfs = gtfs,
                        table = "frequencies",
                        needed_fields = c("trip_id"))
  }

  # fare_rules: route_id (optional)
  fare_rules_present <- all(gtfs_val %>%
                         dplyr::filter(file == "fare_rules") %>%
                         dplyr::pull(file_provided_status))
  if (fare_rules_present) {
    validate_gtfs_input(gtfs = gtfs,
                        table = "fare_rules",
                        needed_fields = c("route_id"))
  }

  # --- Filtering ---
  # Routes
  new_routes <- gtfs$routes %>%
    dplyr::filter(route_id %in% route_ids)
  # Check if new routes empty -- no matching route IDs in original GTFS
  if (dim(new_routes)[1] == 0) {
    rlang::abort(message = "No matching route_ids in GTFS.",
                 class = "error_gtfsfilt_none")
  }
  new_agency_id <- new_routes %>%
    dplyr::pull(agency_id)

  # Trips
  new_trips <- gtfs$trips %>%
    dplyr::filter(route_id %in% route_ids)
  if (!is.null(dir_id)) {
    # If given, filter for direction IDs
    new_trips <- new_trips %>%
      dplyr::filter(direction_id %in% dir_id)
    # Check if new trips empty -- no matching direction IDs in original GTFS
    if (dim(new_trips)[1] == 0) {
      rlang::abort(message = "No matching direction_ids in GTFS route.",
                   class = "error_gtfsfilt_none")
    }
  }
  new_trip_ids <- new_trips %>%
    dplyr::pull(trip_id)
  new_shape_ids <- new_trips %>%
    dplyr::pull(shape_id)
  new_service_ids <- new_trips %>%
    dplyr::pull(service_id)

  # stop_times
  new_stop_times <- gtfs$stop_times %>%
    dplyr::filter(trip_id %in% new_trip_ids)
  new_stop_ids <- new_stop_times %>%
    dplyr::pull(stop_id)

  # stops
  if (stops_present) {
    new_stops <- gtfs$stops %>%
      dplyr::filter(stop_id %in% new_stop_ids)
  } else {
    new_stops <- NULL
  }

  # shapes
  if (shapes_present) {
    new_shapes <- gtfs$shapes %>%
      dplyr::filter(shape_id %in% new_shape_ids)
  } else {
    new_shapes <- NULL
  }

  # agency
  new_agency <- gtfs$agency %>%
    dplyr::filter(agency_id %in% new_agency_id)

  # calendar
  if (calendar_present) {
    new_calendar <- gtfs$calendar %>%
      dplyr::filter(service_id %in% new_service_ids)
  } else {
    new_calendar <- NULL
  }

  # calendar_dates
  if (calendar_dates_present) {
    new_calendar_dates <- gtfs$calendar_dates %>%
      dplyr::filter(service_id %in% new_service_ids)
  } else {
    new_calendar_dates <- NULL
  }

  # frequencies
  if (frequencies_present) {
    new_frequencies <- gtfs$frequencies %>%
      dplyr::filter(trip_id %in% new_trip_ids)
  } else {
    new_frequencies <- NULL
  }

  # transfers
  if (transfers_present) {
    new_transfers <- gtfs$transfers %>%
      dplyr::filter((trip_id %in% new_trip_ids) &
                      (stop_id %in% new_stop_ids))
  } else {
    new_transfers <- NULL
  }

  # fare_rules
  if (fare_rules_present) {
    new_fare_rules <- gtfs$fare_rules %>%
      dplyr::filter(route_id %in% route_ids)
  } else {
    new_fare_rules <- NULL
  }

  # --- Compile into final new GTFS ---
  # Validator will give warnings over NULL files, if not present in
  # input GTFS. This is OK.
  new_gtfs <- suppressWarnings(tidytransit::as_tidygtfs(list(
    agency = new_agency,
    calendar = new_calendar,
    routes = new_routes,
    shapes = new_shapes,
    stop_times = new_stop_times,
    stops = new_stops,
    trips = new_trips,
    calendar_dates = new_calendar_dates,
    frequencies = new_frequencies,
    transfers = new_transfers,
    fare_rules = new_fare_rules,
    feed_info = gtfs$feed_info,
    fare_attributes = gtfs$fare_attributes)))

  return(new_gtfs)
}

#' Get the geometry of a route shape.
#'
#' @description
#' This function returns an SF multilinestring of the route alignments from
#' GTFS shapes. Similar to tidytransit's `get_geometry()`, but allows filtering
#' by `shape_id` and projection to a new coordinate system. See `Details` for
#' requirements on the input GTFS.
#'
#' @details
#' A `shapes` file must be present in your GTFS object. This file must contain
#' at least the following fields:
#'
#' - `shape_id`
#'
#' - `shape_pt_lat`
#'
#' - `shape_pt_lon`
#'
#' - `shape_pt_sequence`
#'
#' @inheritParams filter_by_route
#' @param shape Optional. The GTFS shape_id to use. Can be a single value, or
#' a vector. Default is NULL, where all `shape_id`s in `gtfs` will be used.
#' @param project_crs Optional. A numeric EPSG identifer indicating the
#' coordinate system to use for spatial calculations. Consider setting to a
#' Euclidian projection, such as the appropriate UTM zone. Default is 4326 (WGS
#' 84 ellipsoid).
#' @return An SF multilinestring, with one multilinestring object per
#' `shape_id`.
#' @export
get_shape_geometry <- function(gtfs, shape = NULL, project_crs = 4326) {

  # --- Validate ---
  # Check if GTFS is tidygtfs object
  if (!("tidygtfs" %in% class(gtfs))) {
    rlang::abort(message = "Provided GTFS not a tidygtfs object.",
                 class = "error_gtfsval_not_tidygtfs")
  }
  validate_gtfs_input(gtfs,
                      table = "shapes",
                      needed_fields = c("shape_id", "shape_pt_sequence",
                                        "shape_pt_lat", "shape_pt_lon"))

  # --- Get geometry ---
  # If specific shape not provided, use all unique shape IDs
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
#' @description
#' This function takes spatial points and projects them onto a route, returning
#' the linear distance from the beginning terminal of the route.
#'
#' @inheritParams get_shape_geometry
#' @param shape_geometry The SF object to project onto. Must include the field
#' `shape_id`. See `get_shape_geometry()`.
#' @param points Can be either: a dataframe representing point coordinates,
#' with fields `longitude` and `latitude`; or, an SF or SFC point object.
#' @param original_crs Optional. A numeric EPSG identifier. If a dataframe is
#' provided for `points`, this will be used to define the coordinate system of
#' the longitude / latitude values. Default is 4326 (WGS 84 ellipsoid).
#' @return The `points` input (either dataframe or SF) with an appended column
#' for the linear distance along the route. If `points` is an SFC, a vector of
#' numeric distances is returned. Units are those of the spatial projection
#' used (e.g., meters if using UTM).
#' @export
project_onto_route <- function(shape_geometry, points,
                               original_crs = 4326, project_crs = 4326) {


  # --- Validate shape geometry ---
  validate_shape_geometry(shape_geometry,
                          require_shape_id = TRUE,
                          max_length = 1,
                          match_crs = project_crs)

  # --- Points ---
  # If provided is dataframe with longitude and latitude, convert to SF first
  if(is.data.frame(points) & !("sf" %in% class(points))) {
    # If DF, check that has the required fields
    points_fields = c(("longitude" %in% names(points)),
                      ("latitude" %in% names(points)))
    if (!all(points_fields)) {
      rlang::abort(message = paste(c("points missing the following required fields:",
                                     c("longitude", "latitude")[!points_fields]),
                                   collapse = " "),
                   class = "error_pointsval_fields")
    }
    # Convert to SFC
    points_sfc <- points %>%
      sf::st_as_sf(coords = c("longitude", "latitude"),
                   crs = original_crs) %>%
      sf::st_transform(crs = project_crs) %>%
      sf::st_geometry()
  } else if ("sf" %in% class(points)) {
    points_sfc <- sf::st_geometry(points)
  } else if ("sfc" %in% class(points)) {
    points_sfc <- points
  } else {
    rlang::abort(message = "Unrecognized points datatype. Please input dataframe, SF, or SFC.",
                 class = "error_pointsval_datatype")
  }

  # Check that SFC is points
  if (!all(sf::st_is(points_sfc, "POINT"))) {
    rlang::abort(message = "Unrecognized points datatype. Please ensure features are points.",
                 class = "error_pointsval_geomtype")
  }

  # Get route line SFC
  line_sfc <- sf::st_geometry(shape_geometry)
  line_len <- sf::st_length(line_sfc)

  # Project and calculate distance
  dist_norm <- sf::st_line_project(line = line_sfc, point = points_sfc,
                                   normalized = TRUE)
  dist = dist_norm * line_len
  units(dist) <- NULL

  # If input points are SFC, no other attributes to return; just give dist
  if ("sfc" %in% class(points)) {
    return(dist)
  } else if ("sf" %in% class(points)) {
    # If input points are SF, drop geometry and add distance
    points_dist <- points %>%
      sf::st_drop_geometry() %>%
      dplyr::mutate(distance = dist)
  } else {
    points_dist <- points %>%
      dplyr::mutate(distance = dist)
  }
  return(points_dist)
}

#' Get the distances of stops along routes.
#'
#' @description
#' This function returns the linear distance of each stop along a route shape,
#' starting from the route's beginning terminal. Unless a `shape_geometry` is
#' provided, stops will be project onto all `shape_id`s that serve them. If a
#' `shape_geometry` is provided, the function will look only for stops served
#' by that shape.
#'
#' @inheritParams filter_by_route
#' @inheritParams get_shape_geometry
#' @param shape_geometry Optional. The SF object to project onto. Must include
#' the field `shape_id`. See `get_shape_geometry()`. Default is NULL, where
#' all shapes in `gtfs` will be used.
#' @return A dataframe containing `stop_id`, the `shape_id` it was projected
#' onto, and `distance`, in units of the spatial projection (e.g., meters if
#' using UTM).
#' @export
get_stop_distances <- function(gtfs, shape_geometry = NULL,
                               project_crs = 4326) {

  # --- Validate gtfs ---
  if (!("tidygtfs" %in% class(gtfs))) {
    rlang::abort(message = "Provided GTFS not a tidygtfs object.",
                 class = "error_gtfsval_not_tidygtfs")
  }
  validate_gtfs_input(gtfs,
                      table = "stops",
                      needed_fields = c("stop_id", "stop_lon", "stop_lat"))
  validate_gtfs_input(gtfs,
                      table = "trips",
                      needed_fields = c("shape_id", "trip_id"))

  # --- Validate shape_geometry ---
  if (is.null(shape_geometry)) {
    # If not provided, take using shape_geometry -- should be OK
    shape_geometry <- get_shape_geometry(gtfs,
                                         project_crs = project_crs)
  } else {
    # If provided by user, validate
    validate_shape_geometry(shape_geometry,
                            require_shape_id = TRUE,
                            max_length = Inf,
                            match_crs = project_crs)
  }

  # --- Spatial geometry ---
  # Get correct shape_ids & trip_ids
  use_shape_ids <- unique(shape_geometry$shape_id)
  trip_shape_pairs <- gtfs$trips %>%
    dplyr::filter(shape_id %in% use_shape_ids) %>%
    dplyr::distinct(trip_id, shape_id)
  stop_shape_pairs <- gtfs$stop_times %>%
    dplyr::left_join(y = trip_shape_pairs, by = "trip_id") %>%
    dplyr::distinct(stop_id, shape_id)

  # Join stops and shapes
  stops_with_shapes <- gtfs$stops %>%
    # Associate each stop with its shape ID
    dplyr::left_join(y = stop_shape_pairs, by = "stop_id",
                     relationship = "one-to-many") %>%
    dplyr::select(stop_id, stop_lat, stop_lon, shape_id) %>%
    dplyr::filter(!is.na(shape_id))

  if (dim(stops_with_shapes)[1] == 0) {
    stop("No stops served by provided shapes.")
  }

  # Spatial
  stop_points <- stops_with_shapes %>%
    sf::st_as_sf(coords = c("stop_lon", "stop_lat"),
                 crs = 4326) %>%
    sf::st_transform(crs = project_crs)

  # Get distances at each stop point
  num_shapes <- length(shape_geometry$shape_id)
  if (num_shapes == 1) {
    # If only one shape
    current_shape_id <- shape_geometry$shape_id[1]
    current_stops <- stop_points %>%
      dplyr::filter(shape_id == current_shape_id)

    stop_dist_df <- project_onto_route(shape_geometry = shape_geometry,
                                       points = stop_points,
                                       project_crs = project_crs) %>%
      sf::st_drop_geometry() %>%
      dplyr::mutate(shape_id = current_shape_id)
  } else {
    # If multiple shapes, must project onto one shape at a time
    # Initialize list
    stop_dist_list <- vector("list", num_shapes)
    # Loop through shapes
    for (iter in 1:num_shapes) {
      current_shape_id <- shape_geometry$shape_id[iter]
      current_shape <- shape_geometry %>%
        dplyr::filter(shape_id == current_shape_id)
      current_stops <- stop_points %>%
        dplyr::filter(shape_id == current_shape_id)
      if (dim(current_stops)[1] == 0) {
        next
      }

      stop_dist_list[[iter]] <- project_onto_route(shape_geometry = current_shape,
                                                   points = current_stops,
                                                   project_crs = project_crs) %>%
        sf::st_drop_geometry() %>%
        dplyr::mutate(shape_id = current_shape_id)
    }
    # Merge list into single dataframe
    stop_dist_df <- purrr::list_rbind(stop_dist_list)
  }
  return(stop_dist_df)
}

#' Function to quickly validate whether input GTFS has required tables and
#' fields within those tables.
#'
#' Not intended for external use.
validate_gtfs_input <- function(gtfs, table, needed_fields) {

  # Pull validation table
  gtfs_val <- attr(gtfs, "validation_result")

  # Check table presence
  table_present <- all(gtfs_val %>%
                         dplyr::filter(file == table) %>%
                         dplyr::pull(file_provided_status))
  if (!table_present) {
    rlang::abort(message = paste("Table ", table, " missing from input GTFS",
                                 sep = ""),
                 class = "error_gtfsval_missing_table")
  }

  # Check field presence
  fields_present <- gtfs_val %>%
    dplyr::filter(file == table) %>%
    dplyr::filter(field %in% needed_fields) %>%
    dplyr::select(field, field_provided_status) %>%
    dplyr::arrange(match(field, needed_fields))

  if (!all(fields_present$field_provided_status)) {
    missing_fields <- fields_present %>%
      dplyr::filter(!field_provided_status) %>%
      dplyr::pull(field)
    rlang::abort(message = paste(c("The following fields are missing from",
                                   table, ":", missing_fields),
                                 collapse = " "),
                 class = "error_gtfsval_missing_fields")
  }
}

#' Function to quickly validate whether an input shape_geometry meets needs
#'
#' Checks:
#' - Class (should be SF, not SFC)
#' - Geometry type (i.e., is multilinestring)
#' - Presence of shape_id column, if desired
#' - Number of shapes present, if desired
#' - Correct CRS, if desired
#'
#' Not intended for external use.
validate_shape_geometry <- function(shape_geometry,
                                    max_length = Inf,
                                    require_shape_id = TRUE,
                                    match_crs = NULL) {

  # Check class
  if (!("sf" %in% shape_geometry)) {
    if ("sfc" %in% shape_geometry) {
      rlang::abort(message = "SFC provided for shape_geometry. Please input SF object.",
                   class = "error_geomval_datatype")
    } else {
      rlang::abort(message = "Unknown geometry datatype. Please input SF object.",
                   class = "error_geomval_datatype")
    }
  }

  # Check multilinestring
  if (!all(sf::st_is(shape_geometry, "MULTILINESTRING"))) {
    rlang::abort(message = "shape_geometry is not a MULTILINESTRING. Please input SF multilinestring object.",
                 class = "error_geomval_geomtype")
  }

  # Check if shape_id is present
  if (require_shape_id & !("shape_id" %in% names(shape_geometry))) {
    rlang::abort(message = "shape_id field not found in provided shape_geometry.",
                 class = "error_geomval_id")
  }

  # Check length of SF (i.e., num of shapes)
  if (dim(shape_geometry)[1] > max_length) {
    rlang::abort(message = paste("Too many shape_ids in shape_geometry. Please input only ",
                                 max_length, " shapes.",
                                 sep = ""),
                 class = "error_geomval_length")
  }

  # Check CRS, if one is provided
  if (!is.null(match_crs)) {
    if (sf::st_crs(shape_geometry)$epsg != match_crs) {
      rlang::abort(message = paste("shape_geometry not in correct projection. Please transform to EPSG ",
                                   match_crs, ".",
                                   sep = ""),
                   class = "error_geomval_crs")
    }
  }
}
