#' Animate vehicle trajectory or AVL data.
#'
#' @description
#' These functions use the input trajectory object or TIDES AVL data to animate
#' vehicles progressing along their routes. This can be visualized in two ways:
#'
#' - `plot_animated_line()` simplifies the route alignment into a single
#' straight line and shows the vehicles moving down this line.
#'
#' - `plot_animated_map()` plots the full route's alignment and shows the
#' vehicles moving through space.
#'
#' Both functions allow the plotting of spatial features and labels for these
#' features. A `gganimate` object is returned, which can be further modified
#' and customized as desired.
#'
#' @details
#'
#' ## Input Trajectory Data
#'
#' There are two ways to provide data to these plotting functions:
#'
#' - A single or grouped trajectory object. This will use the direct
#' trajectory function at a resolution controlled by `timestep`. This is
#' simplest, and looks best when zooming in using `distance_lim`. The only
#' attribute that can be mapped to if using a trajectory is `trip_id_performed`.
#'
#' - A `distance_df` of TIDES AVL data. This will use the distance and time
#' point pairs for plotting, and draw linearly between them. This will look
#' similar to a plot using `trajectory` when zoomed out. It is most useful
#' if you want to map formatting to attributes other than `trip_id_performed`,
#' such as a vehicle or operator ID. If starting with a `trajectory`,
#' but the additional control over formatting is desired, consider using
#' `predict()` to generate distance and time points to plot, then joining
#' the desired attributes to the `trip_id_performed` column.
#'
#' Note that only one of `trajectory` and `distance_df` can be used. If both
#' (or neither) are provided, an error will be thrown.
#'
#' ## Features and Labels
#'
#' Often it is useful to plot the features of a route, such as its
#' stops/stations or the traffic signals it passes through. Use
#' `feature_distances` to provide information about spatial features to plot.
#' Each row in `feature_distances` should include at least a `distance` column.
#' Each of these rows will be plotted as a point on the route line.
#'
#' These features can also be labeled. Set `label_name` to a character string
#' corresponding to a field in `feature_distances` to generate labels with
#' this field as their text. The color of the label will automatically match
#' that of the feature they describe. The label placement is controlled by
#' `label_pos`, which has the following options available:
#'
#' - `plot_animated_line()`: Either `"left"` or `"right"` of the route line. The
#' y-value will be that of the feature it describes.
#'
#' - `plot_animated_map()`. A cardinal or intermediate direction (`"N"`,
#' `"SE"`, etc.) relative to the feature point. Or, `"in"`/`"out"`, relative to
#' the center of the plot.
#'
#' Note that for `plot_animated_map()` the `feature_distances` must still be
#' linear distances, not a spatial datatype. To retrieve distance values for
#' spatial features, see `get_stop_distances()` and `project_onto_route()`.
#'
#' ## Formatting Options
#'
#' Once a layer is created on a `ggplot2` object, it is difficult to change its
#' formatting. As such, this function attempts to provide as much flexibility
#' in formatting its layers as possible. The resulting plot includes three
#' layers:
#'
#' - Route line, controlled by `route_color`, `route_width`, and `route_alpha`.
#'
#' - Features, controlled by `feature_fill`, `feature_outline`, `feature_shape`,
#' `feature_size`, `feature_alpha`, and `feature_stroke`.
#'
#' - Labels, controlled by `label_size`, `label_alpha`, and `label_pos`.
#'
#' - Vehicles, controlled by `veh_fill`, `veh_outline`, `veh_shape`,
#' `veh_size`, `veh_alpha`, and `veh_stroke`.
#'
#' All of these formats can be controlled by inputting a single string or
#' numeric. The following attributes can also be modified using a dataframe,
#' mapping them to attributes of the layer:
#'
#' - `veh_shape` and `feature_shape`: A dataframe with one column named `shape`,
#' and another column sharing a name with a column in `distance_df` or
#' `feature_distances` (or, if using `trajectory`, a column named
#' `trip_id_performed`). The values in `shape` should be valid `ggplot2` point
#' shapes, and the values in the mapping column should match the values in
#' feature or trip column.
#'
#' - `veh_outline` and `feature_outline`: A dataframe with one column named
#' `outline`, and another column sharing a name with a column in `distance_df`
#' or `feature_distances` (or, if using `trajectory`, a column named
#' `trip_id_performed`). The values in `outline` should be valid color strings,
#' and the values in the mapping column should match the values in
#' feature or trip column.
#'
#' Note that if inputting `trajectory`, instead of `distance_df`, `veh_shape`
#' and `veh_outline` can only be mapped to `trip_id_performed`. If using
#' `distance_df`, they may be mapped to any column in `distance_df` (e.g.,
#' vehicle or operator IDs).
#'
#' ## Basemaps
#'
#' The function `plot_animated_map()` has one additional layer to format: the
#' basemap beneath the route alignment. OpenStreetMaps basemaps are used here.
#' See a full list of available basemaps using `rosm::osm.image()`.
#'
#' In addition to the map itself, the zoom level on the map can be
#' adjusted using `background_zoom`. This will describe a zoom level relative
#' to the "correct" level of your bounding box (i.e., what you what see if
#' you looked at that data in an online mapping platform). Setting to a
#' negative value (zooming out) will substantially speed up rendering, but will
#' give a much lower resolution may.
#'
#' Finally, the bounding box of the basemap can also be set. The bounding
#' box is defined relative to the spatial range of `trajectory` or
#' `distance_df`. The default is expansion is 0.05% (0.0025% for
#' `distance_lim != NULL`) of the larger dimension (northing or easting) of the
#' vehicle location bounding box. To customize, det `bbox_expand` to some
#' numeric in the distance units of the `shape_geometry`'s spatial
#' projection (e.g., meters if using a UTM projection).
#'
#' For examples and a more in-depth discussion, see (xyz).
#'
#' @param shape_geometry An SF object representing the route alignment. See
#' `get_shape_geometry()`.
#' @param background Optional. The OSM background (basemap) for the animation.
#' See `rosm::osm.image()`. Default is `"cartolight"`.
#' @param background_zoom Optional. The zoom, relative to the "correct" level,
#' for the background basemap. Default is 0.
#' @param bbox_expand Optional. The distance by which to expand the plotting
#' window in both directions. Default is `NULL`, which will expand the window by
#' 0.05% of the larger dimension (or 0.0025% if `distance_lim` is provided).
#' @param trajectory Optional. A trajectory object, either a single trajectory
#' or grouped trajectory. If provided, `distance_df` must not be provided.
#' Default is `NULL`.
#' @param distance_df Optional. A dataframe of time and distance points. Must
#' include at least `event_timestamp`, `distance`, and `trip_id_performed`.
#' If provided, `trajectory` must not be provided. Default is `NULL`.
#' @param plot_trips Optional. A vector of `trip_id_performed`s to plot. Default
#' is `NULL`, which will plot all trips provided in the `trajectory` or
#' `distance_df`.
#' @param timestep Optional. If `trajectory` is provided, the time interval, in
#' seconds, between interpolated observations to plot. Default is 5.
#' @param distance_lim Optional. A vector with `(minimum, maximum)` distance
#' values to plot.
#' @param center_vehicles Optional. A boolean, should all vehicle points be
#' centered to start at the same time (0 seconds)? Default is `FALSE`.
#' @param feature_distances Optional. A dataframe with at least numeric
#' `distance` for features. Default is `NULL`.
#' @param transition_style Optional. A `gganimate` `transition_style`,
#' specifying how the animation transitions from point to point. See
#' `gganimate::ease_aes()`. Default is `"linear"`.
#' @param route_color Optional. A color string for the color of the route
#' alignment. Default is `"coral"`.
#' @param route_width Optional. A numeric, the linewidth of the route
#' alignment. Default is 3.
#' @param route_alpha Optional. A numeric, the opacity of the route alignment.
#' Default is 1.
#' @param feature_shape Optional. A numeric specifying the `ggplot2` point
#' shape, or a dataframe mapping an attribute in `feature_distances` to a shape.
#' Must contain column `shape`. Default is 21 (circle).
#' @param feature_outline Optional. A color string, or a dataframe mapping an
#' attribute in `feature_distances` to a color. Must contain column `outline`.
#' Default is `"black"`.
#' @param feature_fill Optional. A color string, the inside fill of feature
#' points. Default is `"white"`.
#' @param feature_size Optional. A numeric, the size of the feature point.
#' Default is 2.
#' @param feature_stroke Optional. A numeric, the linewidth of the feature point
#' outline. Default is 1.25.
#' @param feature_alpha Optional. A numeric, the opacity of the feature point.
#' Default is 1.
#' @param veh_shape Optional. A numeric specifying the `ggplot2` point shape, or
#' a dataframe mapping an attribute in `distance_df` or `trajectory` to
#' a shape. Must contain column `shape`. Default is 23 (diamond).
#' @param veh_outline Optional. A color string, or a dataframe mapping an
#' attribute in `distance_df` or `trajectory` to a color. Must contain column
#' `outline`. Default is `"grey30"`.
#' @param veh_fill Optional. A color string, the inside fill of the vehicle
#' point. Default is `"white"`.
#' @param veh_size Optional. A numeric, the size of the vehicle point. Default
#' is 3.
#' @param veh_stroke Optional. A numeric, the linewidth of the vehicle point
#' outline. Default is 2.
#' @param veh_alpha Optional. A numeric, the opacity of the vehicle point.
#' Default is 0.8.
#' @param label_field Optional. A string specifying the column in
#' `feature_distances` with which to label the feature lines. Default is `NULL`,
#' where no labels will be plotted.
#' @param label_size Optional. The font size of the feature labels. Default is
#' 3.
#' @param label_alpha Optional. The opacity of the feature labels. Default is
#' 0.6.
#' @param label_pos Optional. A string specifying the label position on the
#' graph. Options include:
#' - `plot_animated_line()`: `"left"` or `"right"`. Default is `"left"`.
#' - `plot_animated_map()`: cardinal or intermediate direction (e.g.,
#' `"N"`, `"SW"`, etc.), or `"in"`/`"out"`. Default is `"out"`.
#' @returns A `gganimate` object.
#' @export
plot_animated_line <- function(trajectory = NULL, distance_df = NULL, plot_trips = NULL,
                               timestep = 5, distance_lim = NULL, center_vehicles = FALSE,
                               feature_distances = NULL, transition_style = "linear",
                               # Format route
                               route_color = "coral", route_width = 3, route_alpha = 1,
                               # Format features
                               feature_shape = 21, feature_outline = "black",
                               feature_fill = "white", feature_size = 2,
                               feature_stroke = 1.25, feature_alpha = 1,
                               # Format vehicles
                               veh_shape = 23, veh_outline = "grey30",
                               veh_fill = "white", veh_size = 3, veh_stroke = 2,
                               veh_alpha = 0.8,
                               # Format labels
                               label_field = NULL, label_size = 3,
                               label_alpha = 0.6, label_pos = "left") {

  # --- Validation & Setup ---
  # Validate input data & set up feature & vehicle location DFs to plot
  val_data <- plot_df_setup(trajectory = trajectory,
                                 distance_df = distance_df,
                                 timestep = timestep,
                                 plot_trips = plot_trips,
                                 feature_distances = feature_distances,
                                 distance_lim = distance_lim,
                                 center_vehicles = center_vehicles)
  trips_df <- val_data[[1]]
  feature_distances <- val_data[[2]]

  # Add x-value at which to plot vehicles
  trips_df <- trips_df %>% dplyr::mutate(x = 0)
  if (!is.null(feature_distances)) {
    feature_distances <- feature_distances %>% dplyr::mutate(x = 0)
  }

  # --- Route DF setup ---
  if (is.null(distance_lim)) {
    # If distance limits are not provided, draw route between lowest & highest observed distance
    route_df <- data.frame(distance = c(min(trips_df$distance), max(trips_df$distance)),
                           x = c(0, 0))
  } else {
    # If distance limits are provided, draw route between these limits
    route_df <- data.frame(distance = distance_lim,
                           x = c(0, 0))
  }

  # --- Formatting ---
  # Vehicle outline setup
  veh_outline_list <- plot_format_setup(plotting_df = trips_df,
                                             attribute_input = veh_outline,
                                             attribute_type = "outline",
                                             attribute_name = "veh_outline")
  trips_df <- veh_outline_list[[1]]
  show_legend_vehcolor <- veh_outline_list[[2]]
  veh_outline_by <- veh_outline_list[[3]]
  veh_outline_vals <- veh_outline_list[[4]]
  # Vehicle shape setup
  veh_shape_list <- plot_format_setup(plotting_df = trips_df,
                                             attribute_input = veh_shape,
                                             attribute_type = "shape",
                                             attribute_name = "veh_shape")
  trips_df <- veh_shape_list[[1]]
  show_legend_vehshape <- veh_shape_list[[2]]
  veh_shape_by <- veh_shape_list[[3]]
  veh_shape_vals <- veh_shape_list[[4]]
  if (!is.null(feature_distances)) {
    # Feature outline setup
    feature_outline_list <- plot_format_setup(plotting_df = feature_distances,
                                                   attribute_input = feature_outline,
                                                   attribute_type = "outline",
                                                   attribute_name = "feature_outline")
    feature_distances <- feature_outline_list[[1]]
    show_legend_featurecolor <- feature_outline_list[[2]]
    feature_outline_by <- feature_outline_list[[3]]
    feature_outline_vals <- feature_outline_list[[4]]
    # Feature shape setup
    feature_shape_list <- plot_format_setup(plotting_df = feature_distances,
                                                 attribute_input = feature_shape,
                                                 attribute_type = "shape",
                                                 attribute_name = "feature_shape")
    feature_distances <- feature_shape_list[[1]]
    show_legend_featureshape <- feature_shape_list[[2]]
    feature_shape_by <- feature_shape_list[[3]]
    feature_shape_vals <- feature_shape_list[[4]]

    # Label setup
    if (!is.null(label_field)) {
      # Check that requested field is in feature DF
      if (!(label_field %in% names(feature_distances))) {
        rlang::abort(message = "feature_distances: label_field not found in field names.",
                     class = "error_plottraj_labels")
      }
      # Label position setup
      if (label_pos == "left") {
        label_nudge = -0.05
        label_just = "right"
        x_lims <- c(-1, 0)
      } else if (label_pos == "right") {
        label_nudge = 0.05
        label_just = "left"
        x_lims <- c(0, 1)
      } else {
        rlang::abort(message = "Unknown label_pos. Please enter \"left\" or \"right\".",
                     class = "error_plottraj_labels")
      }
    } else {
      x_lims <- c(-1, 1)
    }
  }

  # Generate plot of routeline
  bus_plot <- ggplot2::ggplot() +
    # Add route line
    ggplot2::geom_line(data = route_df, ggplot2::aes(y = distance, x = x),
                       linewidth = route_width, color = route_color, alpha = route_alpha)

  # Add features
  if (!is.null(feature_distances)) {
    bus_plot <- bus_plot +
      ggplot2::geom_point(data = feature_distances,
                          ggplot2::aes(y = distance, x = x,
                                       color = factor(!!rlang::sym(feature_outline_by)),
                                       shape = factor(!!rlang::sym(feature_shape_by))),
                          size = feature_size, stroke = feature_stroke,
                          fill = feature_fill, alpha = feature_alpha) +
      ggplot2::scale_color_manual(name = feature_outline_by,
                                  values = feature_outline_vals,
                                  guide = show_legend_featurecolor) +
      ggplot2::scale_shape_manual(name = feature_shape_by,
                                  values = feature_shape_vals,
                                  guide = show_legend_featureshape)

    # Add labels
    if (!is.null(label_field)) {
      bus_plot <- bus_plot +
        ggplot2::geom_label(data = feature_distances,
                            ggplot2::aes(x = 0, y = distance,
                                         label = !!rlang::sym(label_field),
                                         color = factor(!!rlang::sym(feature_outline_by))),
                            hjust = label_just,
                            nudge_x = label_nudge,
                            alpha = label_alpha, size = label_size,
                            show.legend = FALSE)
    }
  }

  # Add vehicles
  bus_plot <- bus_plot +
    ggnewscale::new_scale("color") +
    ggnewscale::new_scale("shape") +
    ggplot2::geom_point(data = trips_df,
                        ggplot2::aes(y = distance, x = x, group = trip_id_performed,
                                     color = factor(!!rlang::sym(veh_outline_by)),
                                     shape = factor(!!rlang::sym(veh_shape_by))),
                        size = veh_size, stroke = veh_stroke,
                        fill = veh_fill, alpha = veh_alpha) +
    ggplot2::scale_color_manual(name = veh_outline_by,
                                values = veh_outline_vals,
                                guide = show_legend_vehcolor) +
    ggplot2::scale_shape_manual(name = veh_shape_by,
                                values = veh_shape_vals,
                                guide = show_legend_vehshape) +
    # Add time component
    gganimate::transition_components(event_timestamp) +
    gganimate::ease_aes(transition_style)

  # Theming
  bus_plot <- bus_plot +
    ggplot2::labs(title = "AVL Animation",
                  subtitle = "Time: {round(frame_time})",
                  y = "Distance") +
    ggplot2::scale_x_continuous(breaks = NULL, name = NULL,
                                limits = x_lims) +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(face = "bold", size = 12),
                   plot.subtitle = ggplot2::element_text(size = 10))

  # Subtitle time -- depends on datatype
  if (is.numeric(trips_df$event_timestamp)) {
    # If time is a numeric seconds value
    bus_plot <- bus_plot +
      ggplot2::labs(subtitle = "Time: {round(frame_time)} sec")
  } else {
    # Otherwise, time has to be a date
    bus_plot <- bus_plot +
      ggplot2::labs(subtitle = "{round_date(frame_time, unit = \"second\")}")
  }

  return(bus_plot)
}

#' @rdname plot_animated_line
#' @export
plot_animated_map <- function(shape_geometry, trajectory = NULL, distance_df = NULL,
                              plot_trips = NULL, timestep = 5, distance_lim = NULL,
                              center_vehicles = FALSE, feature_distances = NULL,
                              background = "cartolight", background_zoom = 0,
                              bbox_expand = NULL, transition_style = "linear",
                              # Format route
                              route_color = "coral", route_width = 3, route_alpha = 1,
                              # Format features
                              feature_shape = 21, feature_outline = "black",
                              feature_fill = "white", feature_size = 2,
                              feature_stroke = 1.25, feature_alpha = 1,
                              # Format vehicles
                              veh_shape = 23, veh_outline = "grey30",
                              veh_fill = "white", veh_size = 3, veh_stroke = 2,
                              veh_alpha = 0.8,
                              # Format labels
                              label_field = NULL, label_size = 3,
                              label_alpha = 0.6, label_pos = "out") {

  # --- Validation & Setup ---
  # Validate input data & set up feature & vehicle location DFs to plot
  val_data <- plot_df_setup(trajectory = trajectory,
                                 distance_df = distance_df,
                                 timestep = timestep,
                                 plot_trips = plot_trips,
                                 feature_distances = feature_distances,
                                 distance_lim = distance_lim,
                                 center_vehicles = center_vehicles)
  trips_df <- val_data[[1]]
  feature_distances <- val_data[[2]]
  # Validate shape geometry
  validate_shape_geometry(shape_geometry = shape_geometry,
                          max_length = 1,
                          require_shape_id = FALSE)

  # --- Formatting ---
  # Vehicle outline setup
  veh_outline_list <- plot_format_setup(plotting_df = trips_df,
                                        attribute_input = veh_outline,
                                        attribute_type = "outline",
                                        attribute_name = "veh_outline")
  trips_df <- veh_outline_list[[1]]
  show_legend_vehcolor <- veh_outline_list[[2]]
  veh_outline_by <- veh_outline_list[[3]]
  veh_outline_vals <- veh_outline_list[[4]]
  # Vehicle shape setup
  veh_shape_list <- plot_format_setup(plotting_df = trips_df,
                                      attribute_input = veh_shape,
                                      attribute_type = "shape",
                                      attribute_name = "veh_shape")
  trips_df <- veh_shape_list[[1]]
  show_legend_vehshape <- veh_shape_list[[2]]
  veh_shape_by <- veh_shape_list[[3]]
  veh_shape_vals <- veh_shape_list[[4]]
  if (!is.null(feature_distances)) {
    # Feature outline setup
    feature_outline_list <- plot_format_setup(plotting_df = feature_distances,
                                              attribute_input = feature_outline,
                                              attribute_type = "outline",
                                              attribute_name = "feature_outline")
    feature_distances <- feature_outline_list[[1]]
    show_legend_featurecolor <- feature_outline_list[[2]]
    feature_outline_by <- feature_outline_list[[3]]
    feature_outline_vals <- feature_outline_list[[4]]
    # Feature shape setup
    feature_shape_list <- plot_format_setup(plotting_df = feature_distances,
                                            attribute_input = feature_shape,
                                            attribute_type = "shape",
                                            attribute_name = "feature_shape")
    feature_distances <- feature_shape_list[[1]]
    show_legend_featureshape <- feature_shape_list[[2]]
    feature_shape_by <- feature_shape_list[[3]]
    feature_shape_vals <- feature_shape_list[[4]]
  }

  # --- Spatial ---
  # Get geometry SFC
  shape_geom_sfc <- sf::st_geometry(shape_geometry)
  # Get coordinates for trips_df, but leave as dataframe
  trips_sf <- trips_df %>%
    dplyr::mutate(point_geom = sf::st_line_interpolate(line = shape_geom_sfc,
                                                       dist = distance,
                                                       normalized = FALSE),
                  x_spatial = sf::st_coordinates(point_geom)[,1],
                  y_spatial = sf::st_coordinates(point_geom)[,2])

  # Convert features into SF points
  if (!is.null(feature_distances)) {
    features_sf <- feature_distances %>%
      dplyr::mutate(point_geom = sf::st_line_interpolate(line = shape_geom_sfc,
                                                         dist = distance,
                                                         normalized = FALSE),
                    x_spatial = sf::st_coordinates(point_geom)[,1],
                    y_spatial = sf::st_coordinates(point_geom)[,2]) %>%
      dplyr::select(-point_geom)
  }

  bbox <- sf::st_bbox(trips_sf$point_geom)
  trips_sf <- trips_sf %>% dplyr::select(-point_geom)
  # If bounding box expansion not provided, calculate as portion of bbox
  if (is.null(bbox_expand)) {
    if (is.null(distance_lim)) {
      bbox_expand <- 0.0005 * max(c(bbox[1], bbox[2]))
    } else {
      # Use closer zoom if a distance limit provided
      bbox_expand <- 0.000025 * max(c(bbox[1], bbox[2]))
    }
  }

  # Label setup
  if (!is.null(label_field)) {
    # Check that requested field is in feature DF
    if (!(label_field %in% names(feature_distances))) {
      rlang::abort(message = "feature_distances: label_field not found in field names.",
                   class = "error_plottraj_formatting")
    }

    # Label position setup
    if (label_pos == "N") {
      label_nudge_y = 0.125 * bbox_expand
      label_nudge_x = 0
      label_hjust = "middle"
      label_vjust = "bottom"
    } else if (label_pos == "S") {
      label_nudge_y = -0.125 * bbox_expand
      label_nudge_x = 0
      label_hjust = "middle"
      label_vjust = "top"
    } else if (label_pos == "E") {
      label_nudge_y = 0 * bbox_expand
      label_nudge_x = 0.125 * bbox_expand
      label_hjust = "left"
      label_vjust = "center"
    } else if (label_pos == "W") {
      label_nudge_y = 0
      label_nudge_x = -0.125 * bbox_expand
      label_hjust = "right"
      label_vjust = "center"
    } else if (label_pos == "NE") {
      label_nudge_y = 0.125 * bbox_expand
      label_nudge_x = 0.125 * bbox_expand
      label_hjust = "left"
      label_vjust = "bottom"
    } else if (label_pos == "NW") {
      label_nudge_y = 0.125 * bbox_expand
      label_nudge_x = -0.125 * bbox_expand
      label_hjust = "right"
      label_vjust = "bottom"
    } else if (label_pos == "SE") {
      label_nudge_y = -0.125 * bbox_expand
      label_nudge_x = 0.125 * bbox_expand
      label_hjust = "left"
      label_vjust = "top"
    } else if (label_pos == "SW") {
      label_nudge_y = -0.125 * bbox_expand
      label_nudge_x = -0.125 * bbox_expand
      label_hjust = "right"
      label_vjust = "top"
    } else if (label_pos == "in") {
      label_nudge_y = 0
      label_nudge_x = 0
      label_hjust = "inward"
      label_vjust = "inward"
    } else if (label_pos == "out") {
      label_nudge_y = 0
      label_nudge_x = 0
      label_hjust = "outward"
      label_vjust = "outward"
    } else {
      rlang::abort(message = "Unknown label position. Please enter \"N\", \"S\", \"E\", \"W\", \"NE\", \"NW\", \"SE\", \"SW\", \"in\", or \"out\".",
                   class = "error_plottraj_labels")
    }
  }

  # --- Plotting ---
  # Build basemap & route
  anim_map <- ggplot2::ggplot() +
    # Set basemap
    ggspatial::annotation_map_tile(type = background, zoomin = background_zoom,
                                   progress = "none") +
    # Plot route
    ggspatial::geom_sf(data = shape_geom_sfc, color = route_color, size = route_width) +
    # Set bounding box
    ggspatial::coord_sf(xlim = c((bbox[1] - bbox_expand), (bbox[3] + bbox_expand)),
                        ylim = c((bbox[2] - bbox_expand), (bbox[4] + bbox_expand)),
                        crs = sf::st_crs(shape_geom_sfc))

  # Add features
  if (!is.null(feature_distances)) {
    anim_map <- anim_map +
      ggplot2::geom_point(data = features_sf,
                          ggspatial::aes(x = x_spatial, y = y_spatial,
                                         color = factor(!!rlang::sym(feature_outline_by)),
                                         shape = factor(!!rlang::sym(feature_shape_by))),
                          fill = feature_fill, alpha = feature_alpha,
                          stroke = feature_stroke, size = feature_size) +
      ggplot2::scale_color_manual(name = feature_outline_by,
                                  values = feature_outline_vals,
                                  guide = show_legend_featurecolor) +
      ggplot2::scale_shape_manual(name = feature_shape_by,
                                  values = feature_shape_vals,
                                  guide = show_legend_featureshape)

    # Add labels
    if (!is.null(label_field)) {
      anim_map <- anim_map +
        ggplot2::geom_label(data = features_sf,
                            ggplot2::aes(x = x_spatial, y = y_spatial,
                                label = !!rlang::sym(label_field),
                                color = factor(!!rlang::sym(feature_outline_by))),
                            hjust = label_hjust, vjust = label_vjust,
                            nudge_x = label_nudge_x, nudge_y = label_nudge_y,
                            alpha = label_alpha, size = label_size,
                            show.legend = FALSE, fill = "white")
    }
  }

  # Add vehicles
  anim_map <- anim_map +
    ggnewscale::new_scale("color") +
    ggnewscale::new_scale("shape") +
    ggplot2::geom_point(data = trips_sf,
                        ggplot2::aes(x = x_spatial, y = y_spatial,
                                     group = trip_id_performed,
                                     color = factor(!!rlang::sym(veh_outline_by)),
                                     shape = factor(!!rlang::sym(veh_shape_by))),
                        fill = veh_fill, size = veh_size, stroke = veh_stroke) +
    ggplot2::scale_color_manual(name = veh_outline_by,
                                values = veh_outline_vals,
                                guide = show_legend_vehcolor) +
    ggplot2::scale_shape_manual(name = veh_shape_by,
                                values = veh_shape_vals,
                                guide = show_legend_vehshape) +
    gganimate::transition_components(event_timestamp) +
    gganimate::ease_aes(transition_style)

  # Theming
  anim_map <- anim_map +
    ggplot2::theme_void() +
    ggplot2::labs(title = "AVL Animation",
                  xlab = NULL, ylab = NULL) +
    ggplot2::scale_x_discrete(breaks = NULL, name = NULL) +
    ggplot2::scale_y_discrete(breaks = NULL, name = NULL)

  # Subtitle time -- depends on datatype
  if (is.numeric(trips_sf$event_timestamp)) {
    # If time is a numeric seconds value
    anim_map <- anim_map +
      ggplot2::labs(subtitle = "Time: {round(frame_time)} sec")
  } else {
    # Otherwise, time has to be a date
    anim_map <- anim_map +
      ggplot2::labs(subtitle = "{round_date(frame_time, unit = \"second\")}")
  }

  return(anim_map)
}

#' Set up dataframe & validate of point objects for vehicle animations
#'
#' Intended for internal use only
#'
#' @param trajectory Single or grouped trajectory object.
#' @param distance_df AVL distance DF.
#' @param plot_trips Vector of trip_id_performed to plot.
#' @param timestep Time in seconds for interpolation.
#' @param distance_lim Vector of (minimum, maximum) distance to plot.
#' @param feature_distances Linear distance to features.
#' @param center_vehicles Should vehicles be centered
#' @return plotting dataframe (trips_df)
plot_df_setup <- function(trajectory, distance_df,
                               plot_trips,
                               timestep,
                               distance_lim,
                               feature_distances,
                               center_vehicles) {

  # --- Vehicle DF setup ---
  # Check provided trajectories & distance DF, and filter as needed
  if (!is.null(trajectory) & !is.null(distance_df)) {
    rlang::abort(message = "Please provide only one of trajectory and distance_df.",
                 class = "error_plottraj_inputdata")
  } else if (!is.null(trajectory)) {
    # If trajectory is provided, generate the DF by predicting from functions

    # Get times to interpolate over
    from_time <- min(attr(trajectory, "min_time"))
    to_time <- max(attr(trajectory, "max_time"))
    time_seq <- seq(from = from_time, to = to_time,
                    by = timestep)

    # Depending on object type, get distances at times
    if ("avltrajectory_single" %in% class(trajectory)) {
      # If single trajectory, should not filter by trips
      trips_df <- predict.avltrajectory_single(trajectory, new_times = time_seq) %>%
        dplyr::rename(distance = interp) %>%
        dplyr::mutate(trip_id_performed = unclass(trajectory))
    } else if ("avltrajectory_group" %in% class(trajectory)) {
      # If grouped trajectory, handle trips
      trips_df <- predict.avltrajectory_group(trajectory, trips = plot_trips,
                          new_times = time_seq) %>%
        dplyr::rename(distance = interp)
    } else {
      rlang::abort(message = "Unrecognized trajectory object. Please use get_trajectory_function() to generate a trajectory object.",
                   class = "error_plottraj_inputdata")
    }
  } else {
    # If distance_df provided, validate it
    needed_fields <- c("trip_id_performed", "event_timestamp", "distance")
    validate_input_to_tides(needed_fields = needed_fields,
                            avl_df = distance_df)

    # Filter to desired trips
    if (is.null(plot_trips)) {
      plot_trips <- unique(distance_df$trip_id_performed)
    }
    trips_df <- distance_df %>%
      dplyr::filter(trip_id_performed %in% plot_trips)
  }

  # Filter observations to distance limits
  if (!is.null(distance_lim)) {
    trips_df <- trips_df %>%
      dplyr::filter((distance >= distance_lim[1]) & (distance <= distance_lim[2]))

    # If features, filter these to be within distance range
    if (!is.null(feature_distances)) {
      feature_distances <- feature_distances %>%
        dplyr::filter((distance >= distance_lim[1]) & (distance <= distance_lim[2]))
    }
  }

  # Filter observations to distance limits
  if (!is.null(distance_lim)) {
    trips_df <- trips_df %>%
      dplyr::filter((distance >= distance_lim[1]) & (distance <= distance_lim[2]))

    # Check that observations remain after filtering.
    if (dim(trips_df)[1] == 0) {
      rlang::abort(message = "No trip observations within distance limit.",
                   class = "error_plottraj_distlim")
    }

    # If features, filter these to be within distance range
    if (!is.null(feature_distances)) {
      feature_distances <- feature_distances %>%
        dplyr::filter((distance >= distance_lim[1]) & (distance <= distance_lim[2]))

      # Check that feature values remain after filtering.
      if (dim(feature_distances)[1] == 0) {
        rlang::abort(message = "No features within distance limit.",
                     class = "error_plottraj_distlim")
      }
    }
  }

  # Center trajectories to all begin at same point
  if (center_vehicles) {
    trips_df <- trips_df %>%
      dplyr::mutate(event_timestamp = as.numeric(event_timestamp)) %>%
      dplyr::group_by(trip_id_performed) %>%
      dplyr::mutate(event_timestamp = event_timestamp - min(event_timestamp)) %>%
      dplyr::ungroup()
  }
  return(list(trips_df, feature_distances))
}

#' Function to set up plot formats.
#'
#' Intended for internal use only.
#'
#' @importFrom rlang :=
#' @param plotting_df DF for plotting, either trips or features
#' @param attribute_input The user input value for the attribute (e.g.,
#' outline_input = veh_outline)
#' @param attribute_type The type of attribute being constructed (e.g.,
#' "outline")
#' @param attribute_name The name of the attribute (e.g., "veh_outline")
#' @return List with: 1) new plotting_df, 2) show_legend, 3) attribute_by,
#' and 4) attribute_vals
plot_format_setup <- function(plotting_df,
                              attribute_input,
                              attribute_type,
                              attribute_name) {

  if (!is.data.frame(attribute_input)) {
    temp_attr_name <- paste("temp_", attribute_name, sep = "")
    show_legend <- "none"
    plotting_df <- plotting_df %>%
      dplyr::mutate(!!rlang::sym(temp_attr_name) := "1")
    attribute_by <- temp_attr_name
    attribute_vals <- c(attribute_input)
    names(attribute_vals) <- "1" # Temp = 1 is a dummy grouping factor to code all plotting_df the same color
  } else if (attribute_type %in% names(attribute_input)) {
    show_legend <- "legend"
    attr_df_names <- names(attribute_input)
    plotting_names <- names(plotting_df)

    # Match outline to a vehicle location data type
    attribute_by <- plotting_names[!is.na(match(plotting_names,
                                                attr_df_names))]
    # Check attribute_by -- should be exaclty one matching column
    if (length(attribute_by) > 1) {
      rlang::abort(message = paste(attribute_name, ": multiple columns match input data. Only one column can match.",
                                   sep = ""),
                   class = "error_plottraj_format")
    } else if (length(attribute_by) == 0) {
      rlang::abort(message = paste(attribute_name, ": no columns match input data. One column must match.",
                                   sep = ""),
                   class = "error_plottraj_format")
    }
    attribute_vals <- attribute_input[[attribute_type]]
    names(attribute_vals) <- as.character(attribute_input[[attribute_by]])
  } else {
    rlang::abort(message = paste(attribute_name, ": ", attribute_type, " column not provided.",
                                 sep = ""),
                 class = "error_plottraj_format")
  }

  return(list(plotting_df,
              show_legend,
              attribute_by,
              attribute_vals
              ))
}

#' Plot vehicle trajectories or AVL data.
#'
#' @description
#' This function use the input trajectory object or TIDES AVL data to draw a
#' trajectory plot (i.e., linear distance versus time) for each trip. This
#' function allows for the plotting of spatial features and labels for these
#' features. A `ggplot2` object is returned, which can be further modified
#' and customized as desired.
#'
#' @details
#'
#' ## Input Trajectory Data
#'
#' There are two ways to provide data to these plotting functions:
#'
#' - A single or grouped trajectory object. This will use the direct
#' trajectory function at a resolution controlled by `timestep`. This is
#' simplest, and looks best when zooming in using `distance_lim`. The only
#' attribute that can be mapped to if using a trajectory is `trip_id_performed`.
#'
#' - A `distance_df` of TIDES AVL data. This will use the distance and time
#' point pairs for plotting, and draw linearly between them. This will look
#' similar to a plot using `trajectory` when zoomed out. It is most useful
#' if you want to map formatting to attributes other than `trip_id_performed`,
#' such as a vehicle or operator ID. If starting with a `trajectory`,
#' but the additional control over formatting is desired, consider using
#' `predict()` to generate distance and time points to plot, then joining
#' the desired attributes to the `trip_id_performed` column.
#'
#' Note that only one of `trajectory` and `distance_df` can be used. If both
#' (or neither) are provided, an error will be thrown.
#'
#' ## Features and Labels
#'
#' Often it is useful to plot the features of a route, such as its
#' stops/stations or the traffic signals it passes through. Use
#' `feature_distances` to provide information about spatial features to plot.
#' Each row in `feature_distances` should include at least a `distance` column.
#' Each of these rows will be plotted as a horizontal line across the graph.
#'
#' These features can also be labeled. Set `label_name` to a character string
#' corresponding to a field in `feature_distances` to generate labels with
#' this field as their text. The color of the label will automatically match
#' that of the feature they describe. The label placement is controlled by
#' `label_pos`, which can be set to `"left"` or `"right"`.
#'
#' ## Formatting Options
#'
#' Once a layer is created on a `ggplot2` object, it is difficult to change its
#' formatting. As such, this function attempts to provide as much flexibility
#' in formatting its layers as possible. The resulting plot includes three
#' layers:
#'
#' - Vehicle trajectories, controlled by `traj_color`, `traj_type`, and
#'  `traj_width`, and `traj_alpha`.
#'
#' - Features, controlled by `feature_color`, `feature_type`, `feature_width`,
#' and `feature_alpha`.
#'
#' - Labels, controlled by `label_size`, `label_alpha`, and `label_pos`.
#'
#' All of these formats can be controlled by inputting a single string or
#' numeric. The following attributes can also be modified using a dataframe,
#' mapping them to attributes of the layer:
#'
#' - `traj_type` and `feature_type`: A dataframe with one column named
#' `linetype`, and another column sharing a name with a column in
#' `distance_df` or `feature_distances` (or, if using `trajectory`, a column
#' named `trip_id_performed`). The values in `linetype` should be valid
#' `ggplot2` linetypes, and the values in the mapping column should match the
#' values in feature or trip column.
#'
#' - `traj_color` and `feature_color`: A dataframe with one column named
#' `color`, and another column sharing a name with a column in `distance_df` or
#' `feature_distances` (or, if using `trajectory`, a column named
#' `trip_id_performed`). The values in `color` should be valid color strings,
#' and the values in the mapping column should match the values in
#' feature or trip column.
#'
#' Note that if inputting `trajectory`, instead of `distance_df`, `veh_shape`
#' and `traj_color` and `traj_type` can only be mapped to `trip_id_performed`.
#' If using `distance_df`, they may be mapped to any column in `distance_df`
#' (e.g., vehicle or operator IDs).
#'
#' @inheritParams plot_animated_line
#' @param center_trajectories Optional. A boolean, should all trajectories be
#' centered to start at the same time (0 seconds)? Default is `FALSE`.
#' @param traj_color Optional. A color string, or a dataframe mapping an
#' attribute in `distance_df` or `trip_id_performed` in `trajectory` to a color.
#' Must contain column `color`. Default is `"coral"`.
#' @param traj_type Optional. A string specifying the `ggplot2` linetype, or a
#' dataframe mapping an attribute in `distance_df` or `trip_id_performed` in
#' `trajectory` to a linetype. Must contain column `linetype`. Default is
#' `"solid"`.
#' @param traj_width Optional. A numeric, the width of the trajectory line.
#' Default is 1.
#' @param traj_alpha Optional. A numeric, the opacity of the trajectory line.
#' Default is 1.
#' @param feature_color Optional. A color string, or a dataframe mapping an
#' attribute in `feature_distances` to a color. Must contain column `color`.
#' Default is `grey30`.
#' @param feature_type Optional. A string specifying the `ggplot2` linetype, or
#' a dataframe mapping an attribute in `feature_distances` to a linetype. Must
#' contain column `linetype`. Default is `"dashed"`.
#' @param feature_width Optional. A numeric, the width of the feature line.
#' Default is 0.8.
#' @param label_pos Optional. A string specifying the label position on the
#' graph. Must be either `"left"` or `"right"`. Default is `"left"`.
#' @return A `ggplot2` object.
#' @export
plot_trajectory <- function(trajectory = NULL, distance_df = NULL, plot_trips = NULL,
                            timestep = 5, distance_lim = NULL, center_trajectories = FALSE,
                            feature_distances = NULL,
                            # Trajectory line customization
                            traj_color = "coral", traj_type = "solid",
                            traj_width = 1, traj_alpha = 1,
                            # Feature line customization
                            feature_color = "grey30", feature_type = "dashed",
                            feature_width = 0.8, feature_alpha = 0.8,
                            # Feature label customization
                            label_field = NULL, label_size = 3,
                            label_alpha = 0.6, label_pos = "left") {

  # --- Plotting DF setup ---
  val_data <- plot_df_setup(trajectory = trajectory,
                            distance_df = distance_df,
                            timestep = timestep,
                            plot_trips = plot_trips,
                            feature_distances = feature_distances,
                            distance_lim = distance_lim,
                            center_vehicles = center_trajectories)
  trips_df <- val_data[[1]]
  feature_distances <- val_data[[2]]

  # --- Formatting setup ---
  # Trajectory color
  traj_color_list <- plot_format_setup(plotting_df = trips_df,
                                       attribute_input = traj_color,
                                       attribute_type = "color",
                                       attribute_name = "traj_color")
  trips_df <- traj_color_list[[1]]
  show_legend_trajcolor <- traj_color_list[[2]]
  color_by <- traj_color_list[[3]]
  color_vals <- traj_color_list[[4]]
  # Trajectory linetype
  traj_type_list <- plot_format_setup(plotting_df = trips_df,
                                       attribute_input = traj_type,
                                       attribute_type = "linetype",
                                       attribute_name = "traj_type")
  trips_df <- traj_type_list[[1]]
  show_legend_trajtype <- traj_type_list[[2]]
  traj_type_by <- traj_type_list[[3]]
  traj_type_vals <- traj_type_list[[4]]
  # Features
  if (!is.null(feature_distances)) {
    # Feature outline setup
    feature_color_list <- plot_format_setup(plotting_df = feature_distances,
                                              attribute_input = feature_color,
                                              attribute_type = "color",
                                              attribute_name = "feature_color")
    feature_distances <- feature_color_list[[1]]
    show_legend_featurecolor <- feature_color_list[[2]]
    feature_color_by <- feature_color_list[[3]]
    feature_color_vals <- feature_color_list[[4]]
    # Feature shape setup
    feature_type_list <- plot_format_setup(plotting_df = feature_distances,
                                            attribute_input = feature_type,
                                            attribute_type = "linetype",
                                            attribute_name = "feature_type")
    feature_distances <- feature_type_list[[1]]
    show_legend_featuretype <- feature_type_list[[2]]
    feature_type_by <- feature_type_list[[3]]
    feature_type_vals <- feature_type_list[[4]]

    # Label setup
    if (!is.null(label_field)) {
      # Check that requested field is in feature DF
      if (!(label_field %in% names(feature_distances))) {
        rlang::abort(message = "feature_distances: label_field not found in field names.",
                     class = "error_plottraj_labels")
      }
      # Label position setup
      if (label_pos == "left") {
        label_t = min(trips_df$event_timestamp)
      } else if (label_pos == "right") {
        label_t = max(trips_df$event_timestamp)
      } else {
        rlang::abort(message = "Unknown label_pos. Please enter \"left\" or \"right\".",
                     class = "error_plottraj_labels")
      }
    }
  }

  # --- Plotting ---
  # Create trajectory line and stop points
  traj_plot <- ggplot2::ggplot() +
    # Add lines
    ggplot2::geom_line(data = trips_df,
                       ggplot2::aes(y = distance, x = event_timestamp,
                                    group = trip_id_performed,
                                    color = factor(!!rlang::sym(color_by)),
                                    linetype = factor(!!rlang::sym(traj_type_by))),
                       linewidth = traj_width, alpha = traj_alpha) +
    ggplot2::scale_color_manual(name = color_by,
                                values = color_vals,
                                guide = show_legend_trajcolor) +
    ggplot2::scale_linetype_manual(name = traj_type_by,
                                   values = traj_type_vals,
                                   guide = show_legend_trajtype)

  # Add features
  if (!is.null(feature_distances)) {
    traj_plot <- traj_plot +
      ggnewscale::new_scale("color") +
      ggnewscale::new_scale("linetype") +
      ggplot2::geom_hline(data = feature_distances,
                          ggplot2::aes(yintercept = distance,
                              color = factor(!!rlang::sym(feature_color_by)),
                              linetype = factor(!!rlang::sym(feature_type_by))),
                          linewidth = feature_width, alpha = feature_alpha) +
      ggplot2::scale_color_manual(name = feature_color_by,
                                  values = feature_color_vals,
                                  guide = show_legend_featurecolor) +
      ggplot2::scale_linetype_manual(name = feature_type_by,
                                     values = feature_type_vals,
                                     guide = show_legend_featuretype)

    # Add labels
    if (!is.null(label_field)) {
      traj_plot <- traj_plot +
        ggplot2::geom_label(data = feature_distances,
                            ggplot2::aes(x = label_t, y = distance,
                                label = !!rlang::sym(label_field),
                                color = factor(!!rlang::sym(feature_color_by))),
                            hjust = label_pos, alpha = label_alpha,
                            size = label_size, show.legend = FALSE)
    }
  }

  # Theming
  traj_plot <- traj_plot +
    ggplot2::labs(title = "AVL Trajectories",
                  x = "Time",
                  y = "Distance") +
    ggplot2::theme_minimal()

  return(traj_plot)
}

#' Generates a Leaflet viewer of GTFS routes and stops.
#'
#' @description
#' This function generates a simple Leaflet-based interactive map viewer of a
#' GTFS. This function is intended for quick and easy visualization of a GTFS
#' feed. As such, formatting options are relatively limited.
#'
#' @details
#'
#' ## Route Shapes and Stops
#'
#' The primary goal of this function is to visualize and explore each GTFS
#' shape, including its associated `route_id` and `direction_id`. This function
#' will plot all shapes and stops present in the input `gtfs`. To plot only
#' a specific route or direction, first the feed using `filter_by_route()`.
#'
#' Routes have both pop-ups and hover labels. The hover label shows the
#' shapes's `route_id` (from the `trips` file). The pop-up will show the
#' `route_id`, `direction_id`, and `shape_id`.
#'
#' Stops also have both pop-ups and hover labels. The hover label will show the
#' point's `stop_id` (from the `stops` file). The pop-up will show the
#' `stop_name` and `stop_id`.
#'
#' ## Formatting
#'
#' Two formatting options are available through this function: basemaps
#' and route color palettes.
#'
#' The `background` parameter allows you to customize the background map below
#' the plotted shapes and stops. Esri's light grey canvas is the default, as it
#' is excellent for providing geographic context while still allowing the
#' routes to stand out. To see the available options, type
#' `leaflet::providers$` into your console.
#'
#' The route colors can be customized in two different ways:
#'
#' - Using the `gtfs`'s colors. Typically, the `routes` file in a GTFS feed
#' will contain a field `route_color`; this is the color you see in most
#' public-facing mapping/navigation applications (e.g., Google Maps, Transit,
#' etc.). If this is present in the input `gtfs` feed, setting
#' `color_palette = "gtfs"` will use this field to color each shape.
#'
#' - Using a named color palette. Without `gtfs` colors, this function
#' assigns colors categorically (using `leaflet::colorFactor()`). To set
#' the palette, input a string corresponding to a palette name from
#' `RColorBrewer`, a palette name from `viridis`, a vector of color names (with
#' the same length as the number of shapes), or some other color function. See
#' Leaflet's
#' [colors vignette](https://rstudio.github.io/leaflet/articles/colors.html)
#' for more information.
#'
#' @param gtfs A tidytransit GTFS object.
#' @param background Optional. A string for the background of the transit map,
#' from Leaflet's provider library (see `leaflet::providers$`). Default is
#' Esri's light gray canvas (`"Esri.WorldGrayCanvas"`).
#' @param color_palette Optional. A string for the Leaflet color palette to
#' color routes. If `"gtfs"`, will use color codes in the GTFS `routes` file.
#' Default is `"Dark2"`.
#' @return A Leaftlet object.
#' @export
plot_interactive_gtfs <- function(gtfs,
                                  background = "Esri.WorldGrayCanvas",
                                  color_palette = "Dark2") {

  # --- Validate input GTFS ---
  validate_gtfs_input(gtfs,
                      table = "stops",
                      needed_fields = c("stop_id", "stop_lon", "stop_lat",
                                        "stop_name"))
  validate_gtfs_input(gtfs,
                      table = "trips",
                      needed_fields = c("shape_id", "direction_id", "route_id"))
  if (color_palette == "gtfs") {
    validate_gtfs_input(gtfs,
                        table = "routes",
                        needed_fields = c("route_color"))
  }

  # --- Get GTFS geometries ---
  # Routes
  shape_geometry <- get_shape_geometry(gtfs = gtfs)

  # Stops
  stops_sf <- gtfs$stops %>%
    dplyr::select(stop_name, stop_id, stop_lon, stop_lat) %>%
    sf::st_as_sf(coords = c("stop_lon", "stop_lat"),
                 crs = 4326)


  # --- Formatting ---
  # Should equal dimension of shape_geometry, assuming each shape_id has
  # one direction & route
  shapes_info <- gtfs$trips %>%
    dplyr::distinct(shape_id, direction_id, route_id)
  shape_geometry <- shape_geometry %>%
    dplyr::left_join(y = shapes_info, by = "shape_id")

  # Popups & labels
  route_popup <- paste("Route: ", shape_geometry$route_id,
                       "<br>Direction: ", shape_geometry$direction_id,
                       "<br>Shape: ", shape_geometry$shape_id,
                       sep = "")
  route_hover <- as.character(shape_geometry$route_id)
  stop_popup <- paste(stops_sf$stop_name, " (", stops_sf$stop_id, ")", sep = "")
  stop_hover <- as.character(stops_sf$stop_id)

  # Colors
  if (color_palette == "gtfs") {
    # Get color codes and append #
    route_colors <- gtfs$routes %>%
      dplyr::select(route_id, route_color) %>%
      dplyr::mutate(route_color = paste("#", route_color, sep = ""))

    # Create palette
    route_pal <- leaflet::colorFactor(route_colors$route_color, route_colors$route_id)
  } else {
    # Create palette
    route_pal <- leaflet::colorFactor(color_palette, shape_geometry$route_id)
  }

  # --- Create map ---
  interactive_map <- leaflet::leaflet() %>%
    leaflet::addPolylines(data = shape_geometry, # Route alignment
                          color = ~route_pal(route_id),
                          opacity = 1,
                          label = route_hover,
                          popup = route_popup) %>%
    leaflet::addLegend(data = shape_geometry, # Legend
                       position ="bottomright",
                       pal = route_pal,
                       values = ~route_id,
                       title = "Route ID") %>%
    leaflet::addCircleMarkers(data = gtfs_stops, # Stops
                              fillColor = "white",
                              fillOpacity = 1,
                              color = "black",
                              opacity = 1,
                              weight = 1,
                              radius = 3,
                              label = stop_hover,
                              popup = stop_popup) %>%
    leaflet::addProviderTiles(background) # Basemap

  return(interactive_map)
}

#' Save your animation at a desired quality.
#'
#' This function is a helepr for `gganimate`'s `anim_save()`, providing a
#' simplified, though less feature-rich, version of these functions. Animations
#' are saved as `.gif`s at the desired path. With this function,
#' publication-quality (high-resolution and smooth) animations are possible,
#' but take a long time to render.
#'
#' @param anim_object A `gganimate` object.
#' @param path A string representing the desired path and name at which to save
#' animation.
#' @param duration Optional. A numeric, in seconds, representing the length of
#' the animation. Default is 30.
#' @param fps Optional. The frames per second of the saved animation. Default
#' is 10.
#' @param width Optional. The width of the exported image, in inches. Default
#' is 7.5
#' @param height Optional. The height of the exported image, in inches. Default
#' is 5.5.
#' @param dpi Optional. The resolution, in dots per inch, of the image. Default
#' is 100.
#' @export
export_animation <- function(anim_object, path,
                             duration = 30, fps = 10,
                             width = 7.5, height = 5.5, dpi = 100) {

  if (!grepl(".gif", path)) {
    stop("Please provide path including .gif extension.")
  }

  n_frames = duration * fps

  # Save animation
  gganimate::anim_save(filename = path, animation = anim_object,
                       duration = duration,
                       nframes = n_frames,
                       width = width,
                       height = height,
                       units = "in",
                       res = dpi)
  message(" -- Save Successful -- ")
}
