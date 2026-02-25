# Animate vehicle trajectory or AVL data.

These functions use the input trajectory object or TIDES AVL data to
animate vehicles progressing along their routes. This can be visualized
in two ways:

- `plot_animated_line()` simplifies the route alignment into a single
  straight line and shows the vehicles moving down this line.

- `plot_animated_map()` plots the full route's alignment and shows the
  vehicles moving through space.

Both functions allow the plotting of spatial features and labels for
these features. A `gganimate` object is returned, which can be further
modified and customized as desired.

## Usage

``` r
plot_animated_line(
  trajectory = NULL,
  distance_df = NULL,
  plot_trips = NULL,
  timestep = 5,
  distance_lim = NULL,
  center_vehicles = FALSE,
  feature_distances = NULL,
  transition_style = "linear",
  route_color = "coral",
  route_width = 3,
  route_alpha = 1,
  feature_shape = 21,
  feature_outline = "black",
  feature_fill = "white",
  feature_size = 2,
  feature_stroke = 1.25,
  feature_alpha = 1,
  veh_shape = 23,
  veh_outline = "grey30",
  veh_fill = "white",
  veh_size = 3,
  veh_stroke = 2,
  veh_alpha = 0.8,
  label_field = NULL,
  label_size = 3,
  label_alpha = 0.6,
  label_pos = "left"
)

plot_animated_map(
  shape_geometry,
  trajectory = NULL,
  distance_df = NULL,
  plot_trips = NULL,
  timestep = 5,
  distance_lim = NULL,
  center_vehicles = FALSE,
  feature_distances = NULL,
  background = "cartolight",
  background_zoom = 0,
  bbox_expand = NULL,
  transition_style = "linear",
  route_color = "coral",
  route_width = 3,
  route_alpha = 1,
  feature_shape = 21,
  feature_outline = "black",
  feature_fill = "white",
  feature_size = 2,
  feature_stroke = 1.25,
  feature_alpha = 1,
  veh_shape = 23,
  veh_outline = "grey30",
  veh_fill = "white",
  veh_size = 3,
  veh_stroke = 2,
  veh_alpha = 0.8,
  label_field = NULL,
  label_size = 3,
  label_alpha = 0.6,
  label_pos = "out"
)
```

## Arguments

- trajectory:

  Optional. A trajectory object, either a single trajectory or grouped
  trajectory. If provided, `distance_df` must not be provided. Default
  is `NULL`.

- distance_df:

  Optional. A dataframe of time and distance points. Must include at
  least `event_timestamp`, `distance`, and `trip_id_performed`. If
  provided, `trajectory` must not be provided. Default is `NULL`.

- plot_trips:

  Optional. A vector of `trip_id_performed`s to plot. Default is `NULL`,
  which will plot all trips provided in the `trajectory` or
  `distance_df`.

- timestep:

  Optional. If `trajectory` is provided, the time interval, in seconds,
  between interpolated observations to plot. Default is 5.

- distance_lim:

  Optional. A vector with `(minimum, maximum)` distance values to plot.

- center_vehicles:

  Optional. A boolean, should all vehicle points be centered to start at
  the same time (0 seconds)? Default is `FALSE`.

- feature_distances:

  Optional. A dataframe with at least numeric `distance` for features.
  Default is `NULL`.

- transition_style:

  Optional. A `gganimate` `transition_style`, specifying how the
  animation transitions from point to point. See
  [`gganimate::ease_aes()`](https://gganimate.com/reference/ease_aes.html).
  Default is `"linear"`.

- route_color:

  Optional. A color string for the color of the route alignment. Default
  is `"coral"`.

- route_width:

  Optional. A numeric, the linewidth of the route alignment. Default is
  3.

- route_alpha:

  Optional. A numeric, the opacity of the route alignment. Default is 1.

- feature_shape:

  Optional. A numeric specifying the `ggplot2` point shape, or a
  dataframe mapping an attribute in `feature_distances` to a shape. Must
  contain column `shape`. Default is 21 (circle).

- feature_outline:

  Optional. A color string, or a dataframe mapping an attribute in
  `feature_distances` to a color. Must contain column `outline`. Default
  is `"black"`.

- feature_fill:

  Optional. A color string, the inside fill of feature points. Default
  is `"white"`.

- feature_size:

  Optional. A numeric, the size of the feature point. Default is 2.

- feature_stroke:

  Optional. A numeric, the linewidth of the feature point outline.
  Default is 1.25.

- feature_alpha:

  Optional. A numeric, the opacity of the feature point. Default is 1.

- veh_shape:

  Optional. A numeric specifying the `ggplot2` point shape, or a
  dataframe mapping an attribute in `distance_df` or `trajectory` to a
  shape. Must contain column `shape`. Default is 23 (diamond).

- veh_outline:

  Optional. A color string, or a dataframe mapping an attribute in
  `distance_df` or `trajectory` to a color. Must contain column
  `outline`. Default is `"grey30"`.

- veh_fill:

  Optional. A color string, the inside fill of the vehicle point.
  Default is `"white"`.

- veh_size:

  Optional. A numeric, the size of the vehicle point. Default is 3.

- veh_stroke:

  Optional. A numeric, the linewidth of the vehicle point outline.
  Default is 2.

- veh_alpha:

  Optional. A numeric, the opacity of the vehicle point. Default is 0.8.

- label_field:

  Optional. A string specifying the column in `feature_distances` with
  which to label the feature lines. Default is `NULL`, where no labels
  will be plotted.

- label_size:

  Optional. The font size of the feature labels. Default is 3.

- label_alpha:

  Optional. The opacity of the feature labels. Default is 0.6.

- label_pos:

  Optional. A string specifying the label position on the graph. Options
  include:

  - `plot_animated_line()`: `"left"` or `"right"`. Default is `"left"`.

  - `plot_animated_map()`: cardinal or intermediate direction (e.g.,
    `"N"`, `"SW"`, etc.), or `"in"`/`"out"`. Default is `"out"`.

- shape_geometry:

  An SF object representing the route alignment. See
  [`get_shape_geometry()`](https://obrien-ben.github.io/transittraj/reference/get_shape_geometry.md).

- background:

  Optional. The OSM background (basemap) for the animation. See
  [`rosm::osm.image()`](https://rdrr.io/pkg/rosm/man/deprecated.html).
  Default is `"cartolight"`.

- background_zoom:

  Optional. The zoom, relative to the "correct" level, for the
  background basemap. Default is 0.

- bbox_expand:

  Optional. The distance by which to expand the plotting window in both
  directions. Default is `NULL`, which will expand the window by 0.05%
  of the larger dimension (or 0.0025% if `distance_lim` is provided).

## Value

A `gganimate` object.

## Details

### Input Trajectory Data

There are two ways to provide data to these plotting functions:

- A single or grouped trajectory object. This will use the direct
  trajectory function at a resolution controlled by `timestep`. This is
  simplest, and looks best when zooming in using `distance_lim`. The
  only attribute that can be mapped to if using a trajectory is
  `trip_id_performed`.

- A `distance_df` of TIDES AVL data. This will use the distance and time
  point pairs for plotting, and draw linearly between them. This will
  look similar to a plot using `trajectory` when zoomed out. It is most
  useful if you want to map formatting to attributes other than
  `trip_id_performed`, such as a vehicle or operator ID. If starting
  with a `trajectory`, but the additional control over formatting is
  desired, consider using
  [`predict()`](https://rdrr.io/r/stats/predict.html) to generate
  distance and time points to plot, then joining the desired attributes
  to the `trip_id_performed` column.

Note that only one of `trajectory` and `distance_df` can be used. If
both (or neither) are provided, an error will be thrown.

### Features and Labels

Often it is useful to plot the features of a route, such as its
stops/stations or the traffic signals it passes through. Use
`feature_distances` to provide information about spatial features to
plot. Each row in `feature_distances` should include at least a
`distance` column. Each of these rows will be plotted as a point on the
route line.

These features can also be labeled. Set `label_name` to a character
string corresponding to a field in `feature_distances` to generate
labels with this field as their text. The color of the label will
automatically match that of the feature they describe. The label
placement is controlled by `label_pos`, which has the following options
available:

- `plot_animated_line()`: Either `"left"` or `"right"` of the route
  line. The y-value will be that of the feature it describes.

- `plot_animated_map()`. A cardinal or intermediate direction (`"N"`,
  `"SE"`, etc.) relative to the feature point. Or, `"in"`/`"out"`,
  relative to the center of the plot.

Note that for `plot_animated_map()` the `feature_distances` must still
be linear distances, not a spatial datatype. To retrieve distance values
for spatial features, see
[`get_stop_distances()`](https://obrien-ben.github.io/transittraj/reference/get_stop_distances.md)
and
[`project_onto_route()`](https://obrien-ben.github.io/transittraj/reference/project_onto_route.md).

### Formatting Options

Once a layer is created on a `ggplot2` object, it is difficult to change
its formatting. As such, this function attempts to provide as much
flexibility in formatting its layers as possible. The resulting plot
includes three layers:

- Route line, controlled by `route_color`, `route_width`, and
  `route_alpha`.

- Features, controlled by `feature_fill`, `feature_outline`,
  `feature_shape`, `feature_size`, `feature_alpha`, and
  `feature_stroke`.

- Labels, controlled by `label_size`, `label_alpha`, and `label_pos`.

- Vehicles, controlled by `veh_fill`, `veh_outline`, `veh_shape`,
  `veh_size`, `veh_alpha`, and `veh_stroke`.

All of these formats can be controlled by inputting a single string or
numeric. The following attributes can also be modified using a
dataframe, mapping them to attributes of the layer:

- `veh_shape` and `feature_shape`: A dataframe with one column named
  `shape`, and another column sharing a name with a column in
  `distance_df` or `feature_distances` (or, if using `trajectory`, a
  column named `trip_id_performed`). The values in `shape` should be
  valid `ggplot2` point shapes, and the values in the mapping column
  should match the values in feature or trip column.

- `veh_outline` and `feature_outline`: A dataframe with one column named
  `outline`, and another column sharing a name with a column in
  `distance_df` or `feature_distances` (or, if using `trajectory`, a
  column named `trip_id_performed`). The values in `outline` should be
  valid color strings, and the values in the mapping column should match
  the values in feature or trip column.

Note that if inputting `trajectory`, instead of `distance_df`,
`veh_shape` and `veh_outline` can only be mapped to `trip_id_performed`.
If using `distance_df`, they may be mapped to any column in
`distance_df` (e.g., vehicle or operator IDs).

### Basemaps

The function `plot_animated_map()` has one additional layer to format:
the basemap beneath the route alignment. OpenStreetMaps basemaps are
used here. See a full list of available basemaps using
[`rosm::osm.image()`](https://rdrr.io/pkg/rosm/man/deprecated.html).

In addition to the map itself, the zoom level on the map can be adjusted
using `background_zoom`. This will describe a zoom level relative to the
"correct" level of your bounding box (i.e., what you what see if you
looked at that data in an online mapping platform). Setting to a
negative value (zooming out) will substantially speed up rendering, but
will give a much lower resolution may.

Finally, the bounding box of the basemap can also be set. The bounding
box is defined relative to the spatial range of `trajectory` or
`distance_df`. The default is expansion is 0.05% (0.0025% for
`distance_lim != NULL`) of the larger dimension (northing or easting) of
the vehicle location bounding box. To customize, det `bbox_expand` to
some numeric in the distance units of the `shape_geometry`'s spatial
projection (e.g., meters if using a UTM projection).

For examples and a more in-depth discussion, see (xyz).
