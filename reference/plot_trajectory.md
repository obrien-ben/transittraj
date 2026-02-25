# Plot vehicle trajectories or AVL data.

This function use the input trajectory object or TIDES AVL data to draw
a trajectory plot (i.e., linear distance versus time) for each trip.
This function allows for the plotting of spatial features and labels for
these features. A `ggplot2` object is returned, which can be further
modified and customized as desired.

## Usage

``` r
plot_trajectory(
  trajectory = NULL,
  distance_df = NULL,
  plot_trips = NULL,
  timestep = 5,
  distance_lim = NULL,
  center_trajectories = FALSE,
  feature_distances = NULL,
  traj_color = "coral",
  traj_type = "solid",
  traj_width = 1,
  traj_alpha = 1,
  feature_color = "grey30",
  feature_type = "dashed",
  feature_width = 0.8,
  feature_alpha = 0.8,
  label_field = NULL,
  label_size = 3,
  label_alpha = 0.6,
  label_pos = "left"
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

- center_trajectories:

  Optional. A boolean, should all trajectories be centered to start at
  the same time (0 seconds)? Default is `FALSE`.

- feature_distances:

  Optional. A dataframe with at least numeric `distance` for features.
  Default is `NULL`.

- traj_color:

  Optional. A color string, or a dataframe mapping an attribute in
  `distance_df` or `trip_id_performed` in `trajectory` to a color. Must
  contain column `color`. Default is `"coral"`.

- traj_type:

  Optional. A string specifying the `ggplot2` linetype, or a dataframe
  mapping an attribute in `distance_df` or `trip_id_performed` in
  `trajectory` to a linetype. Must contain column `linetype`. Default is
  `"solid"`.

- traj_width:

  Optional. A numeric, the width of the trajectory line. Default is 1.

- traj_alpha:

  Optional. A numeric, the opacity of the trajectory line. Default is 1.

- feature_color:

  Optional. A color string, or a dataframe mapping an attribute in
  `feature_distances` to a color. Must contain column `color`. Default
  is `grey30`.

- feature_type:

  Optional. A string specifying the `ggplot2` linetype, or a dataframe
  mapping an attribute in `feature_distances` to a linetype. Must
  contain column `linetype`. Default is `"dashed"`.

- feature_width:

  Optional. A numeric, the width of the feature line. Default is 0.8.

- feature_alpha:

  Optional. A numeric, the opacity of the feature point. Default is 1.

- label_field:

  Optional. A string specifying the column in `feature_distances` with
  which to label the feature lines. Default is `NULL`, where no labels
  will be plotted.

- label_size:

  Optional. The font size of the feature labels. Default is 3.

- label_alpha:

  Optional. The opacity of the feature labels. Default is 0.6.

- label_pos:

  Optional. A string specifying the label position on the graph. Must be
  either `"left"` or `"right"`. Default is `"left"`.

## Value

A `ggplot2` object.

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
`distance` column. Each of these rows will be plotted as a horizontal
line across the graph.

These features can also be labeled. Set `label_name` to a character
string corresponding to a field in `feature_distances` to generate
labels with this field as their text. The color of the label will
automatically match that of the feature they describe. The label
placement is controlled by `label_pos`, which can be set to `"left"` or
`"right"`.

### Formatting Options

Once a layer is created on a `ggplot2` object, it is difficult to change
its formatting. As such, this function attempts to provide as much
flexibility in formatting its layers as possible. The resulting plot
includes three layers:

- Vehicle trajectories, controlled by `traj_color`, `traj_type`, and
  `traj_width`, and `traj_alpha`.

- Features, controlled by `feature_color`, `feature_type`,
  `feature_width`, and `feature_alpha`.

- Labels, controlled by `label_size`, `label_alpha`, and `label_pos`.

All of these formats can be controlled by inputting a single string or
numeric. The following attributes can also be modified using a
dataframe, mapping them to attributes of the layer:

- `traj_type` and `feature_type`: A dataframe with one column named
  `linetype`, and another column sharing a name with a column in
  `distance_df` or `feature_distances` (or, if using `trajectory`, a
  column named `trip_id_performed`). The values in `linetype` should be
  valid `ggplot2` linetypes, and the values in the mapping column should
  match the values in feature or trip column.

- `traj_color` and `feature_color`: A dataframe with one column named
  `color`, and another column sharing a name with a column in
  `distance_df` or `feature_distances` (or, if using `trajectory`, a
  column named `trip_id_performed`). The values in `color` should be
  valid color strings, and the values in the mapping column should match
  the values in feature or trip column.

Note that if inputting `trajectory`, instead of `distance_df`,
`veh_shape` and `traj_color` and `traj_type` can only be mapped to
`trip_id_performed`. If using `distance_df`, they may be mapped to any
column in `distance_df` (e.g., vehicle or operator IDs).
