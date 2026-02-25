# Package index

## All functions

- [`clean_incomplete_trips()`](https://obrien-ben.github.io/transittraj/reference/clean_incomplete_trips.md)
  : Filters out entire trips which do not meet distance or duration
  requirements.

- [`clean_jumps()`](https://obrien-ben.github.io/transittraj/reference/clean_jumps.md)
  : Applies median filters to detect large jumps (i.e., outliers) in the
  trajectories.

- [`clean_overlapping_subtrips()`](https://obrien-ben.github.io/transittraj/reference/clean_overlapping_subtrips.md)
  : Removes trips with multiple overlapping operators or vehicles
  assigned to the same trip number.

- [`correct_speeds_fun()`](https://obrien-ben.github.io/transittraj/reference/correct_speeds_fun.md)
  : Corrects speeds to Fristch-Carlson constraints, recursively.

- [`export_animation()`](https://obrien-ben.github.io/transittraj/reference/export_animation.md)
  : Save your animation at a desired quality.

- [`filter_by_route()`](https://obrien-ben.github.io/transittraj/reference/filter_by_route.md)
  : Filter GTFS to a desired route(s) and direction(s).

- [`get_gtfs_trajectory_fun()`](https://obrien-ben.github.io/transittraj/reference/get_gtfs_trajectory_fun.md)
  : Fits continuous trajectory interpolating curves from GTFS schedule
  data.

- [`get_inverse_traj()`](https://obrien-ben.github.io/transittraj/reference/get_inverse_traj.md)
  : Calculates numerical inverse of a trajectory function

- [`get_linear_distances()`](https://obrien-ben.github.io/transittraj/reference/get_linear_distances.md)
  : Linearizes latitude-longitude GPS points to a provided route shape.

- [`get_shape_geometry()`](https://obrien-ben.github.io/transittraj/reference/get_shape_geometry.md)
  : Get the geometry of a route shape.

- [`get_stop_distances()`](https://obrien-ben.github.io/transittraj/reference/get_stop_distances.md)
  : Get the distances of stops along routes.

- [`get_trajectory_fun()`](https://obrien-ben.github.io/transittraj/reference/get_trajectory_fun.md)
  : Fits continuous trajectory interpolating curves from transit AVL
  data.

- [`interpolate_distances_group()`](https://obrien-ben.github.io/transittraj/reference/interpolate_distances_group.md)
  : Distance interpolation for group trajectories

- [`interpolate_distances_single()`](https://obrien-ben.github.io/transittraj/reference/interpolate_distances_single.md)
  : Distance interpolation for single trajectories

- [`interpolate_times_group()`](https://obrien-ben.github.io/transittraj/reference/interpolate_times_group.md)
  : Time interpolation for grouped trajectories

- [`interpolate_times_single()`](https://obrien-ben.github.io/transittraj/reference/interpolate_times_single.md)
  : Time interpolation for single trajectories

- [`make_monotonic()`](https://obrien-ben.github.io/transittraj/reference/make_monotonic.md)
  : Corrects distance observations, and optionally speeds, to be weakly
  or strictly monotonic.

- [`new_avltrajectory_group()`](https://obrien-ben.github.io/transittraj/reference/new_avltrajectory_group.md)
  : Constructor for grouped trajectory class

- [`new_avltrajectory_single()`](https://obrien-ben.github.io/transittraj/reference/new_avltrajectory_single.md)
  : Constructor for single trajectory class

- [`plot(`*`<avltrajectory_group>`*`)`](https://obrien-ben.github.io/transittraj/reference/plot.avltrajectory_group.md)
  [`plot(`*`<avltrajectory_single>`*`)`](https://obrien-ben.github.io/transittraj/reference/plot.avltrajectory_group.md)
  : \#' Quickly plots an AVL trajectory.

- [`plot_animated_line()`](https://obrien-ben.github.io/transittraj/reference/plot_animated_line.md)
  [`plot_animated_map()`](https://obrien-ben.github.io/transittraj/reference/plot_animated_line.md)
  : Animate vehicle trajectory or AVL data.

- [`plot_df_setup()`](https://obrien-ben.github.io/transittraj/reference/plot_df_setup.md)
  : Set up dataframe & validate of point objects for vehicle animations

- [`plot_format_setup()`](https://obrien-ben.github.io/transittraj/reference/plot_format_setup.md)
  : Function to set up plot formats.

- [`plot_interactive_gtfs()`](https://obrien-ben.github.io/transittraj/reference/plot_interactive_gtfs.md)
  : Generates a Leaflet viewer of GTFS routes and stops.

- [`plot_trajectory()`](https://obrien-ben.github.io/transittraj/reference/plot_trajectory.md)
  : Plot vehicle trajectories or AVL data.

- [`predict(`*`<avltrajectory_group>`*`)`](https://obrien-ben.github.io/transittraj/reference/predict.avltrajectory_group.md)
  [`predict(`*`<avltrajectory_single>`*`)`](https://obrien-ben.github.io/transittraj/reference/predict.avltrajectory_group.md)
  : Interpolate time or distance points using AVL trajectories.

- [`print(`*`<avltrajectory_group>`*`)`](https://obrien-ben.github.io/transittraj/reference/print.avltrajectory_group.md)
  [`print(`*`<avltrajectory_single>`*`)`](https://obrien-ben.github.io/transittraj/reference/print.avltrajectory_group.md)
  : Print function for AVL trajectories

- [`project_onto_route()`](https://obrien-ben.github.io/transittraj/reference/project_onto_route.md)
  : Projects points to linear distances along a route shape.

- [`set_globals`](https://obrien-ben.github.io/transittraj/reference/set_globals.md)
  : Set global variables to use throughout, silencing notes during
  check.

- [`summary(`*`<avltrajectory_group>`*`)`](https://obrien-ben.github.io/transittraj/reference/summary.avltrajectory_group.md)
  [`summary(`*`<avltrajectory_single>`*`)`](https://obrien-ben.github.io/transittraj/reference/summary.avltrajectory_group.md)
  : Summary function for AVL trajectories.

- [`trim_trips()`](https://obrien-ben.github.io/transittraj/reference/trim_trips.md)
  : Removes observations occurring before a trip's minimum distance, or
  after a trip's maximum distance.

- [`validate_gtfs_input()`](https://obrien-ben.github.io/transittraj/reference/validate_gtfs_input.md)
  : Function to quickly validate whether input GTFS has required tables
  and fields within those tables.

- [`validate_input_to_tides()`](https://obrien-ben.github.io/transittraj/reference/validate_input_to_tides.md)
  :

  Uses `validate_tides` and an input vector of needed fields to check
  whether an AVL DF meets the requirements of a given function.

- [`validate_monotonicity()`](https://obrien-ben.github.io/transittraj/reference/validate_monotonicity.md)
  : Check if an AVL dataframe satisfies assumptions of monotonicity.

- [`validate_shape_geometry()`](https://obrien-ben.github.io/transittraj/reference/validate_shape_geometry.md)
  : Function to quickly validate whether an input shape_geometry meets
  needs

- [`validate_tides()`](https://obrien-ben.github.io/transittraj/reference/validate_tides.md)
  : Check if an AVL dataframe meets TIDES standards.

- [`wmata_avl`](https://obrien-ben.github.io/transittraj/reference/wmata_avl.md)
  : WMATA Bus Automatic Vehicle Location Data

- [`wmata_gtfs`](https://obrien-ben.github.io/transittraj/reference/wmata_gtfs.md)
  : WMATA Bus GTFS
