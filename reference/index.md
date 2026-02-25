# Package index

## GTFS Helpers

Functions to manipulate tidygtfs objects.

- [`filter_by_route()`](https://obrien-ben.github.io/transittraj/reference/filter_by_route.md)
  : Filter GTFS to a desired route(s) and direction(s).
- [`get_shape_geometry()`](https://obrien-ben.github.io/transittraj/reference/get_shape_geometry.md)
  : Get the geometry of a route shape.
- [`project_onto_route()`](https://obrien-ben.github.io/transittraj/reference/project_onto_route.md)
  : Projects points to linear distances along a route shape.
- [`get_stop_distances()`](https://obrien-ben.github.io/transittraj/reference/get_stop_distances.md)
  : Get the distances of stops along routes.
- [`plot_interactive_gtfs()`](https://obrien-ben.github.io/transittraj/reference/plot_interactive_gtfs.md)
  : Generates a Leaflet viewer of GTFS routes and stops.

## AVL Cleaning

Functions to clean AVL data.

- [`get_linear_distances()`](https://obrien-ben.github.io/transittraj/reference/get_linear_distances.md)
  : Linearizes latitude-longitude GPS points to a provided route shape.
- [`clean_overlapping_subtrips()`](https://obrien-ben.github.io/transittraj/reference/clean_overlapping_subtrips.md)
  : Removes trips with multiple overlapping operators or vehicles
  assigned to the same trip number.
- [`clean_jumps()`](https://obrien-ben.github.io/transittraj/reference/clean_jumps.md)
  : Applies median filters to detect large jumps (i.e., outliers) in the
  trajectories.
- [`clean_incomplete_trips()`](https://obrien-ben.github.io/transittraj/reference/clean_incomplete_trips.md)
  : Filters out entire trips which do not meet distance or duration
  requirements.
- [`trim_trips()`](https://obrien-ben.github.io/transittraj/reference/trim_trips.md)
  : Removes observations occurring before a trip's minimum distance, or
  after a trip's maximum distance.
- [`make_monotonic()`](https://obrien-ben.github.io/transittraj/reference/make_monotonic.md)
  : Corrects distance observations, and optionally speeds, to be weakly
  or strictly monotonic.

## Trajectory Construction

Functions to build trajectory objections.

- [`get_trajectory_fun()`](https://obrien-ben.github.io/transittraj/reference/get_trajectory_fun.md)
  : Fits continuous trajectory interpolating curves from transit AVL
  data.
- [`get_gtfs_trajectory_fun()`](https://obrien-ben.github.io/transittraj/reference/get_gtfs_trajectory_fun.md)
  : Fits continuous trajectory interpolating curves from GTFS schedule
  data.

## Trajectory Plotting

Functions to plot trajectories.

- [`plot(`*`<avltrajectory_group>`*`)`](https://obrien-ben.github.io/transittraj/reference/plot.avltrajectory_group.md)
  [`plot(`*`<avltrajectory_single>`*`)`](https://obrien-ben.github.io/transittraj/reference/plot.avltrajectory_group.md)
  : \#' Quickly plots an AVL trajectory.
- [`plot_animated_line()`](https://obrien-ben.github.io/transittraj/reference/plot_animated_line.md)
  [`plot_animated_map()`](https://obrien-ben.github.io/transittraj/reference/plot_animated_line.md)
  : Animate vehicle trajectory or AVL data.
- [`plot_trajectory()`](https://obrien-ben.github.io/transittraj/reference/plot_trajectory.md)
  : Plot vehicle trajectories or AVL data.
- [`export_animation()`](https://obrien-ben.github.io/transittraj/reference/export_animation.md)
  : Save your animation at a desired quality.

## Methods for Trajectories

Functions for using trajectories.

- [`predict(`*`<avltrajectory_group>`*`)`](https://obrien-ben.github.io/transittraj/reference/predict.avltrajectory_group.md)
  [`predict(`*`<avltrajectory_single>`*`)`](https://obrien-ben.github.io/transittraj/reference/predict.avltrajectory_group.md)
  : Interpolate time or distance points using AVL trajectories.
- [`print(`*`<avltrajectory_group>`*`)`](https://obrien-ben.github.io/transittraj/reference/print.avltrajectory_group.md)
  [`print(`*`<avltrajectory_single>`*`)`](https://obrien-ben.github.io/transittraj/reference/print.avltrajectory_group.md)
  : Print function for AVL trajectories
- [`summary(`*`<avltrajectory_group>`*`)`](https://obrien-ben.github.io/transittraj/reference/summary.avltrajectory_group.md)
  [`summary(`*`<avltrajectory_single>`*`)`](https://obrien-ben.github.io/transittraj/reference/summary.avltrajectory_group.md)
  : Summary function for AVL trajectories.

## Data Validation

Functions for validating data.

- [`validate_tides()`](https://obrien-ben.github.io/transittraj/reference/validate_tides.md)
  : Check if an AVL dataframe meets TIDES standards.
- [`validate_monotonicity()`](https://obrien-ben.github.io/transittraj/reference/validate_monotonicity.md)
  : Check if an AVL dataframe satisfies assumptions of monotonicity.

## Supporting Datasets

Datasets to test with.

- [`wmata_avl`](https://obrien-ben.github.io/transittraj/reference/wmata_avl.md)
  : WMATA Bus Automatic Vehicle Location Data
- [`wmata_gtfs`](https://obrien-ben.github.io/transittraj/reference/wmata_gtfs.md)
  : WMATA Bus GTFS
