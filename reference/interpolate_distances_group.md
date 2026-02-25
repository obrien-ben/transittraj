# Distance interpolation for group trajectories

Not intended for external use.

## Usage

``` r
interpolate_distances_group(
  trip_extremes,
  new_times,
  trajectory_function,
  deriv
)
```

## Arguments

- trip_extremes:

  DF of max and min distance values

- new_times:

  DF of new time points

- trajectory_function:

  trajectory function list

- deriv:

  derivative to use

## Value

DF of interpolated values
