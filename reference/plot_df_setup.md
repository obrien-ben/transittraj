# Set up dataframe & validate of point objects for vehicle animations

Intended for internal use only

## Usage

``` r
plot_df_setup(
  trajectory,
  distance_df,
  plot_trips,
  timestep,
  distance_lim,
  feature_distances,
  center_vehicles
)
```

## Arguments

- trajectory:

  Single or grouped trajectory object.

- distance_df:

  AVL distance DF.

- plot_trips:

  Vector of trip_id_performed to plot.

- timestep:

  Time in seconds for interpolation.

- distance_lim:

  Vector of (minimum, maximum) distance to plot.

- feature_distances:

  Linear distance to features.

- center_vehicles:

  Should vehicles be centered

## Value

plotting dataframe (trips_df)
