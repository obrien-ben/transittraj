# Constructor for single trajectory class

This is a subclass (special case) of a grouped trajectory, in which
there is only one trip. Trajectory function is inteded to describe only
one trip. Associated trip properties (ID & ranges) should describe only
that trip. Not intended for external use.

## Usage

``` r
new_avltrajectory_single(
  trip_id_performed = character(),
  traj_fun,
  inv_traj_fun = NULL,
  min_dist,
  max_dist,
  min_time,
  max_time,
  traj_type,
  inv_tol = NULL,
  max_deriv = 0,
  used_speeds = FALSE
)
```

## Arguments

- trip_id_performed:

  Character if trip ids.

- traj_fun:

  List or single trajectory functions.

- inv_traj_fun:

  List or single inverse trajectory functions.

- min_dist:

  Vector if minimum distance values.

- max_dist:

  Vector of maximum distance values.

- min_time:

  Vector of minimum time values.

- max_time:

  Vector of maximum time values.

- traj_type:

  Interp method character string

- inv_tol:

  Tolerance used in numeric inverse

- max_deriv:

  Max derivative allowed

- used_speeds:

  Whether speeds were used

## Value

Single trajectory object
