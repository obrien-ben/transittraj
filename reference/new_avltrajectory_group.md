# Constructor for grouped trajectory class

This superclass holds a single, combined trajectory function. Plus
vectors of all trip IDs described by that function, and vectors of the
time & distance ranges of those trips. Not intended for external use.

## Usage

``` r
new_avltrajectory_group(
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
  used_speeds = FALSE,
  ...,
  class = character()
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

- ...:

  Other inputs

- class:

  Object class

## Value

Grouped trajectory object
