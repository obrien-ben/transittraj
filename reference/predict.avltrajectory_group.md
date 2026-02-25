# Interpolate time or distance points using AVL trajectories.

Using a function stored in a grouped or single trajectory object, new
points will be interpolated along a trajectory. Depending on whether
new_times or new_distances is provided, the function will utilize the
direct or inverse trajectory function.

## Usage

``` r
# S3 method for class 'avltrajectory_group'
predict(
  object,
  new_times = NULL,
  new_distances = NULL,
  deriv = 0,
  trips = NULL,
  ...
)

# S3 method for class 'avltrajectory_single'
predict(object, new_times = NULL, new_distances = NULL, deriv = 0, ...)
```

## Arguments

- object:

  The single or grouped trajectory object.

- new_times:

  Optional. A vector of numeric timepoints, or a dataframe with at least
  the column `"event_timestamp"` of new timepoints to interpolate at.
  Default is `NULL`.

- new_distances:

  Optional. A vector of numeric distances, or a dataframe with at least
  the column `"distance"` of new distances to interpolate at. Default is
  `NULL`.

- deriv:

  Optional. The derivative with which to calculate at. Default is 0.

- trips:

  Optional. A vector of `trip_id_performed`s to interpolate for. Default
  is `NULL`, which will use all trips found in the trajectory object.

- ...:

  Other parameters (not used).

## Value

The input dataframe, with an additional column `"interp"` of the
interpolated values requested, and an additional `"trip_id_performed"`
column will all trips for which that point is within range.

## Details

This function is the recommended way to use a fit trajectory function.
It has a few key features:

### Interpolate for Distance or Time

If `new_times` is provided, the function will find the `distance` of
each trip at each new time using the direct trajectory function.
Conversely, if `new_distances` is provided, the function will find the
`event_timestamp` at which each trip crossed that distance using the
inverse trajectory function. If an input trajectory object does not
contain an inverse function, an error will be thrown.

These new points can be either vectors or dataframes:

- If the input is a vector, the returned value will be a dataframe
  associating each trip with each new point and a column `interp`
  indicating the interpolated value.

- If the input is a dataframe, it must contain a column names
  `event_timestamp` or `distance`, depending on whether it is put into
  `new_times` or `new_distances`. All other columns will be preserved in
  the output. Each row will be duplicated for each trip, and a column
  `interp` will be added indicating the interpolated value.

The latter option is particularly useful for finding crossing times of
particular features, or the positions at notable points in time. For
instance, `new_distance` may be a dataframe of corridors, with a column
`name` for the corridor name and `inout` for whether each row is the
entrance or exit to the corridor. The returned value will append the
column `trip_id_performed` for each trip that crosses those distances,
and `interp` for the time the trip passes the entrance or exit.

### Finding Derivatives

Depending on the `interp_method` used when fitting the trajectory
object, a its derivative may be able to be found:

- `interp_method = "linear"`. This will not allow derivatives. This is
  because, at each observation, the piecewise linear function is not
  differentiable.

- `interp_method` is a spline from
  [`stats::splinefun()`](https://rdrr.io/r/stats/splinefun.html). This
  will typically be differentiable up to the third degree.

The derivative returned (as column `interp`) is the derivative of
distance with respect to time. This means the first derivative is
velocity, second is acceleration, and third is jerk. The derivative is
taken from the direct trajectory, not the inverse, and the inverse
trajectory cannot be used to find derivatives. This means that if
`new_distances` is provided, `deriv ` must equal 0. If starting from
distance values, but derivatives are desired, consider interpolating for
timepoints first, then using these as `new_times` to find the
derivative.

### Prevents Extrapolation

By default, many fit interpolating curves will allow extrapolation
(i.e., the input of an `event_timestamp` beyond the original time domain
of the trip). This is especially true for splines fit using
[`stats::splinefun()`](https://rdrr.io/r/stats/splinefun.html). In
general, this will not be reasonable for transit vehicles: time points
should be constrained by the time that a trip has actually been
observed, and distances should be constrained to the part of a route a
trip actually ran.

This function uses the maximum and minimum time and distance values
stored in the trajectory object to identify if an input `new_times` or
`new_distances` is beyond the domain/range of each trip individually.
The returned output will only include `interp` values for trips within
the domain/range of the input.

### Accessing the Raw Trajectory Function

Because of the above features and protections, it is recommend that
these [`predict()`](https://rdrr.io/r/stats/predict.html) functions are
used to access the fit trajectory and inverse trajectory functions.
However, if the raw function itself is desired, it can be accessed using
`attr(trajectory, "traj_fun")` or `attr(trajectory, "inv_traj_fun")`.
For a group trajectory object, these will return lists of individual
trip functions indexed by `trip_id_performed`; for single trajectory
objects, these will return the single function for that trip.
