# Corrects distance observations, and optionally speeds, to be weakly or strictly monotonic.

Due to error in GPS position and speed measurements, raw AVL data is
often not monotonic, creating difficulties for advanced analyses. This
function presents a variety of options to correct data, resulting in
distance values, and optionally speeds, which form a strictly or weakly
monotonic curve. See `Details` for more information.

## Usage

``` r
make_monotonic(
  distance_df,
  correct_speed = FALSE,
  add_distance_error = 0,
  return_changes = FALSE
)
```

## Arguments

- distance_df:

  A dataframe of linearized AVL data. Must include `trip_id_performed`,
  `event_timestamp`, and `distance`. If `correct_speed = TRUE`, must
  also include `speed`.

- correct_speed:

  Optional. A boolean, should speeds be corrected to meet adjusted
  distances and Fritsch-Carlson conditions? Default is `FALSE`.

- add_distance_error:

  Optional. If non-zero, each "flat" observation will be adjusted by
  this amount forwards, in units of input `distance`. Default is 0.

- return_changes:

  Optional. Should a dataframe of each observation changed be returned?
  Default is `FALSE`.

## Value

The input `distance_df` with distances and speeds adjusted. If
`return_changes = TRUE`, a dataframe with observations changed.

## Details

There are two primary types of monotonicity:

- Weak monotonicity: The trajectory is increasing or constant. To make
  points weakly monotonic, this function replaces each point with the
  cumulative maximum `distance` value at that point in the trip. This
  means that backtracking points will be "pulled up".

- Strict monotonicity: The trajectory is increasing only, never
  constant. To make points strictly monotonic, we first begin with a
  weakly monotonic trajectory. Then, constant portions (adjacent points
  with equal `distance` values) are identified, and `add_distance_error`
  is added to each point. The function identifies and prevents
  "overshoots". Effectively, this gives flat portions of the trajectory
  a slight upward slope.

Weak monotonicity most accurately describes real transit vehicle
trajectories: we expect the vehicle to either move forwards, or stand
still at a stop. However, strict monotonicity is a nice mathematical
property that allows us to find the inverse trajectory (i.e., retrieve
time as a function of distance). Choose between these two options by
setting `add_distance_error`. If `add_distance_error = 0` (the default),
a weakly monotonic trajectory is returned. Otherwise, the trajectory
will be strictly monotonic.

In addition to distance corrections, some applications (e.g., fitting a
velocity-informed interpolation spline) require speeds to satisfy
certain monotonic conditions. If `correct_speed = TRUE`, the following
corrections will be made:

- For strict monotonicity (`add_distance_error > 0`), speeds must be
  non-zero. At each point where the recorded `speed == 0`, the speed
  will be replaced by `add_distance_error` divided by the time between
  that point and the previous point.

- For both strict and weak monotonicity, speeds will be adjusted to meet
  the [Fritsch-Carlson
  (1980)](https://epubs.siam.org/doi/10.1137/0717021) constraints.
  Often, only a handful of input `speed` values will be adjusted.

If recorded speed values are not present, set `correct_speed = FALSE`.
However, if you are interested in later fitting a velocity-informed
interpolating curve, such as Fritsch-Carlson's piecewise cubic
polynomials, consider setting `correct_speed = TRUE` to guarantee a
monotonic interpolating curve.

After using this function to perform corrections, use
`validate_montonicity()` to check if weak, strict, and Fritsch-Carlson
speed conditions are met.
