# Check if an AVL dataframe satisfies assumptions of monotonicity.

This function checks whether the provided AVL dataframe of linearized
distances satisfies three conditions:

- Weak monotonicity, either flat or increasing

- Strict monotonicty, increasing only

- Speeds satisfy Fritsch-Carlson constraints

See
[`make_monotonic()`](https://obrien-ben.github.io/transittraj/reference/make_monotonic.md)
for more information.

## Usage

``` r
validate_monotonicity(distance_df, check_speed = FALSE, return_full = FALSE)
```

## Arguments

- distance_df:

  A dataframe of linearized AVL data. Must include `trip_id_performed`,
  `event_timestamp`, and `distance`. If `check_speed = TRUE`, must also
  include `speed`.

- check_speed:

  Optional. A boolean, should the Fritsch-Carlson conditions

- return_full:

  Optional. Should a dataframe of each point checked be returned?
  Default is `FALSE`. for slopes be checked? Default is `FALSE`, where
  the speed check will return `NA`.

## Value

A named vector of booleans indicating whether each of the three
conditions are satisfied.
