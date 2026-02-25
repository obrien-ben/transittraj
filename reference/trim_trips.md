# Removes observations occurring before a trip's minimum distance, or after a trip's maximum distance.

Sometimes observations will be recorded under a trip ID while a vehicle
is still traveling in the opposite direction. Conversely, a trip may
continue recording as it begins traversing the opposite direction. This
function attempts to remove these observations by identifying each
trip's minimum (beginning) and maximum (ending) distance, then filtering
to only observations after and before these points. For both ends, the
first occurrence of the beginning/maximum value is used.

## Usage

``` r
trim_trips(distance_df, trim_type = "both", return_removals = FALSE)
```

## Arguments

- distance_df:

  A dataframe of linearized AVL data. Must include `trip_id_performed`,
  `event_timestamp`, and `distance`.

- trim_type:

  Optional. A string, indicating whether the beginning of trips, end of
  trips, or both beginning and end of trips should be trimmed. Must be
  one of "beginning", "end", or "both". Default is "beginning".

- return_removals:

  Optional. A boolean, should the function return a dataframe of points
  removed and why? Default is `FALSE`.

## Value

The input `distance_df` with violating points removed. If
`return_removals = TRUE`, a dataframe with observations removed and why.
