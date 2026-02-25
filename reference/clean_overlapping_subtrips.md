# Removes trips with multiple overlapping operators or vehicles assigned to the same trip number.

In some AVL vendors, multiple vehicles or operators may be logged to the
same trip ID at the same time. This may be acceptable in some scenarios
(e.g., a vehicle/operator tradeoff mid-trip). Other times, it may be an
error, with these distinct (trip, vehicle, operator) truples running
simulataneously. This function identifies both scenarios, and gives the
option to remove one or both.

## Usage

``` r
clean_overlapping_subtrips(
  distance_df,
  check_operator = FALSE,
  remove_single_observations = TRUE,
  remove_non_overlapping = FALSE,
  return_removals = FALSE
)
```

## Arguments

- distance_df:

  A dataframe of linearized AVL data. Must include `event_timestamp`,
  `trip_id_performed`, and `vehicle_id`. Optionally, may include
  `operator_id`.

- check_operator:

  Optional. A boolean, should overlaps of multiple `operator_id`s be
  checked for? Default is FALSE.

- remove_single_observations:

  Optional. A boolean, should subtrips with only one observation be
  removed? Default is TRUE.

- remove_non_overlapping:

  Optional. A boolean, should trips with multiple vehicles or operators
  that do not overlap be removed? Default is FALSE.

- return_removals:

  Optional. A boolean, should the function return a dataframe of trips
  removed and why? Default is FALSE.

## Value

The input distance_df, with violating trips removed. If return_removals
= TRUE, a dataframe with trip IDs and the reason why it was identified
for removal.
