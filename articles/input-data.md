# Understanding Data Inputs

## Introduction

This vignette introduces you to the input data used by `transittraj`.
For most projects, there are two important data sources:

- *Automatic vehicle location (AVL) data*: A set of GPS
  latitude-longitude points describing a transit vehicle’s location over
  time. This package requires that AVL data follow the
  [TIDES](https://tides-transit.org/main/) `vehicle-location` table
  schema.

- *GTFS feed*: The routes, trips, and schedules a transit vehicle
  follows. The most important part is the `shapes` file, which tells us
  the route alignment we expect our AVL data to follow.

We will introduce and discuss these data sources using the two public
datasets included with `transittraj`: An archive of WMATA’s GTFS-rt
feed, `wmata_avl`, and an archive of WMATA’s static GTFS feed,
`wmata_gtfs`.

Before we begin, let’s add some needed packages:

``` r
library(transittraj)
library(tidytransit)
```

## Exploring Input Data

### Automatic Vehicle Location Data

`transittraj` is designed to clean and process AVL data. Unfortunately,
AVL data does not have a widely-adopted standardized format. In the
interest of standardizing inputs to the package’s functions,
`transittraj` is designed to take in files adhering to the TIDES
`vehicle-location` standard table schema. TIDES is a new open standard
intended to standardize transit data types not covered by GTFS. The
TIDES `vehicle-location` table schema is described
[here](https://tides-transit.org/main/tables/#vehicle-locations).

Let’s see what TIDES AVL data looks like. The `wmata_avl` dataset
provided with `transittraj` is reformatted from GTFS-rt to meet TIDES’s
needs. Below is a peak:

``` r
head(wmata_avl)
#>   location_ping_id vehicle_id trip_id_performed service_date route_id
#> 1                0       4582          30095100   2026-02-16      D96
#> 2                1       5461          18632100   2026-02-16      C53
#> 3                2       5463            698100   2026-02-16      C53
#> 4                3       5464          14078100   2026-02-16      C53
#> 5                4       5466          25836100   2026-02-16      C53
#> 6                5       5470           8560100   2026-02-16      C53
#>   direction_id latitude longitude   speed trip_stop_sequence
#> 1            1 38.93342 -77.07974  8.8392                 36
#> 2            0 38.92353 -77.05198  0.0000                 63
#> 3            1 38.91215 -77.01222  7.9248                 19
#> 4            0 38.91594 -77.02111 10.9728                 52
#> 5            0 38.84528 -76.98762  0.0000                  2
#> 6            1 38.91702 -77.03714  0.0000                  9
#>       event_timestamp stop_id
#> 1 2026-02-16 10:58:35    7752
#> 2 2026-02-16 10:58:09    7219
#> 3 2026-02-16 10:58:23   17574
#> 4 2026-02-16 10:58:31    6843
#> 5 2026-02-16 10:58:27   13111
#> 6 2026-02-16 10:58:14    6879
```

To check whether an input dataframe meets our needs, we can run
`validatde_tides()`. This will check whether the needed fields are
present, and whether they have the correct data type.

``` r
wmata_tides_val <- validate_tides(avl_df = wmata_avl)
wmata_tides_val
#>      required_field required_field_type field_present actual_field_type
#> 1  location_ping_id           character          TRUE         character
#> 2 trip_id_performed           character          TRUE         character
#> 3   event_timestamp             POSIXct          TRUE           POSIXct
#> 4        vehicle_id           character          TRUE         character
#> 5       operator_id           character         FALSE              <NA>
#> 6         longitude             numeric          TRUE           numeric
#> 7          latitude             numeric          TRUE           numeric
#> 8          distance             numeric         FALSE              <NA>
#> 9             speed             numeric          TRUE           numeric
#>   field_type_ok field_ok
#> 1          TRUE     TRUE
#> 2          TRUE     TRUE
#> 3          TRUE     TRUE
#> 4          TRUE     TRUE
#> 5            NA    FALSE
#> 6          TRUE     TRUE
#> 7          TRUE     TRUE
#> 8            NA    FALSE
#> 9          TRUE     TRUE
```

This dataset does not meet two requirements: first, it is missing an
`operator_id` column. This is okay; `operator_id` is not required by any
`transittraj` functions, though there are some that benefit from it.
Second, the dataset is missing `distance`. This is something we’ll
calculate in the cleaning process.

Read more about `transittraj`’s data requirements using
[`help(validate_tides)`](https://obrien-ben.github.io/transittraj/reference/validate_tides.md).
Before running, each `transittraj` function will check that your input
dataframe has the fields and data types that that function requires.

### GTFS Feed

A GTFS feed gives us information we need to effectively use our AVL
data. `transittraj` is designed to use `tidygtfs` objects from the
`tidytransit` package. Let’s look at the GTFS object `wmata_gtfs`, which
complements the `wmata_avl` dataset we saw above:

``` r
summary(wmata_gtfs)
#> tidygtfs object
#> files        agency, routes, stop_times, trips, shapes, calendar, calendar_dates, stops
#> agency       WMATA
#> service      from 2025-12-14 to 2026-06-13
#> uses         stop_times (no frequencies)
#> # routes        3
#> # trips      4167
#> # stop_ids    347
#> # stop_names  253
#> # shapes       15
```

We recommend checking out
[`tidytransit`](https://r-transit.github.io/tidytransit/) for functions
to read, write, and manipulate GTFS feeds. `transittraj` offers a
handful of additional helper functions for working with GTFS, most
notably
[`filter_by_route()`](https://obrien-ben.github.io/transittraj/reference/filter_by_route.md)
and
[`get_shape_geometry()`](https://obrien-ben.github.io/transittraj/reference/get_shape_geometry.md).
Additionally, we can also create an interactive visualization of a GTFS
feed:

``` r
plot_interactive_gtfs(gtfs = wmata_gtfs,
                      color_palette = "gtfs")
```

Try clicking on routes or stops to see a pop-up with more information.
This interactive map is very useful for deciding which `shape_id` and
`direction_id` you want to work with.

## Conclusion

We now know what data sources `transittraj` requires. Each function in
the package will check whether the input data meets that function’s
requirements, and if it does not, an error will be thrown describing
what’s wrong. When in doubt, try using
[`validate_tides()`](https://obrien-ben.github.io/transittraj/reference/validate_tides.md)
or
[`tidytransit::validate_gtfs()`](https://r-transit.github.io/tidytransit/reference/validate_gtfs.html)
to check your data.

In the next vignette
([`vignette("data-workflow")`](https://obrien-ben.github.io/transittraj/articles/data-workflow.md)),
we’ll explore the AVL cleaning process.
