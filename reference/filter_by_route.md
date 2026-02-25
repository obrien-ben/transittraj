# Filter GTFS to a desired route(s) and direction(s).

This function returns a new `tidygtfs` object with only the information
relevant to your desired routes and directions. All fields included in
the input `gtfs` will be filtered. See `Details` for more information
about required files and fields

## Usage

``` r
filter_by_route(gtfs, route_ids, dir_id = NULL)
```

## Arguments

- gtfs:

  A tidygtfs object.

- route_ids:

  A numeric vector or single numeric containing the desired route ID(s).

- dir_id:

  Optional. A numeric vector or single numeric containing the desired
  direction ID(s).

## Value

A tidygtfs object containing only information relevant to the desired
route and direction.

## Details

The following files and fields are required for this function:

- `routes`: with `route_id` and `agency_id`

- `agency`: with `agency_id`

- `trips`: with `route_id`, `direction_id`, `shape_id`, `service_id`,
  and `trip_id`

- `stop_times`: with `stop_id` and `trip_id`

The following files are optional. If they are included, the must include
the listed fields:

- `stops`: with `stop_id`

- `shapes`: with `shape_id`

- `calendar`: with `service_id`

- `calendar_dates`: with `service_id`

- `transfers`: with `trip_id` and `stop_id`

- `frequencies`: with `trip_id`

- `fare_rules`: with `route_id`

- `feed_info`

For these optional files, the function will detect whether they are
present. If so, they will be filtered; if not, they will be left `NULL`
in the new GTFS. If any required file or field is missing, an error will
be thrown describing what is missing.
