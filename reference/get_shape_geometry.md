# Get the geometry of a route shape.

This function returns an SF multilinestring of the route alignments from
GTFS shapes. Similar to tidytransit's `get_geometry()`, but allows
filtering by `shape_id` and projection to a new coordinate system. See
`Details` for requirements on the input GTFS.

## Usage

``` r
get_shape_geometry(gtfs, shape = NULL, project_crs = 4326)
```

## Arguments

- gtfs:

  A tidygtfs object.

- shape:

  Optional. The GTFS shape_id to use. Can be a single value, or a
  vector. Default is NULL, where all `shape_id`s in `gtfs` will be used.

- project_crs:

  Optional. A numeric EPSG identifer indicating the coordinate system to
  use for spatial calculations. Consider setting to a Euclidian
  projection, such as the appropriate UTM zone. Default is 4326 (WGS 84
  ellipsoid).

## Value

An SF multilinestring, with one multilinestring object per `shape_id`.

## Details

A `shapes` file must be present in your GTFS object. This file must
contain at least the following fields:

- `shape_id`

- `shape_pt_lat`

- `shape_pt_lon`

- `shape_pt_sequence`
