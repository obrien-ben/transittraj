# Get the distances of stops along routes.

This function returns the linear distance of each stop along a route
shape, starting from the route's beginning terminal. Unless a
`shape_geometry` is provided, stops will be project onto all `shape_id`s
that serve them. If a `shape_geometry` is provided, the function will
look only for stops served by that shape.

## Usage

``` r
get_stop_distances(gtfs, shape_geometry = NULL, project_crs = 4326)
```

## Arguments

- gtfs:

  A tidygtfs object.

- shape_geometry:

  Optional. The SF object to project onto. Must include the field
  `shape_id`. See
  [`get_shape_geometry()`](https://obrien-ben.github.io/transittraj/reference/get_shape_geometry.md).
  Default is NULL, where all shapes in `gtfs` will be used.

- project_crs:

  Optional. A numeric EPSG identifer indicating the coordinate system to
  use for spatial calculations. Consider setting to a Euclidian
  projection, such as the appropriate UTM zone. Default is 4326 (WGS 84
  ellipsoid).

## Value

A dataframe containing `stop_id`, the `shape_id` it was projected onto,
and `distance`, in units of the spatial projection (e.g., meters if
using UTM).
