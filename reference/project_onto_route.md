# Projects points to linear distances along a route shape.

This function takes spatial points and projects them onto a route,
returning the linear distance from the beginning terminal of the route.

## Usage

``` r
project_onto_route(
  shape_geometry,
  points,
  original_crs = 4326,
  project_crs = 4326
)
```

## Arguments

- shape_geometry:

  The SF object to project onto. Must include the field `shape_id`. See
  [`get_shape_geometry()`](https://obrien-ben.github.io/transittraj/reference/get_shape_geometry.md).

- points:

  Can be either: a dataframe representing point coordinates, with fields
  `longitude` and `latitude`; or, an SF or SFC point object.

- original_crs:

  Optional. A numeric EPSG identifier. If a dataframe is provided for
  `points`, this will be used to define the coordinate system of the
  longitude / latitude values. Default is 4326 (WGS 84 ellipsoid).

- project_crs:

  Optional. A numeric EPSG identifer indicating the coordinate system to
  use for spatial calculations. Consider setting to a Euclidian
  projection, such as the appropriate UTM zone. Default is 4326 (WGS 84
  ellipsoid).

## Value

The `points` input (either dataframe or SF) with an appended column for
the linear distance along the route. If `points` is an SFC, a vector of
numeric distances is returned. Units are those of the spatial projection
used (e.g., meters if using UTM).
