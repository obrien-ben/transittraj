# Linearizes latitude-longitude GPS points to a provided route shape.

This functions projects raw AVL data, as GPS latitude-longitude points,
onto a provided route geometry, returning the distance of that point
along the shape from the beginning terminal.

## Usage

``` r
get_linear_distances(
  avl_df,
  shape_geometry,
  clip_buffer = NULL,
  original_crs = 4326,
  project_crs = 4326
)
```

## Arguments

- avl_df:

  A dataframe of raw AVL data. Must include at least `longitude` and
  `latitude` columns. See
  [`validate_tides()`](https://obrien-ben.github.io/transittraj/reference/validate_tides.md).

- shape_geometry:

  The SF object to project onto. Must be only one shape. See
  [`get_shape_geometry()`](https://obrien-ben.github.io/transittraj/reference/get_shape_geometry.md).

- clip_buffer:

  Optional. The distance, in units of the used spatial projection, to
  clip the GPS points. Only points within this distance of the
  `shape_geometry` will be kept. Default is NULL, where no clip will be
  applied.

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

The input `avl_df` with `latitude` and `longitude` columns replaced by a
`distance` column, in the units of the spatial projection used (e.g.,
meters if using UTM).
