# Function to quickly validate whether an input shape_geometry meets needs

Checks:

- Class (should be SF, not SFC)

- Geometry type (i.e., is multilinestring)

- Presence of shape_id column, if desired

- Number of shapes present, if desired

- Correct CRS, if desired

## Usage

``` r
validate_shape_geometry(
  shape_geometry,
  max_length = Inf,
  require_shape_id = TRUE,
  match_crs = NULL
)
```

## Arguments

- shape_geometry:

  SF multilinestring

- max_length:

  numeric of max number of objects in SF

- require_shape_id:

  should shape ID be required in SF?

- match_crs:

  CRS that SF should have

## Value

Throws error if not satisfied

## Details

Not intended for external use.
