# Function to quickly validate whether input GTFS has required tables and fields within those tables.

Not intended for external use.

## Usage

``` r
validate_gtfs_input(gtfs, table, needed_fields)
```

## Arguments

- gtfs:

  tidygtfs.

- table:

  The gtfs table to look for.

- needed_fields:

  fields in that table to look for.

## Value

Throws error if not satisfied
