# Uses `validate_tides` and an input vector of needed fields to check whether an AVL DF meets the requirements of a given function.

Intended for internal use only.

## Usage

``` r
validate_input_to_tides(needed_fields, avl_df)
```

## Arguments

- needed_fields:

  Vector of fields desired

- avl_df:

  DF of TIDES AVL data

## Value

boolean for each field
