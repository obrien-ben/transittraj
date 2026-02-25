# Summary function for AVL trajectories.

This function prints a summary for grouped or single trajectory object
If the input is a single trajectory, the trip's ID and distance & time
range will be printed. If the input is a grouped trajectory, the number
of trips and the distance & time range across all trips will be printed.
For both, the interpolating curve methods will be printed.

## Usage

``` r
# S3 method for class 'avltrajectory_group'
summary(object, ...)

# S3 method for class 'avltrajectory_single'
summary(object, ...)
```

## Arguments

- object:

  A single or grouped trajectory object.

- ...:

  Other parameters (not used).

## Value

A summary character string.
