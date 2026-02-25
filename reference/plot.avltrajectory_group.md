# \#' Quickly plots an AVL trajectory.

This function generates a quick plot of a single or grouped trajectory
object. Using the trajectory function, the entire trajectory will be
plotted at a temporal resolution of 10 seconds. For grouped
trajectories, a maximum of 50 trips will be plotted. For more control
over plotting and formatting, see
[`plot_trajectory()`](https://obrien-ben.github.io/transittraj/reference/plot_trajectory.md).

## Usage

``` r
# S3 method for class 'avltrajectory_group'
plot(x, ...)

# S3 method for class 'avltrajectory_single'
plot(x, ...)
```

## Arguments

- x:

  A trajectory object.

- ...:

  Other parameters (not used).

## Value

A ggplot2 object.
