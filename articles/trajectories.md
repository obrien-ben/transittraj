# trajectories

## Introduction

In the previous vignette, we saw how we can use `transittraj` to clean
our AVL data. We took care of outliers, deadheading trips, noise and
non-monotonic observations, and more. In this vignette, we’ll apply the
cleaned data (`c53_mono`) to fit a trajectory function.

Let’s begin by loading the library we’ll be using:

``` r
library(transittraj)
library(tidytransit)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(ggplot2)
```

## Fitting a Trajectory Curve

Our ultimate goal is to fit an interpolating curve describing the
position of a transit vehicle at any point in time. Ideally, we could
fit an inverse curve, giving us the time the transit vehicle passes any
point in space. We can do both using
[`get_trajectory_fun()`](https://obrien-ben.github.io/transittraj/reference/get_trajectory_fun.md).

`transittraj` supports many different methods for fitting these
functions. The simplest is linear interpolation without an inverse. If
this is all you need, you can probably skip many of the cleaning steps
from the previous vignette. For more fine-grained analyses, though, we
recommend fitting a *velocity-informed piecewise cubic interpolating
polynomial*. This uses the speeds and distances, correct for
monotonicity, to fit a cubic spline between each observation. By
default,
[`get_trajectory_fun()`](https://obrien-ben.github.io/transittraj/reference/get_trajectory_fun.md)
will fit this type of interpolating curve (`interp_method = "monoH.FC"`
and `use_speeds = TRUE`).

Using the data we cleaned in the previous vignette, let’s finally fit
our trajectory functions:

``` r
c53_traj <- get_trajectory_fun(distance_df = c53_mono,
                               interp_method = "monoH.FC",
                               use_speeds = TRUE,
                               find_inverse_fun = TRUE)
```

And that’s it! Once your data is cleaned, fitting the trajectory is
quick and easy; we’ve already done most of the heavy lifting.

## Exploring the Trajectory Object

`transittraj` stores the fit curves in a special object class. This
object stores a list of fit trajectories, one for each trip, as well as
the time and distances ranges for each trip. We can use
[`summary()`](https://rdrr.io/r/base/summary.html) to take a look inside
the object:

``` r
summary(c53_traj)
#> ------
#> AVL Group Trajectory Object
#> ------
#> Number of trips: 24
#> Total distance range: 0 to 15370.75
#> Total time range: 1771257490 to 1771275553
#> ------
#> Trajectory function present: TRUE
#>    --> Trajectory interpolation method: monoH.FC
#>    --> Maximum derivative: 3
#>    --> Fit with speeds: TRUE
#> Inverse function present: TRUE
#>    --> Inverse function tolerance: 0.01
#> ------
```

You’ll notice the summary also contains information about the parameters
we used to fit the curve.

## Interpolating

How do you use the fit curve to actually interpolate at new points? We
recommend using [`predict()`](https://rdrr.io/r/stats/predict.html), as
this will ensure that the curves aren’t used to extrapolate beyond the
range of each trip. Using
[`predict()`](https://rdrr.io/r/stats/predict.html), there are three
main values we can interpolate for: time from distances (requires an
inverse function), distances from times, and speeds from times (requires
a spline).

### Interpolating for Times

One of the most common applications of the fit trajectory curve is to
find the time at which each vehicle passed a point along its route. To
do this, we’ll use [`predict()`](https://rdrr.io/r/stats/predict.html)
with the `new_distances` parameter. We’ll begin by finding the distance
of each stop along the route using
[`get_stop_distances()`](https://obrien-ben.github.io/transittraj/reference/get_stop_distances.md):

``` r
# First, use stop_times to find which stop_ids are timepoints
c53_timepoints <- c53_gtfs$stop_times %>%
  distinct(stop_id, timepoint)
# Now, find stop distances and join the timepoints column
c53_stops <- get_stop_distances(gtfs = c53_gtfs,
                                shape_geometry = c53_shape,
                                project_crs = dc_CRS) %>%
  left_join(y = c53_timepoints, by = "stop_id") %>%
  select(-shape_id) %>%
  mutate(timepoint = if_else(condition = (timepoint == 1),
                             true = "Yes",
                             false = "No"))
head(c53_stops)
#> # A tibble: 6 × 3
#>   stop_id distance timepoint
#>   <chr>      <dbl> <chr>    
#> 1 2584        677. No       
#> 2 2609        880. No       
#> 3 2683       1155. No       
#> 4 2793       1605. No       
#> 5 2811       1807. No       
#> 6 2867       2037. No
```

Now that we have some distances, let’s interpolate using
[`predict()`](https://rdrr.io/r/stats/predict.html):

``` r
c53_stop_crossings <- predict(
  object = c53_traj,
  new_distances = c53_stops
)
head(c53_stop_crossings)
#> # A tibble: 6 × 5
#>   stop_id distance timepoint trip_id_performed      interp
#>   <chr>      <dbl> <chr>     <chr>                   <dbl>
#> 1 2584        677. No        10185100          1771274452.
#> 2 2584        677. No        10249100          1771272909.
#> 3 2584        677. No        1306100           1771262814.
#> 4 2584        677. No        13437100          1771259527.
#> 5 2584        677. No        13478100          1771275106.
#> 6 2584        677. No        1699100           1771265258.
```

And now we have the crossing time (labeled `interp`) at each stop, for
each trip! The interpolated times are in seconds of epoch time.

### Interpolating for Distances

Let’s say you want to know where every vehicle is at a certain point in
time. We can do that by providing `new_times` to
[`predict()`](https://rdrr.io/r/stats/predict.html). Let’s see below:

``` r
c53_time_interp <- predict(
  object = c53_traj,
  new_times = c(1771265000, 1771275000)
)
print(c53_time_interp)
#>   event_timestamp trip_id_performed     interp
#> 1      1771265000           1306100  8933.8531
#> 2      1771265000          18298100 10899.0966
#> 3      1771265000          21499100  4006.3031
#> 4      1771265000          21555100 13912.1101
#> 5      1771265000          22663100  6944.8919
#> 6      1771275000          10185100  2883.3087
#> 7      1771275000          10249100  7986.3226
#> 8      1771275000          13478100   140.0436
#> 9      1771275000           3597100  6792.4405
```

Here, `interp` will be the distance in meters from the route’s
beginning. You’ll notice that, even though we have 24 trips, there were
only four to five distance for each timepoint. This is because
[`predict()`](https://rdrr.io/r/stats/predict.html) will only
interpolate a distance for trips that were actually running at that
point in time.

### Interpolating for Speeds

The last thing we can interpolate for is the speed at any given point in
time. We can control this by setting the `deriv` parameter in
[`predict()`](https://rdrr.io/r/stats/predict.html):

``` r
c53_speed_interp <- predict(
  object = c53_traj,
  new_times = c(1771265000, 1771275000),
  deriv = 1
)
print(c53_speed_interp)
#>   event_timestamp trip_id_performed       interp
#> 1      1771265000           1306100 1.675073e-01
#> 2      1771265000          18298100 1.464849e+00
#> 3      1771265000          21499100 1.372977e+01
#> 4      1771265000          21555100 6.424041e-01
#> 5      1771265000          22663100 1.508816e+00
#> 6      1771275000          10185100 4.473051e-01
#> 7      1771275000          10249100 4.354498e-05
#> 8      1771275000          13478100 7.213864e-01
#> 9      1771275000           3597100 9.079446e-01
```

Here, `interp` will be the speed in meters per second. Finding speeds
requires starting from time values.

## Visualizing Trajectories

### Quick Plots

Now its time for the *really* fun part – plotting our trajectory curves.
We can use [`plot()`](https://rdrr.io/r/graphics/plot.default.html) to
generate a quick and easy plot of all trajectories:

``` r
plot(c53_traj) +
  # This part is optional -- we're doing it to make the
  # figure more readable for this vignette
  theme(text = element_text(size = 5))
```

![](trajectories_files/figure-html/unnamed-chunk-9-1.png)

[`plot()`](https://rdrr.io/r/graphics/plot.default.html) is intended for
quick visualizations of trajectories, and as such does not allow for
much customization. In the next section, we’ll use
[`plot_trajectory()`](https://obrien-ben.github.io/transittraj/reference/plot_trajectory.md)
to create more interesting plots.

### Detailed Trajectories

To add features (such as stops) and customize formatting, we recommend
using
[`plot_trajectory()`](https://obrien-ben.github.io/transittraj/reference/plot_trajectory.md).
This function starts of similar to
[`plot()`](https://rdrr.io/r/graphics/plot.default.html), taking a
trajectory object. After that, you can add a dataframe of feature
distances, such as the `c53_stops` we made ealier. Finally, formatting
can be controlled using mapping dataframes. The colors and linetypes of
both trajectories and features can be mapped to attributes using
something similar to what is below:

``` r
stop_formatting <- data.frame(timepoint = c("Yes", "No"),
                              color = c("firebrick", "grey50"),
                              linetype = c("longdash", "dashed"))
```

For mapping dataframes, at least one column must match the layer being
mapped to (trajectories or features). The other columns must be `color`
and/or `linetype`, telling `transittraj` which feature they describe.

We can plug all that in to
[`plot_trajectory()`](https://obrien-ben.github.io/transittraj/reference/plot_trajectory.md)
to generate our formatted plot:

``` r
traj_plot <- plot_trajectory(
  trajectory = c53_traj,
  feature_distances = c53_stops,
  feature_color = stop_formatting,
  feature_type = stop_formatting,
  feature_width = 0.2, feature_alpha = 0.5,
  traj_width = 0.4, traj_alpha = 1
)
traj_plot +
  # This part is optional -- we're doing it to make the
  # figure more readable for this vignette
  theme(text = element_text(size = 5))
```

![](trajectories_files/figure-html/unnamed-chunk-11-1.png)

### Line Animations

### Map Animations
