# transittraj

An R package for reconstructing and visualizing transit vehicle
trajectories.

This is an early development version of `transitraj`. Expect it to
change without warning.

## Introduction

Today’s transit vehicles generate a large amount of automatic vehicle
location (AVL) data. This data is vital in planning and performance
studies, but turning sparse and noisy GPS pings into meaningful
performance metrics is difficult. `transittraj` fills this gap,
integrating with existing open data standards, including GTFS and TIDES,
to provide tools for cleaning AVL data and fitting continuous,
monotonic, invertible, and differentiable trajectory curves. By doing
so, `transittraj` provides versatile and powerful tools to analyze
transit system performance and support decision-making.

You can install the development version of transittraj from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("obrien-ben/transittraj")
```

## What and Why

The primary goal of `transittraj` is to reconstruct *trajectories* of
transit vehicles from AVL data. A trajectory is a function which
describes the one-dimensional position (i.e., the distance from a trip’s
beginning) of a transit vehicle over time. Vehicle trajectories are
incredibly powerful and versatile tools, and are widely used by traffic
engineers and operations researchers for planning and system performance
studies.

Despite this, there is a gap between transit practice and the field of
trajectory research. Transit practitioners and researchers rarely have
the opportunity to see detailed trajectories of their vehicles, and
instead typically only receive finalized metrics – such as dwell time
and segment-level travel times – from analytics platforms and AVL
providers. If practioners and researchers want a trajectory, they may
look to trajectory research in parallel fields. Transit data is unique,
though, making it difficult to transfer methods. We have both
opportunities and challenges:

- *Opportunities*: Transit vehicles follow a known path; we need not
  worry about lane changing or lateral position, and can measure a
  vehicle’s progress linearly along that path. Additionally, transit has
  standardized data formats, such as [GTFS](https://gtfs.org/) for
  schedule and route alignment, and
  [TIDES](https://tides-transit.org/main/) for vehicle locations.

- *Challenges*: Transit AVL data is sparse, typically pinging every
  15-30 seconds. This is a challenge because the stop-and-go cycles of
  transit vehicles – including stop dwells, signal delays, congestion
  slowdowns, and more – occur with the same frequency as these pings.
  This contrasts much of the research in traffic trajectory
  construction, which has near-continuous data points in free-flow, or
  at least uncongested, traffic conditions.

These challenges inform our general approach to trajectory
reconstruction: use all the data we have to fit an interpolation curve.
This contrasts the smoothing techniques common in other fields, which
may oversmooth the stops and slowdowns transit vehicles experience in
real urban traffic.

There are two main steps to the `transittraj` workflow. The first is AVL
cleaning, where we focus on correcting point observations, such as
infeasible jumps and outliers or backwards drifting during stopping
events. Then, we use position and speed measurements from the cleaned
AVL data to fit a trajectory curve. The trajectory has four important
attributes:

- *Continuous*: There should be no gaps in the trajectory for each trip.

- *Monotonic*: The trajectory should be strictly increasing – transit
  vehicles typically don’t back up in service.

- *Invertible*: If a curve satisfies the previous two points, it will
  also be invertible. This lets us retrieve time as a function of
  distance, incredibly useful if one wants to know when a vehicle passed
  a certain point on its route.

- *Differentiable*: At any point on the curve, we should be able to
  interpolate for the speed of the vehicle. This allows us to generate a
  continuous speed profile of each vehicle’s trip.

![A \`transittraj\` trajectory meeting our four
requirements](reference/figures/README-example_traj.png)

A `transittraj` trajectory meeting our four requirements

`transittraj` aims to make these workflows as smooth and accessible as
possible. We begin with data that adheres to industry standard data
formats, including GTFS and TIDES. In our cleaning and trajectory
fitting, we provide versatile and flexible functions, but avoid
techniques which require complex tuning. Finally, we provide a handful
of helper functions to make it easy to use and visualize your fit
trajectory curves. This website hosts the package’s full documentation,
and includes examples of complete `transittraj` workflows. Check out
some of the vignettes below to get started.

## Getting Started with `transittraj`

Check out the following vignettes to learn more about how to use
`transittraj`:

- [Overview of
  Data](https://obrien-ben.github.io/transittraj/articles/input-data.html)

- [AVL
  Cleaning](https://obrien-ben.github.io/transittraj/articles/data-workflow.html)

- [Using
  Trajectories](https://obrien-ben.github.io/transittraj/articles/intro-trajectories.html)
