
<!-- README.md is generated from README.Rmd. Please edit that file -->

# transittraj

<!-- badges: start -->

<!-- badges: end -->

An R package for reconstructing and visualizing transit vehicle
trajectories.

This is an early development version of `transittraj`. Expect it to
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

<div class="figure" style="text-align: center">

<img src="man/figures/README-example2.png" alt="`transittraj` turns noisy GPS data (left) into a trajectory (right) meeting the four requirements discussed below" width="100%" />
<p class="caption">

`transittraj` turns noisy GPS data (left) into a trajectory (right)
meeting the four requirements discussed below
</p>

</div>

## Installation

You can install the development version of `transittraj` from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("UTEL-UIUC/transittraj")
```

## Statement of Need

The primary goal of `transittraj` is to reconstruct *trajectories* of
transit vehicles from AVL data. A trajectory is a function which
describes the one-dimensional position (i.e., the distance from a trip’s
beginning) of a transit vehicle over time. Vehicle trajectories are
incredibly powerful and versatile tools, and are widely used by traffic
engineers and operations researchers for planning and system performance
studies.

Despite this, transit professionals rarely have the opportunity to see
detailed trajectories of their vehicles, and instead typically only
receive finalized metrics – such as dwell time and segment-level travel
times – from analytics platforms. If a practitioner wanted to apply
trajectory fitting tools from parallel fields, they would likely
encounter challenges: transit data adheres to unique formatting
standards; very few open-source tools exist for handling transit vehicle
location data; and, most importantly, common GPS processing techniques
can oversmooth transit trajectories, as AVL pings occur with the same
frequency (15-30 seconds) as stop dwells, signal delays, and other
stop-and-go cyles.

`transittraj` fills this gap this gap by proposing a workflow with two
main steps. The first is data cleaning, where we focus on correcting
noise and errors in point observations. Second, we use cleaned position
and speed measurements to fit an interpolating curve representing the
vehicle’s trajectory. This curve has four important attributes:

- *Continuous*: There should be no gaps in the trajectory for each trip.

- *Monotonic*: The trajectory should be strictly increasing – transit
  vehicles typically don’t back up in service.

- *Invertible*: The trajectory should provide position as a function of
  time, or time as a function of position.

- *Differentiable*: At any point on the curve, we should be able to
  interpolate for the speed of the vehicle.

`transittraj` aims to make these workflows as smooth and accessible as
possible. We begin with data that adheres to industry standard data
formats, including [GTFS](https://gtfs.org/) and
[TIDES](https://tides-transit.org/main/). In our cleaning and trajectory
fitting, our functions are flexible but avoid techniques which require
complex tuning. Finally, we provide tools to visualize and apply fit
trajectory curves.

<div class="figure" style="text-align: center">

<img src="man/figures/README-arch.png" alt="Overview of `transittraj` workflow" width="90%" />
<p class="caption">

Overview of `transittraj` workflow
</p>

</div>

Check out the vignettes below to get started.

## Getting Started with `transittraj`

Check out the following vignettes to learn more about how to use
`transittraj`:

- [Understanding Data
  Inputs](https://utel-uiuc.github.io/transittraj/articles/input-data.html)

- [The AVL Cleaning
  Workflow](https://utel-uiuc.github.io/transittraj/articles/data-workflow.html)

- [Using
  Trajectories](https://utel-uiuc.github.io/transittraj/articles/intro-trajectories.html)

## Works in Progress

This package is still in early development. In prepartion for an
eventual submission to CRAN, we’re still working on the following:

- Examples in all function documentation

- Sample datasets to include with `transittraj`

- Formal automated testing

- Vignettes discussing methodology and inner-workings of `transittraj`

## Citation

`transittraj` is free and open source, but if you find the package
helpful, we’d appreciate a citation:

``` r
citation("transittraj")
#> To cite package 'transittraj' in publications use:
#> 
#>   O'Brien B, Lehe L (2026). _transittraj: Reconstruct and Visualize
#>   Transit Vehicle Trajectories_. R package version 0.0.0.9000, commit
#>   ce1262e5ffde138ec4e1f3708416cda4ab3810bc,
#>   <https://github.com/UTEL-UIUC/transittraj>.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {transittraj: Reconstruct and Visualize Transit Vehicle Trajectories},
#>     author = {Benjamin O'Brien and Lewis Lehe},
#>     year = {2026},
#>     note = {R package version 0.0.0.9000, commit ce1262e5ffde138ec4e1f3708416cda4ab3810bc},
#>     url = {https://github.com/UTEL-UIUC/transittraj},
#>   }
```
