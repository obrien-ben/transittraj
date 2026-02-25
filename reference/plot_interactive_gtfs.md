# Generates a Leaflet viewer of GTFS routes and stops.

This function generates a simple Leaflet-based interactive map viewer of
a GTFS. This function is intended for quick and easy visualization of a
GTFS feed. As such, formatting options are relatively limited.

## Usage

``` r
plot_interactive_gtfs(
  gtfs,
  background = "Esri.WorldGrayCanvas",
  color_palette = "Dark2"
)
```

## Arguments

- gtfs:

  A tidytransit GTFS object.

- background:

  Optional. A string for the background of the transit map, from
  Leaflet's provider library (see `leaflet::providers$`). Default is
  Esri's light gray canvas (`"Esri.WorldGrayCanvas"`).

- color_palette:

  Optional. A string for the Leaflet color palette to color routes. If
  `"gtfs"`, will use color codes in the GTFS `routes` file. Default is
  `"Dark2"`.

## Value

A Leaftlet object.

## Details

### Route Shapes and Stops

The primary goal of this function is to visualize and explore each GTFS
shape, including its associated `route_id` and `direction_id`. This
function will plot all shapes and stops present in the input `gtfs`. To
plot only a specific route or direction, first the feed using
[`filter_by_route()`](https://obrien-ben.github.io/transittraj/reference/filter_by_route.md).

Routes have both pop-ups and hover labels. The hover label shows the
shapes's `route_id` (from the `trips` file). The pop-up will show the
`route_id`, `direction_id`, and `shape_id`.

Stops also have both pop-ups and hover labels. The hover label will show
the point's `stop_id` (from the `stops` file). The pop-up will show the
`stop_name` and `stop_id`.

### Formatting

Two formatting options are available through this function: basemaps and
route color palettes.

The `background` parameter allows you to customize the background map
below the plotted shapes and stops. Esri's light grey canvas is the
default, as it is excellent for providing geographic context while still
allowing the routes to stand out. To see the available options, type
`leaflet::providers$` into your console.

The route colors can be customized in two different ways:

- Using the `gtfs`'s colors. Typically, the `routes` file in a GTFS feed
  will contain a field `route_color`; this is the color you see in most
  public-facing mapping/navigation applications (e.g., Google Maps,
  Transit, etc.). If this is present in the input `gtfs` feed, setting
  `color_palette = "gtfs"` will use this field to color each shape.

- Using a named color palette. Without `gtfs` colors, this function
  assigns colors categorically (using
  [`leaflet::colorFactor()`](https://rstudio.github.io/leaflet/reference/colorNumeric.html)).
  To set the palette, input a string corresponding to a palette name
  from `RColorBrewer`, a palette name from `viridis`, a vector of color
  names (with the same length as the number of shapes), or some other
  color function. See Leaflet's [colors
  vignette](https://rstudio.github.io/leaflet/articles/colors.html) for
  more information.
