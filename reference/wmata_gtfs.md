# WMATA Bus GTFS

This dataset is a portion of WMATA's bus GTFS, first published on Dec
14, 2025 and valid through June 13, 2026, accessed through TransitLand.
The dataset is intended to be used alongside the archived GTFS-realtime
feed provided in `wmata_avl`.

## Usage

``` r
wmata_gtfs
```

## Format

### `wmata_gtfs`

A tidytransit object (list) with 8 files.

- agency:

  The GTFS `agency.txt` file

- routes:

  The GTFS `routes.txt` file

- trips:

  The GTFS `trips.txt` file

- stop_times:

  The GTFS `stop_times.txt` file

- stops:

  The GTFS `stops.txt` file

- shapes:

  The GTFS `shapes.txt` file

- calendar:

  The GTFS `calendar.txt` file

- calendar_dates:

  The GTFS `calendar_dates.txt` file

## Source

<https://www.transit.land/feeds/f-dqc-wmata~bus>

## Details

This dataset has been filtered to three routes, with two directions for
each:

- D40: Georgia Ave

- C53 U St/Congress Heights

- D96: Mass Ave toBethesda
