# WMATA Bus Automatic Vehicle Location Data

This dataset is an archive of WMATA's public GTFS-realtime feed,
accessed from their developer API. It has been reformatted from GTFS-rt
to match the TIDES standard for both fields and datatypes. This dataset
is intended to be used alongside the static GTFS feed provided in
`wmata_gtfs`.

## Usage

``` r
wmata_avl
```

## Format

### `wmata_avl`

A dataframe with 20,777 rows and 12 columns.

- location_ping_id:

  A unique ID for each row

- vehicle_id:

  An ID corresponding to each vehicle

- trip_id_performed:

  Trip IDs, matching those in GTFS

- service_date:

  The data of the trip's beginning

- route_id:

  Route IDs, matching those in GTFS

- direction_id:

  Direction IDs, matching those in GTFS

- latitude, longitude:

  The GPS ping longitude and latitude

- speed:

  The recorded speed, in meters per second

- trip_stop_sequence:

  The stop number the vehicle is approaching

- event_timestamp:

  POSIXct time objects

- stop_id:

  Stop IDs the vehicles are approaching, matching those in GTFS

## Source

<https://developer.wmata.com/>

## Details

The dataset contains three bus routes, with two directions for each:

- D40: Georgia Ave

- C53 U St/Congress Heights

- D96: Mass Ave to Bethesda
