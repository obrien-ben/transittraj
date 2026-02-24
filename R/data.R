#' WMATA Bus Automatic Vehicle Location Data
#'
#' @description
#' This dataset is an archive of WMATA's public GTFS-realtime feed, accessed
#' from their developer API. It has been reformatted from GTFS-rt to match
#' the TIDES standard for both fields and datatypes. This dataset is intended
#' to be used alongside the static GTFS feed provided in `wmata_gtfs`.
#'
#' @details
#' The dataset contains three bus routes, with two directions for each:
#'
#' - D40: Georgia Ave
#' - C53 U St/Congress Heights
#' - D96: Mass Ave to Bethesda
#'
#'
#' @format ## `wmata_avl`
#' A dataframe with 20,777 rows and 12 columns.
#' \describe{
#'    \item{location_ping_id}{A unique ID for each row}
#'    \item{vehicle_id}{An ID corresponding to each vehicle}
#'    \item{trip_id_performed}{Trip IDs, matching those in GTFS}
#'    \item{service_date}{The data of the trip's beginning}
#'    \item{route_id}{Route IDs, matching those in GTFS}
#'    \item{direction_id}{Direction IDs, matching those in GTFS}
#'    \item{latitude, longitude}{The GPS ping longitude and latitude}
#'    \item{speed}{The recorded speed, in meters per second}
#'    \item{trip_stop_sequence}{The stop number the vehicle is approaching}
#'    \item{event_timestamp}{POSIXct time objects}
#'    \item{stop_id}{Stop IDs the vehicles are approaching, matching those in GTFS}
#' }
#' @source <https://developer.wmata.com/>
"wmata_avl"

#' WMATA Bus GTFS
#'
#' @description
#' This dataset is a portion of WMATA's bus GTFS, first published on Dec 14,
#' 2025 and valid through June 13, 2026, accessed through TransitLand. The
#' dataset is intended to be used alongside the archived GTFS-realtime
#' feed provided in `wmata_avl`.
#'
#' @details
#' This dataset has been filtered to three routes, with two directions for each:
#'
#' - D40: Georgia Ave
#' - C53 U St/Congress Heights
#' - D96: Mass Ave toBethesda
#'
#' @format ## `wmata_gtfs`
#' A tidytransit object (list) with 8 files.
#' \describe{
#'    \item{agency}{The GTFS `agency.txt` file}
#'    \item{routes}{The GTFS `routes.txt` file}
#'    \item{trips}{The GTFS `trips.txt` file}
#'    \item{stop_times}{The GTFS `stop_times.txt` file}
#'    \item{stops}{The GTFS `stops.txt` file}
#'    \item{shapes}{The GTFS `shapes.txt` file}
#'    \item{calendar}{The GTFS `calendar.txt` file}
#'    \item{calendar_dates}{The GTFS `calendar_dates.txt` file}}
#' @source <https://www.transit.land/feeds/f-dqc-wmata~bus>
"wmata_gtfs"
