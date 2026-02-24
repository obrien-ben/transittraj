## code to prepare `DATASET` dataset goes here

usethis::use_data(DATASET, overwrite = TRUE)

# Import raw data from this folder
wmata_gtfs_full <- tidytransit::read_gtfs(".\\data-raw\\wmata_gtfs.zip")
wmata_gtfs_rt <- read.csv(".\\data-raw\\wmata_gtfsrt_021626.csv")

# Filter GTFS to only desired routes
keep_routes <- unique(wmata_gtfs_rt$vehicle.trip.route_id)
wmata_gtfs <- filter_by_route(gtfs = wmata_gtfs_full,
                                   route_ids = keep_routes)

# Format GTFS-RT into TIDES AVL
wmata_avl <- wmata_gtfs_rt %>%
  # Rename to TIDES
  dplyr::rename(location_ping_id = X,
         trip_id_performed = vehicle.trip.trip_id,
         vehicle_id = id,
         service_date = vehicle.trip.start_date,
         route_id = vehicle.trip.route_id,
         direction_id = vehicle.trip.direction_id,
         latitude = vehicle.position.latitude,
         longitude = vehicle.position.longitude,
         speed = vehicle.position.speed,
         trip_stop_sequence = vehicle.current_stop_sequence,
         event_timestamp = vehicle.timestamp,
         stop_id = vehicle.stop_id) %>%
  # Remove columns we won't use
  dplyr::select(-c(vehicle.trip.start_time,
                   vehicle.trip.schedule_relationship,
                   vehicle.position.bearing,
                   vehicle.occupancy_status,
                   vehicle.current_status,
                   vehicle.vehicle.id,
                   vehicle.vehicle.label,
                   vehicle.occupancy_status)) %>%
  # Change datatype to TIDES
  dplyr::mutate(event_timestamp = as.POSIXct(event_timestamp,
                                             tz = "America/New_York"),
                service_date = as.Date(as.character(service_date), format = "%Y%m%d"),
                trip_id_performed = as.character(trip_id_performed),
                vehicle_id = as.character(vehicle_id),
                location_ping_id = as.character(location_ping_id))

# Verify TIDES compliance
wmata_tides <- validate_tides(avl_df = wmata_avl)
