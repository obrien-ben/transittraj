#' Function to quickly validate whether input GTFS has required tables and
#' fields within those tables.
#'
#' Not intended for external use.
validate_gtfs_input <- function(gtfs, table, needed_fields) {

  # Pull validation table
  gtfs_val <- attr(gtfs, "validation_result")

  # Check table presence
  table_present <- all(gtfs_val %>%
                         dplyr::filter(file == table) %>%
                         dplyr::pull(file_provided_status))
  if (!table_present) {
    rlang::abort(message = paste("Table ", table, " missing from input GTFS",
                                 sep = ""),
                 class = "error_gtfsval_missing_table")
  }

  # Check field presence
  fields_present <- gtfs_val %>%
    dplyr::filter(file == table) %>%
    dplyr::filter(field %in% needed_fields) %>%
    dplyr::select(field, field_provided_status) %>%
    dplyr::arrange(match(field, needed_fields))

  if (!all(fields_present$field_provided_status)) {
    missing_fields <- fields_present %>%
      dplyr::filter(!field_provided_status) %>%
      dplyr::pull(field)
    rlang::abort(message = paste(c("The following fields are missing from",
                                   table, ":", missing_fields),
                                 collapse = " "),
                 class = "error_gtfsval_missing_fields")
  }
}

#' Function to quickly validate whether an input shape_geometry meets needs
#'
#' Checks:
#' - Class (should be SF, not SFC)
#' - Geometry type (i.e., is multilinestring)
#' - Presence of shape_id column, if desired
#' - Number of shapes present, if desired
#' - Correct CRS, if desired
#'
#' Not intended for external use.
validate_shape_geometry <- function(shape_geometry,
                                    max_length = Inf,
                                    require_shape_id = TRUE,
                                    match_crs = NULL) {

  # Check class
  if (!("sf" %in% shape_geometry)) {
    if ("sfc" %in% shape_geometry) {
      rlang::abort(message = "SFC provided for shape_geometry. Please input SF object.",
                   class = "error_geomval_datatype")
    } else {
      rlang::abort(message = "Unknown geometry datatype. Please input SF object.",
                   class = "error_geomval_datatype")
    }
  }

  # Check multilinestring
  if (!all(sf::st_is(shape_geometry, "MULTILINESTRING"))) {
    rlang::abort(message = "shape_geometry is not a MULTILINESTRING. Please input SF multilinestring object.",
                 class = "error_geomval_geomtype")
  }

  # Check if shape_id is present
  if (require_shape_id & !("shape_id" %in% names(shape_geometry))) {
    rlang::abort(message = "shape_id field not found in provided shape_geometry.",
                 class = "error_geomval_id")
  }

  # Check length of SF (i.e., num of shapes)
  if (dim(shape_geometry)[1] > max_length) {
    rlang::abort(message = paste("Too many shape_ids in shape_geometry. Please input only ",
                                 max_length, " shapes.",
                                 sep = ""),
                 class = "error_geomval_length")
  }

  # Check CRS, if one is provided
  if (!is.null(match_crs)) {
    if (sf::st_crs(shape_geometry)$epsg != match_crs) {
      rlang::abort(message = paste("shape_geometry not in correct projection. Please transform to EPSG ",
                                   match_crs, ".",
                                   sep = ""),
                   class = "error_geomval_crs")
    }
  }
}
