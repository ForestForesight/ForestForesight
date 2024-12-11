#' Check SpatVector Properties
#'
#' This function checks if a given object is a valid SpatVector polygon with
#' appropriate coordinate system and size constraints, and validates its overlap
#' with known country boundaries.
#'
#' @param shape Object of class SpatVector to check.
#' @param check_size Logical. Whether to check if the area is within reasonable bounds.
#' @param add_overlap Logical. Whether the overlap with landmass in percentage should be added as an attribute.
#' Default is TRUE.
#'
#' @return The input SpatVector, possibly reprojected to EPSG:4326.
#'
#' @details
#' The function checks:
#' - If input is a SpatVector polygon
#' - If coordinate system is properly defined
#' - If polygons are valid and non-empty
#' - If the area overlaps with known country boundaries
#' - Optionally checks if area is within reasonable bounds
#' (larger than 10,000 ha but smaller than approximately Brazil)
#'
#' @import terra
#'
#' @references
#' Zillah Calle (2023)
#' Jonas van Duijvenbode (2023)
#'
#' @examples
#' \dontrun{
#' shape <- vect("path/to/shapefile.shp")
#' validated_shape <- check_spatvector(shape)
#' }
#'
#' @export
check_spatvector <- function(shape, check_size = TRUE, add_overlap = FALSE) {
  check_basic_properties(shape)
  shape <- check_coordinate_system(shape)
  overlap <- check_country_overlap(shape)
  if (add_overlap) {
    shape$overlap <- overlap
  }
  if (check_size) {
    check_area_bounds(shape)
  }
  return(shape)
}

#' Check basic SpatVector properties
#' @noRd
check_basic_properties <- function(x) {
  if (!inherits(x, "SpatVector")) {
    stop(paste0("Input must be a SpatVector object, received ", class(x)))
  }
  if (terra::geomtype(x) != "polygons") {
    stop(paste("Input must be a polygon SpatVector, received", terra::geomtype(x)))
  }
  if (terra::nrow(x) == 0) {
    stop("Input SpatVector contains no polygons")
  }
}

#' Check and fix coordinate system
#' @noRd
check_coordinate_system <- function(x) {
  if (is.na(terra::crs(x))) {
    x <- handle_missing_crs(x)
  } else if (!is_wgs84(terra::crs(x))) {
    ff_cat("Coordinate system is not EPSG:4326. Converting to WGS84.", color = "yellow", log_level = "WARNING")
    x <- terra::project(x, "EPSG:4326")
  }
  return(x)
}

#' Handle missing CRS
#' @noRd
handle_missing_crs <- function(x) {
  if (is_within_degree_range(x)) {
    ff_cat("No coordinate system defined but coordinates appear to be in decimal degrees.
            Assuming WGS84.", color = "yellow", log_level = "WARNING")
    terra::crs(x) <- "EPSG:4326"
    return(x)
  }
  stop("No coordinate system defined and coordinates are not in decimal degrees range")
}

#' Check if coordinates are within degree range
#' @noRd
is_within_degree_range <- function(x) {
  terra::ext(x)$xmin >= -180 && terra::ext(x)$xmax <= 180
}

#' Check if CRS is WGS84
#' @noRd
is_wgs84 <- function(crs) {
  identical(crs, "EPSG:4326") || length(grep("WGS 84", crs)) > 0
}

#' Check overlap with country boundaries
#' @noRd
check_country_overlap <- function(x) {
  countries <- terra::aggregate(terra::vect(get(data("countries", envir = environment()))))
  intersection <- terra::intersect(x, countries)

  if (terra::nrow(intersection) == 0) {
    stop("Input shape does not overlap with any known country boundaries")
  }

  overlap <- check_overlap_percentage(x, intersection)
  return(overlap)
}

#' Check overlap percentage
#' @noRd
check_overlap_percentage <- function(x, intersection) {
  overlap_area <- sum(terra::expanse(intersection))
  shape_area <- sum(terra::expanse(x))
  overlap_percentage <- (overlap_area / shape_area) * 100

  if (overlap_percentage < 99.9) {
    ff_cat(sprintf(
      "Input shape only partially overlaps with known country boundaries (%.1f%% overlap)",
      overlap_percentage
    ), color = "yellow", log_level = "WARNING")
  }
  return(overlap_percentage)
}

#' Check area bounds
#' @noRd
check_area_bounds <- function(x) {
  area_ha <- sum(terra::expanse(x) / 10000)

  if (area_ha <= 0) {
    stop("Input SpatVector has zero or negative area")
  }

  upper_area_threshold <- 1e9
  lower_area_threshold <- 1e4

  if (area_ha > upper_area_threshold) {
    ff_cat("Input area is very large.
           This may impact performance and processing time.", color = "yellow", log_level = "WARNING")
  }
  if (area_ha < lower_area_threshold) {
    ff_cat("Input area is smaller than 10,000 hectares.
            This may impact performance for training purposes.", color = "yellow", log_level = "WARNING")
  }
}
