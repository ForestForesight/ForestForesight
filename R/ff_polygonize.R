#' Polygonize raster to risk areas
#'
#' @description
#' Converts a raster file into polygons representing different risk levels for forest disturbance.
#' The function processes the input raster by applying thresholds, smoothing, and creating simplified
#' polygons based on various parameters. The number of output polygons can be automatically scaled
#' based on the area size or manually specified.
#'
#' @param input_raster Character or SpatRaster. Path to the input raster file or a SpatRaster object
#' @param output_file Character. File path to save shapefiles.
#'   Default is NA and means the polygons are returned and not written.
#' @param minimum_pixel_count Numeric. Base pixel count to filter on. Default is 5
#' @param threshold Numeric or Character. Threshold to apply to the raster. Default is 0.5.
#'   Valid options are:
#'   * A numeric value between 0 and 1
#'   * "medium" - Automatically chosen threshold for medium risk
#'   * "high" - Automatically chosen threshold for high risk
#'   * "very high" - Automatically chosen threshold for very high risk
#' @param window_size Numeric. Window size for focal calculation. Default is 7
#' @param smoothness Numeric. Smoothness parameter for ksmooth method. Default is 2
#' @param verbose Logical. Whether to print progress messages. Default is FALSE
#' @param calculate_max_count Logical. If TRUE, automatically scales the number of output polygons
#'   based on area size. Uses a power-law scaling that produces approximately:
#'   * 2000 polygons for Brazil-sized areas (~850M ha)
#'   * 1200 polygons for Indonesia-sized areas (~180M ha)
#'   * 500 polygons for Colombia-sized areas (~110M ha)
#'   * 150 polygons for Cambodia-sized areas (~18M ha)
#'   * 50-100 polygons for small areas (<5M ha)
#' @param max_polygons Numeric. Optional manual override for maximum number of polygons.
#'   Cannot be used together with calculate_max_count=TRUE.
#' @param contain_polygons SpatVector. Optional polygon boundary to restrict output.
#'   When provided, only creates polygons within this boundary.
#'
#' @return A list containing:
#'   * polygons: A SpatVector object containing the processed and simplified polygons,
#'     with attributes for risk level and area statistics. NULL if no polygons were generated.
#'   * max_count: The maximum polygon count used for filtering (only when calculate_max_count=TRUE
#'     or max_polygons is specified)
#'
#' @details
#' The function implements a multi-step process:
#' 1. Validates inputs and loads the raster
#' 2. Processes the raster using the specified threshold and window size
#' 3. Generates polygons with smoothing
#' 4. Applies maximum count filtering if requested
#' 5. Adds attributes including risk levels and area statistics
#'
#' The number of output polygons can be controlled in two ways:
#' * Automatic scaling using calculate_max_count=TRUE
#' * Manual specification using max_polygons parameter
#'
#' These options are mutually exclusive and will raise an error if both are provided.
#'
#' @section Warning:
#' When using character-based thresholds ("medium", "high", "very high"), the actual
#' numeric threshold is automatically determined based on the raster values distribution.
#'
#' @examples
#' \dontrun{
#' # Basic usage with default parameters
#' result <- ff_polygonize("input.tif")
#'
#' # Save high-risk areas with automatic polygon count scaling
#' result <- ff_polygonize("input.tif",
#'   output_file = "high_risk.shp",
#'   threshold = "high",
#'   calculate_max_count = TRUE
#' )
#'
#' # Specify exact number of polygons
#' result <- ff_polygonize("input.tif",
#'   threshold = 0.7,
#'   max_polygons = 500
#' )
#'
#' # Use with boundary constraint
#' boundary <- vect("country_boundary.shp")
#' result <- ff_polygonize("input.tif",
#'   calculate_max_count = TRUE,
#'   contain_polygons = boundary
#' )
#' }
#'
#' @seealso
#' [smoothr::ksmooth()] for details on the smoothing algorithm
#' [terra::focal()] for details on focal operations
#'
#' @import terra
#' @import smoothr
#'
#' @export
ff_polygonize <- function(input_raster,
                          output_file = NA,
                          minimum_pixel_count = 5,
                          threshold = 0.5,
                          window_size = 7,
                          smoothness = 2,
                          verbose = FALSE,
                          calculate_max_count = FALSE,
                          max_polygons = NULL,
                          contain_polygons = NA) {
  # Validate inputs and load raster
  if (calculate_max_count && has_value(max_polygons)) {
    stop("Either let the algorithm calculate the maximum amount of polygons or give it yourself")
  }
  input_raster <- load_and_validate_raster(input_raster)
  validate_threshold(threshold)

  # Process raster and determine threshold
  processed_raster <- process_raster(input_raster, threshold, window_size, verbose)
  if (is.null(processed_raster)) {
    return(NULL)
  }

  # Generate polygons
  generation_result <- generate_polygons(
    processed_raster$smoothed_raster,
    minimum_pixel_count,
    input_raster,
    smoothness
  )
  polygons <- generation_result$focus_polygons
  minimum_pixel_area <- generation_result$minimum_pixel_area

  # Apply maximum count filtering if requested

  polygons_and_count <- apply_max_count_filter(
    polygons,
    input_raster,
    contain_polygons,
    threshold,
    minimum_pixel_area,
    calculate_max_count,
    max_polygons,
    verbose
  )
  polygons <- polygons_and_count$polygons

  # Check if any polygons were generated
  if (length(polygons) == 0) {
    ff_cat("Based on the chosen threshold no polygons were generated.
           Lower the threshold to get polygons for this area",
           color = "yellow", log_level = "WARNING"
    )
    return(NULL)
  }

  # Add attributes and write output
  polygons <- add_polygon_attributes(
    polygons,
    input_raster,
    processed_raster$final_threshold
  )

  if (!is.na(output_file)) {
    ff_cat("writing", length(polygons), "polygons to", output_file,
           verbose = verbose
    )
    terra::writeVector(x = polygons, filename = output_file, overwrite = TRUE)
  }

  return(list(polygons = polygons, max_count = polygons_and_count$max_count))
}

#' Load and validate input raster
#' @noRd
load_and_validate_raster <- function(input_raster) {
  if (inherits(input_raster, "character")) {
    if (!file.exists(input_raster)) {
      stop("path to input raster does not exist")
    }
    input_raster <- terra::rast(input_raster)
  }
  return(input_raster)
}

#' Validate threshold parameter
#' @noRd
validate_threshold <- function(threshold) {
  valid_chars <- c("medium", "high", "very high")
  if (!any(
    inherits(threshold, "numeric"),
    inherits(threshold, "character") && threshold %in% valid_chars
  )) {
    stop("threshold must be numeric or one of: medium, high, very high")
  }
}

#' Process raster and determine threshold
#' @noRd
process_raster <- function(input_raster, threshold, window_size, verbose) {
  # Calculate threshold values
  raster_stats <- calculate_raster_stats(input_raster)

  # Check for valid data when using automatic thresholds
  if (inherits(threshold, "character") && raster_stats$raster_average == 0) {
    ff_cat("no values in this raster above 0.5 were found,
           which is the minimum threshold of predictions FF provides when using auto-thresholding.
           Use a value as threshold if you still want polygons",
           color = "yellow", log_level = "WARNING"
    )
    return(NULL)
  }

  # Apply focal mean
  smoothed_raster <- terra::focal(input_raster,
                                  w = window_size,
                                  fun = "mean",
                                  na.policy = "omit",
                                  na.rm = TRUE
  )

  # Determine final threshold
  final_threshold <- determine_threshold(
    threshold,
    smoothed_raster,
    raster_stats,
    verbose
  )

  smoothed_raster <- smoothed_raster > final_threshold

  return(list(
    smoothed_raster = smoothed_raster,
    final_threshold = final_threshold
  ))
}

#' Calculate raster statistics
#' @noRd
calculate_raster_stats <- function(input_raster) {
  default_treshold <- get_variable("DEFAULT_THRESHOLD")
  list(
    raster_average = as.numeric(terra::global(input_raster < default_treshold,
                                              fun = "mean",
                                              na.rm = TRUE
    )),
    high_threshold = as.numeric(terra::global(
      input_raster <
        quantile(input_raster[input_raster > default_treshold], 0.5),
      fun = "mean",
      na.rm = TRUE
    )),
    highest_threshold = as.numeric(terra::global(
      input_raster <
        quantile(input_raster[input_raster > default_treshold], 0.75),
      fun = "mean",
      na.rm = TRUE
    ))
  )
}

#' Determine threshold value based on input
#' @noRd
determine_threshold <- function(threshold, smoothed_raster, raster_stats, verbose) {
  if (!is.character(threshold)) {
    return(threshold)
  }

  final_threshold <- switch(threshold,
                            "medium" = quantile(as.matrix(smoothed_raster),
                                                probs = raster_stats$raster_average,
                                                na.rm = TRUE
                            ),
                            "high" = quantile(as.matrix(smoothed_raster),
                                              probs = raster_stats$high_threshold,
                                              na.rm = TRUE
                            ),
                            "very high" = quantile(as.matrix(smoothed_raster),
                                                   probs = raster_stats$highest_threshold,
                                                   na.rm = TRUE
                            )
  )

  ff_cat("automatically determined threshold is",
         round(final_threshold, 4),
         verbose = verbose
  )

  return(final_threshold)
}

#' Generate polygons from raster
#' @noRd
generate_polygons <- function(smoothed_raster,
                              minimum_pixel_count,
                              input_raster,
                              smoothness) {
  conversion_factor <- if (terra::is.lonlat(input_raster)) {110000} else {1}
  minimum_pixel_area <- minimum_pixel_count * (terra::res(input_raster)[1] * conversion_factor)^2

  clumped_raster <- terra::patches(smoothed_raster,
                                   directions = 8,
                                   zeroAsNA = TRUE
  )

  focus_polygons <- terra::as.polygons(clumped_raster,
                                       values = FALSE,
                                       aggregate = TRUE,
                                       round = FALSE
  )

  suppressWarnings({
    focus_polygons <- smoothr::fill_holes(focus_polygons,
                                          threshold = 5 * minimum_pixel_area
    )
    focus_polygons <- smoothr::smooth(focus_polygons,
                                      method = "ksmooth",
                                      smoothness = smoothness
    )
  })

  focus_polygons <- terra::disagg(focus_polygons)
  focus_polygons <- focus_polygons[order(terra::expanse(focus_polygons),
                                         decreasing = TRUE
  )]
  return(list(focus_polygons = focus_polygons, minimum_pixel_area = minimum_pixel_area))
}

#' Apply maximum count filter to polygons with country-based scaling
#'
#' @param polygons SpatVector of polygons to filter
#' @param input_raster SpatRaster used for area calculation
#' @param contain_polygons Optional SpatVector to intersect with polygons
#' @param threshold Character string indicating risk level
#' @param minimum_pixel_area Numeric minimum area for polygons
#' @param verbose Logical for logging output
#'
#' @return Filtered SpatVector with scaled number of polygons
#' @noRd
apply_max_count_filter <- function(polygons,
                                   input_raster,
                                   contain_polygons,
                                   threshold,
                                   minimum_pixel_area,
                                   calculate_max_count,
                                   max_polygons,
                                   verbose) {
  # Apply containment filter if provided
  if (has_value(contain_polygons)) {
    polygons <- polygons[contain_polygons, ]
  }

  # First filter by minimum area - do this regardless of count method

  valid_polygons <- terra::expanse(polygons) >= minimum_pixel_area

  polygons <- polygons[valid_polygons]

  # If no valid polygons remain after area filtering, return early
  if (length(polygons) == 0) {
    return(list(polygons = polygons, max_count = 0))
  }

  # Calculate max_count based on method
  if (calculate_max_count) {
    # Calculate raster coverage and area
    percentage_covered <- as.numeric(terra::global(!is.na(input_raster), "mean"))
    raster_area <- as.numeric(terra::expanse(input_raster)[2])
    area_ha <- raster_area / 1e4  # Convert to hectares

    # Calculate base polygon count using power function
    base_count <- 20 + (1980 * (area_ha / 8.5e8)^0.4)
    max_count <- ceiling(base_count)

    # Apply coverage percentage scaling
    max_count <- ceiling(max_count * percentage_covered)

    if (verbose) {
      ff_cat(
        "based on area of raster (hectares:",
        round(area_ha),
        ", actual coverage:",
        round(percentage_covered * 100),
        "percent), at maximum",
        max_count,
        " polygons are generated"
      )
    }
  } else {
    if (has_value(max_polygons)) {
      max_count <- max_polygons
    } else {
      max_count <- length(polygons)
    }
  }

  # Ensure we don't try to return more polygons than we have
  max_count <- min(max_count, length(polygons))

  # Order polygons by area before taking the final subset
  areas <- terra::expanse(polygons)
  polygons <- polygons[order(areas, decreasing = TRUE)]

  # Take the top max_count polygons
  final_polygons <- polygons[1:max_count]

  return(list(polygons = final_polygons, max_count = max_count))
}

#' Add attributes to polygons
#' @noRd
add_polygon_attributes <- function(polygons, input_raster, threshold) {
  polygons$risk <- round(terra::extract(
    input_raster, polygons,
    fun = "mean", ID = FALSE
  ), 2)
  conversion_factor <- if (terra::is.lonlat(polygons)) {110000} else {1}
  polygons$size <- round(terra::expanse(polygons) / 10000)*conversion_factor^2
  polygons$riskfactor <- round(polygons$size * polygons$risk)
  polygons$threshold <- threshold
  polygons$date <- as.character(as.Date(Sys.time()))
  return(polygons)
}
