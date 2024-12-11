#' Polygonize raster to risk areas
#'
#' This function processes a raster file, applies thresholds, creates polygons,
#' and simplifies them based on various parameters.
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
#' @param calculate_max_count Logical. If TRUE, outputs a reasonable number of polygons
#'   based on area size. Default is FALSE
#' @param contain_polygons SpatVector. If provided with calculate_max_count=TRUE,
#'   only creates polygons within this boundary
#'
#' @return A SpatVector object containing the processed and simplified polygons,
#'   or NULL if no polygons were generated
#'
#' @import terra
#' @import smoothr
#'
#' @examples
#' \dontrun{
#' # Basic usage with default parameters
#' result <- ff_polygonize("input.tif")
#'
#' # Save output with custom threshold
#' ff_polygonize("input.tif",
#'   output_file = "risk_areas.shp",
#'   threshold = "high"
#' )
#' }
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
                          contain_polygons = NA) {
  # Validate inputs and load raster
  input_raster <- load_and_validate_raster(input_raster)
  validate_threshold(threshold)

  # Process raster and determine threshold
  processed <- process_raster(input_raster, threshold, window_size, verbose)
  if (is.null(processed)) {
    return(NULL)
  }

  # Generate polygons
  generation_result <- generate_polygons(
    processed$smoothed_raster,
    minimum_pixel_count,
    input_raster,
    smoothness
  )
  polygons <- generation_result$focus_polygons
  minimum_pixel_area <- generation_result$minimum_pixel_area
  # Apply maximum count filtering if requested
  if (calculate_max_count) {
    polygons <- apply_max_count_filter(
      polygons,
      input_raster,
      contain_polygons,
      threshold,
      minimum_pixel_area,
      verbose
    )
  }

  # Check if any polygons were generated
  if (length(polygons) == 0) {
    ff_cat("Based on the chosen threshold no polygons were generated.
           Lower the threshold to get polygons for this area",
      color = "yellow", log_level = "WARNING"
    )
    return(NULL)
  }

  # Add attributes and write output
  result <- add_polygon_attributes(
    polygons,
    input_raster,
    processed$final_threshold
  )

  if (!is.na(output_file)) {
    ff_cat("writing", length(result), "polygons to", output_file,
      verbose = verbose
    )
    terra::writeVector(x = result, filename = output_file, overwrite = TRUE)
  }

  return(result)
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
  default_treshold <- as.numeric(Sys.getenv("DEFAULT_THRESHOLD"))
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
  minimum_pixel_area <- minimum_pixel_count * (terra::res(input_raster)[1] * 110000)^2

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

#' Apply maximum count filter to polygons
#' @noRd
apply_max_count_filter <- function(polygons,
                                   input_raster,
                                   contain_polygons,
                                   threshold,
                                   minimum_pixel_area,
                                   verbose) {
  if (!is.na(contain_polygons)) {
    polygons <- polygons[contain_polygons, ]
  }

  percentage_covered <- as.numeric(terra::global(
    !is.na(input_raster),
    "mean"
  ))
  raster_area <- as.numeric(terra::expanse(input_raster)[2])
  max_polygons_count <- ceiling(sqrt(raster_area / 1e3) * percentage_covered)

  ff_cat(
    "based on area of raster (hectares:",
    round(raster_area / 1e5),
    ", actual coverage:",
    round(percentage_covered * 100),
    "percent), at maximum",
    max_polygons_count,
    " polygons are generated",
    verbose = verbose
  )

  return(polygons[1:max(1, min(
    max_polygons_count,
    sum(terra::expanse(polygons) >= minimum_pixel_area)
  ))])
}

#' Add attributes to polygons
#' @noRd
add_polygon_attributes <- function(polygons, input_raster, threshold) {
  polygons$risk <- round(terra::extract(
    input_raster, polygons,
    fun = "mean", ID = FALSE
  ), 2)
  polygons$size <- round(terra::expanse(polygons) / 10000)
  polygons$riskfactor <- round(polygons$size * polygons$risk)
  polygons$threshold <- threshold
  polygons$date <- as.character(as.Date(Sys.time()))
  return(polygons)
}
