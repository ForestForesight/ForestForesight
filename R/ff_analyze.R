#' Calculate Scores
#'
#' This function calculates scores based on predictions, ground truth, and optional parameters.
#' It processes raster data and compares predictions against ground truth, optionally using
#' a forest mask to restrict the analysis area.
#'
#' @param predictions A SpatRaster object or path to raster file representing the predictions
#' @param groundtruth A SpatRaster object or path to raster file representing the ground truth
#' @param forest_mask Optional SpatRaster object or path to raster file representing the forest mask
#' @param forest_mask_condition Character. Condition to apply to forest mask. default is the environment variable
#' FOREST_MASK_FILTER, normally '>0'
#' @param csv_filename Optional path to CSV file for writing results
#' @param country Character string containing ISO3 code for filtering analysis polygons
#' @param append Logical indicating whether to append results to existing CSV file
#' @param analysis_polygons Optional SpatVector object or path to vector file for analysis areas
#' @param remove_empty Logical indicating whether to remove empty records
#' @param date Character string in YYYY-MM-DD format
#' @param tile Character string in format AA{N-S}_BBB{W-E}
#' @param method Character string indicating the analysis method
#' @param add_wkt Logical indicating whether to add WKT geometry to output
#' @param calculate_best_threshold Logical Whether the optimal threshold
#' should be calculated before reclassifying. Can only be used on continuous
#' prediction values.
#' @param verbose Logical indicating whether to print progress messages
#'
#' @return A SpatVector object containing calculated scores for each polygon
#'
#' @export
ff_analyze <- function(predictions, groundtruth, forest_mask = get_variable("FOREST_MASK"),
                       forest_mask_condition = get_variable("FOREST_MASK_FILTER"), csv_filename = NULL,
                       country = NULL, append = TRUE, analysis_polygons = NULL,
                       remove_empty = TRUE, date = NULL, tile = NULL, method = NA,
                       add_wkt = FALSE, calculate_best_threshold = FALSE, verbose = FALSE) {
  # Get date if not provided
  if (is.null(date)) {
    date <- get_date_from_files(predictions, groundtruth)
  }
  forest_mask <- resolve_forest_mask(groundtruth, forest_mask)
  # Validate and load input data
  loaded_rasters <- validate_and_load_data(predictions, groundtruth, forest_mask, verbose = verbose)
  predictions <- loaded_rasters$predictions
  groundtruth <- loaded_rasters$groundtruth
  forest_mask <- loaded_rasters$forest_mask
  predictions_and_threshold <- reclassify_predictions(
    predictions = predictions, groundtruth = groundtruth,
    forest_mask = forest_mask, calculate_best_threshold = calculate_best_threshold,
    filter_condition = forest_mask_condition,
    verbose = verbose
  )
  predictions <- predictions_and_threshold$predictions
  crosstable_raster <- create_crosstable(predictions, groundtruth, forest_mask, verbose)

  # Load or process analysis polygons
  polygons <- retrieve_analysis_polygons(analysis_polygons, predictions, country)


  # Calculate statistics
  ff_cat("summarizing statistics", verbose = verbose)
  polygons <- calculate_scores_crosstable(
    crosstable_raster = crosstable_raster,
    polygons = polygons, verbose = verbose
  )

  # Add metadata
  ff_cat("adding metadata", verbose = verbose)
  polygons <- add_metadata(polygons, date, method, remove_empty, threshold = predictions_and_threshold$threshold, verbose)

  # Process output and write to file if specified
  process_and_write_output(polygons, csv_filename, append, add_wkt, verbose)

  invisible(polygons)
}

#' Extract date from filename
#'
#' @param predictions Path to predictions file or SpatRaster object
#' @param groundtruth Path to groundtruth file or SpatRaster object
#' @return Character string containing the date in YYYY-MM-DD format
#' @noRd
get_date_from_files <- function(predictions, groundtruth) {
  if (inherits(predictions, "character")) {
    return(substr(basename(predictions), 10, 19))
  } else if (inherits(groundtruth, "character")) {
    return(substr(basename(groundtruth), 10, 19))
  }
  stop("No method to derive date from filename")
}

#' retrieve the analysis polygons for analysis
#'
#' @param analysis_polygons either path to SpatVector, a SpatVector or NULL/NA
#' @return Spatvector of analysis polygons
#' @noRd
retrieve_analysis_polygons <- function(analysis_polygons, predictions, country) {
  if (!has_value(analysis_polygons)) {
    polygons <- terra::vect(get(data("degree_polygons", envir = environment())))
  } else {
    if (inherits(analysis_polygons, "character")) {
      polygons <- terra::vect(analysis_polygons)
    } else {
      polygons <- analysis_polygons
    }
  }
  # Filter by country if specified
  if (has_value(country) && ("iso3" %in% names(polygons))) {
    polygons <- polygons[which(polygons$iso3 == country)]
  }

  # Crop polygons to raster extent
  polygons <- polygons[terra::ext(predictions), ]
  if (nrow(polygons) == 0) {
    stop("the country code is incorrect or the loaded polygons do not overlap
         with the area of the predictions or groundtruth")
  }
  return(polygons)
}

#' calculate and if required print the metrics for the polygons
#'
#' @param crosstable_raster SpatRaster object with values between 0 and 3
#' @param groundtruth SpatVector object for analysis per vector
#' @param verbose Whether to print statements
#' @return Spatvector of analysis polygons with metrics embedded
#' @noRd
calculate_scores_crosstable <- function(crosstable_raster, polygons, verbose) {
  polygons$FP <- terra::extract(crosstable_raster == 1, polygons,
    fun = "sum",
    na.rm = TRUE, touches = FALSE
  )[, 2]
  polygons$FN <- terra::extract(crosstable_raster == 2, polygons,
    fun = "sum",
    na.rm = TRUE, touches = FALSE
  )[, 2]
  polygons$TP <- terra::extract(crosstable_raster == 3, polygons,
    fun = "sum",
    na.rm = TRUE, touches = FALSE
  )[, 2]
  polygons$TN <- terra::extract(crosstable_raster == 0, polygons,
    fun = "sum",
    na.rm = TRUE, touches = FALSE
  )[, 2]

  # Calculate and print F0.5 score if verbose
  if (verbose) {
    ff_cat("calculating F0.5 score", verbose = verbose)
    precision <- sum(polygons$TP, na.rm = TRUE) /
      (sum(polygons$TP, na.rm = TRUE) + sum(polygons$FP, na.rm = TRUE))
    recall <- sum(polygons$TP, na.rm = TRUE) /
      (sum(polygons$TP, na.rm = TRUE) + sum(polygons$FN, na.rm = TRUE))
    ff_cat("F0.5 score:", round(1.25 * precision * recall / (0.25 * precision + recall), 2), verbose = verbose)
    ff_cat("precision:", round(precision, 2), verbose = verbose)
    ff_cat("recall:", round(recall, 2), verbose = verbose)
  }
  return(polygons)
}

#' create a cross-validation table based on groundtruth, prediction and forest mask
#'
#' @param predictions SpatRaster object
#' @param groundtruth SpatRaster object
#' @param forest_mask SpatRaster object
#' @param verbose Whether to print statements
#' @return crosstable raster of type SpatRaster
#' @noRd
create_crosstable <- function(predictions, groundtruth, forest_mask, verbose) {
  # Create crosstable raster
  if (has_value(forest_mask)) {
    ff_cat("using forest mask", verbose = verbose)
    crosstable_raster <- (2 * groundtruth + predictions) * (forest_mask > 0)
  } else {
    crosstable_raster <- 2 * groundtruth + predictions
  }
  return(crosstable_raster)
}

#' Validate input data types and load rasters
#'
#' @param predictions Path to predictions file or SpatRaster object
#' @param groundtruth Path to groundtruth file or SpatRaster object
#' @param forest_mask Optional path to forest mask file or SpatRaster object
#' @param verbose Whether to print statements
#' @return List containing loaded SpatRaster objects
#' @noRd
validate_and_load_data <- function(predictions, groundtruth, forest_mask = NULL, verbose) {
  if (!inherits(predictions, c("SpatRaster", "character"))) {
    stop("predictions is not a raster or path to a raster")
  }
  if (!inherits(groundtruth, c("SpatRaster", "character"))) {
    stop("groundtruth is not a raster or path to a raster")
  }

  # Load predictions
  if (inherits(predictions, "character")) {
    if (!file.exists(predictions)) {
      stop("predictions file does not exist")
    }
    predictions <- terra::rast(predictions)
  }

  # Load groundtruth
  if (inherits(groundtruth, "character")) {
    if (!file.exists(groundtruth)) {
      stop("groundtruth file does not exist")
    }
    groundtruth <- terra::rast(groundtruth, win = terra::ext(predictions))
  }

  # Process groundtruth
  groundtruth[is.na(groundtruth)] <- 0
  groundtruth <- groundtruth > 0

  # Load forest mask if provided
  if (has_value(forest_mask)) {
    if (inherits(forest_mask, "character")) {
      if (!file.exists(forest_mask)) {
        stop("forest mask file does not exist")
      }
      forest_mask <- terra::rast(forest_mask)
    }
    forest_mask <- terra::crop(forest_mask, groundtruth)
  }

  ff_cat("finished loading rasters", verbose = verbose)
  list(predictions = predictions, groundtruth = groundtruth, forest_mask = forest_mask)
}

#' Add metadata to polygons
#'
#' @param polygons SpatVector object containing analysis results
#' @param date Character string containing the date
#' @param method Character string containing the method name
#' @param remove_empty Logical indicating whether to remove empty records
#' @param threshold Numeric the threshold that was used to calculate the accuracy
#' @param verbose Logical indicating whether to print progress messages
#' @return Updated SpatVector object
#' @noRd
add_metadata <- function(polygons, date, method, remove_empty = TRUE, threshold, verbose = FALSE) {
  polygons$date <- date
  polygons$method <- method
  polygons$threshold <- threshold
  if (remove_empty) {
    empty_indices <- which(rowSums(as.data.frame(polygons[, c("FP", "FN", "TP")]), na.rm = TRUE) == 0)
    if (length(empty_indices) > 0) {
      ff_cat("removing", length(empty_indices), "empty records", verbose = verbose)
      polygons <- polygons[-empty_indices, ]
    }
  }

  return(polygons)
}

#' Process output and write to CSV
#'
#' @param polygons SpatVector object containing analysis results
#' @param csv_filename Optional path to output CSV file
#' @param append Logical indicating whether to append to existing file
#' @param add_wkt Logical indicating whether to add WKT geometry
#' @param verbose Logical indicating whether to print progress messages
#' @return Processed data frame
#' @noRd
process_and_write_output <- function(polygons, csv_filename = NULL, append = TRUE,
                                     add_wkt = FALSE, verbose = FALSE) {
  if (add_wkt) {
    polygons_dataframe <- as.data.frame(polygons, geom = "wkt")
  } else {
    polygons_dataframe <- as.data.frame(polygons)
  }

  if (has_value(csv_filename)) {
    if (append && file.exists(csv_filename)) {
      ff_cat("appending to existing dataset", verbose = verbose)
      previous_data <- read.csv(csv_filename)
      previous_data$X <- NULL
      # Get all unique column names
      all_columns <- unique(c(names(previous_data), names(polygons_dataframe)))

      # Add missing columns to each dataframe with NA
      previous_data[setdiff(all_columns, names(previous_data))] <- NA
      polygons_dataframe[setdiff(all_columns, names(polygons_dataframe))] <- NA

      # Now rbind will work without errors
      polygons_dataframe <- rbind(previous_data, polygons_dataframe)
      polygons_dataframe <- # Get all unique column names

        write.csv(polygons_dataframe, csv_filename)
    } else {
      if (!file.exists(csv_filename) && append && verbose) {
        ff_cat("the given file does not exist, while append was set to TRUE",
          color = "yellow", verbose = verbose, log_level = "WARNING"
        )
      }
      write.csv(polygons_dataframe, csv_filename)
    }
  }

  return(polygons_dataframe)
}

#' Reclassify Prediction Raster Based on Thresholds
#'
#' @description
#' Internal function to reclassify continuous prediction values into binary values
#' based on either a default threshold or an automatically calculated optimal threshold.
#'
#' @param predictions SpatRaster. Raster containing prediction values (either continuous or already classified).
#' @param groundtruth SpatRaster. Reference raster containing true values for threshold optimization.
#' @param forest_mask SpatRaster. Optional mask defining forest areas for focused threshold calculation.
#' @param calculate_best_threshold logical. Whether to calculate optimal threshold (TRUE) or use default (FALSE).
#' @param filter_condition filter condition to apply to the forest mask, like >0, which is the default.
#' @param verbose logical. Whether to output any verbose statements.
#'
#' @details
#' The function handles three main cases:
#' * Unclassified predictions without threshold calculation: uses default 0.5 threshold
#' * Unclassified predictions with threshold calculation: finds optimal threshold using forest mask
#' * Already classified predictions: returns as-is unless threshold calculation requested
#'
#' @return SpatRaster. Binary classified raster with values 0 and 1.
#'
#' @noRd
reclassify_predictions <- function(predictions, groundtruth, forest_mask, calculate_best_threshold, filter_condition, verbose) {
  threshold <- get_variable("DEFAULT_THRESHOLD")
  # Check and reclassify predictions if needed. multiply by 100 because freq automatically turns to integer
  classified <- nrow(terra::freq(predictions, digits = 2)) < 3
  if (!classified && !calculate_best_threshold) {
    ff_cat("The raster seems to be not classified, automatically reclassifying raster
    based on the default", threshold, "threshold.
           If this is not wanted, please load the raster before using ff_analyze and classify it according
           to the wanted threshold", color = "yellow", log_level = "WARNING")
    predictions <- as.numeric(predictions > threshold)
  }
  if (classified && calculate_best_threshold) {
    stop("calculate_best_threshold was set to TRUE but the predictions raster has already been classified")
  }
  if (!classified && calculate_best_threshold) {
    ff_cat("calculalating optimal threshold", verbose = verbose)
    if (has_value(forest_mask)) {
      optimal_values <- find_best_threshold(
        prediction = predictions * filter_raster_by_condition(forest_mask, filter_condition),
        groundtruth = groundtruth * filter_raster_by_condition(forest_mask, filter_condition)
      )
      threshold <- optimal_values$best_threshold
      ff_cat("automatically found optimal threshold:", round(threshold, 2))
      predictions <- as.numeric(predictions > threshold)
    }
  }
  return(list(predictions = predictions, threshold = threshold))
}


#' Resolve forest mask file path
#'
#' @description
#' Resolves the forest mask file path based on the predictions file location.
#' If forest_mask matches the default from environment variables, searches for
#' a matching mask file in the predictions directory.
#'
#' @param predictions Character string. Path to the predictions .tif file.
#' @param forest_mask Character string. Current forest mask value to potentially resolve.
#'
#' @return Character string. Resolved forest mask file path.
#'
#' @details
#' If forest_mask equals the FOREST_MASK environment variable value and predictions
#' is a valid .tif file path, searches the predictions directory for a .tif file
#' containing the forest mask pattern. Errors if no matching file or multiple
#' matching files are found.
#'
#' @examples
#' \dontrun{
#' # With environment variable FOREST_MASK="initialforestcover"
#' forest_mask <- resolve_forest_mask(
#'   predictions = "path/to/predictions.tif",
#'   forest_mask = "initialforestcover"
#' )
#' }
#'
#' @noRd
resolve_forest_mask <- function(groundtruth, forest_mask) {
  # Early return if forest_mask doesn't match environment variable
  if (!inherits(forest_mask, "character") || forest_mask != get_variable("FOREST_MASK")) {
    return(forest_mask)
  }

  # Validate predictions input
  if (!is.character(groundtruth)) {
    stop("Predictions must be a character string containing a file path,
         otherwise give path to forestmask or let forestmask be a SpatRaster")
  }

  if (!file.exists(groundtruth)) {
    stop(sprintf("Predictions file does not exist: %s", groundtruth))
  }

  if (!grepl("\\.tif$", groundtruth)) {
    stop("Predictions must be a .tif file")
  }

  # Get the directory containing the predictions file
  groundtruth_dir <- gsub("groundtruth", "input", dirname(groundtruth))

  # Get all .tif files in the directory
  tif_files <- list.files(
    path = groundtruth_dir,
    pattern = "\\.tif$",
    full.names = TRUE
  )

  # Search for files containing the forest_mask pattern
  mask_pattern <- get_variable("FOREST_MASK")
  mask_files <- tif_files[grepl(mask_pattern, basename(tif_files), fixed = TRUE)]

  if (length(mask_files) == 0) {
    stop(sprintf(
      "No forest mask file containing pattern '%s' found in directory: %s",
      mask_pattern,
      gsub("groundtruth", "input", groundtruth_dir)
    ))
  } else if (length(mask_files) > 1) {
    stop(sprintf(
      "Multiple forest mask files found matching pattern '%s' in directory: %s\nFiles: %s",
      mask_pattern,
      gsub("groundtruth", "input", groundtruth_dir),
      paste(basename(mask_files), collapse = ", ")
    ))
  }

  # Return the single matching file path
  return(mask_files[1])
}

#' Filter SpatRaster by condition
#'
#' This function filters a single-layer SpatRaster based on a specified condition.
#'
#' @param input_raster The input SpatRaster to filter (single layer).
#' @param filter_condition A character vector specifying the conditions for each feature.
#' @param verbose Logical indicating whether to display verbose output (default is TRUE).
#'
#' @return A filtered SpatRaster where cells not meeting the condition are set to NA.
#'
#' @examples
#' \dontrun{
#' filtered_rast <- filter_raster_by_condition(
#'   filter_conditions = ">0",
#'   input_raster = my_raster
#' )
#' }
#'
#' @noRd
filter_raster_by_condition <- function(input_raster, filter_condition, verbose = TRUE) {
  operator <- gsub("[[:alnum:]\\.-]", "", filter_condition)
  value <- as.numeric(gsub("[^0-9\\.-]", "", filter_condition))

  ff_cat("filtering on condition", filter_condition, verbose = verbose)

  input_raster <- switch(operator,
    ">" = input_raster > value,
    "<" = input_raster < value,
    "==" = input_raster == value,
    "!=" = input_raster != value,
    ">=" = input_raster >= value,
    "<=" = input_raster <= value,
    input_raster
  )


  return(input_raster)
}
