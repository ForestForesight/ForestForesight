#' Get environment variable with type conversion
#'
#' @description
#' Retrieves an environment variable and automatically converts it to the appropriate data type
#' based on the variable name. Handles dates, logical values, numeric thresholds, and text variables.
#'
#' @param variable_name Character string. Name of the environment variable to retrieve.
#' @param default_value Optional default value if the environment variable is not set.
#'                     Must be of the appropriate type for the variable.
#'
#' @return The environment variable converted to its appropriate type:
#'   * Date for EARLIEST_DATA_DATE and FF_PREP_QC_DATE
#'   * Logical for LOGGING and TIMESTAMP
#'   * Numeric for DEFAULT_THRESHOLD
#'   * Character for all other variables
#'
#' @details
#' Handles the following ForestForesight environment variables with specific type conversions:
#'   * EARLIEST_DATA_DATE: Converted to Date (must be YYYY-MM-DD format)
#'   * FF_PREP_QC_DATE: Converted to Date (must be YYYY-MM-DD format)
#'   * DEFAULT_THRESHOLD: Converted to numeric (must be between 0 and 1)
#'   * LOGGING: Converted to logical
#'   * TIMESTAMP: Converted to logical
#'   * All others: Returned as character strings
#'
#' @examples
#' \dontrun{
#' # Get date variable
#' start_date <- get_variable("EARLIEST_DATA_DATE", "2021-01-01")
#'
#' # Get threshold
#' threshold <- get_variable("DEFAULT_THRESHOLD", 0.5)
#'
#' # Get logical flag
#' logging_enabled <- get_variable("LOGGING", TRUE)
#'
#' # Get string variable
#' country <- get_variable("DEFAULT_COUNTRY", "BRN")
#' }
#'
#' @export
get_variable <- function(variable_name, default_value = NULL) {
  # Get raw value from environment
  value <- Sys.getenv(variable_name, unset = NA_character_)

  # If value is NA or empty string, use default if provided
  if (is.na(value) || value == "") {
    if (is.null(default_value)) {
      return(NULL)
    }
    value <- default_value
  }

  # Convert based on variable type
  if (variable_name %in% c("EARLIEST_DATA_DATE", "FF_PREP_QC_DATE")) {
    # Convert to date
    tryCatch(
      {
        result <- as.Date(value)
        if (result < as.Date("2021-01-01")) {
          ff_cat("Date is before 2021-01-01. This might affect data availability.", color = "yellow")
        }
        return(result)
      },
      error = function(e) {
        stop(sprintf("Invalid date format for %s. Must be YYYY-MM-DD", variable_name))
      }
    )
  } else if (variable_name == "DEFAULT_THRESHOLD") {
    # Convert to numeric and validate range
    result <- as.numeric(value)
    if (is.na(result) || result <= 0 || result >= 1) {
      stop("DEFAULT_THRESHOLD must be between 0 and 1")
    }
    return(result)
  } else if (variable_name %in% c("LOGGING", "TIMESTAMP")) {
    # Convert to logical
    if (toupper(value) %in% c("TRUE", "FALSE")) {
      return(as.logical(value))
    } else {
      stop(sprintf("%s must be TRUE or FALSE", variable_name))
    }
  } else if (variable_name == "DEFAULT_GROUNDTRUTH") {
    # Validate groundtruth pattern
    valid_patterns <- c("groundtruth1m", "groundtruth3m", "groundtruth6m", "groundtruth12m")
    if (!value %in% valid_patterns) {
      stop(sprintf(
        "Invalid groundtruth pattern. Must be one of: %s",
        paste(valid_patterns, collapse = ", ")
      ))
    }
    return(value)
  } else if (variable_name == "FOREST_MASK_FILTER") {
    # Validate filter format
    if (!grepl("^(>|<|>=|<=|=|==)\\s*-?\\d+(\\.\\d+)?$", value)) {
      stop("FOREST_MASK_FILTER must be an operator (>, <, >=, <=, =, ==) followed by a number")
    }
    return(value)
  }

  # Return as character for all other variables
  return(value)
}

#' Select Files Based on Closest Previous Date for Each Feature
#'
#' This function takes a given date and a vector of file names and returns a vector
#' containing the file names corresponding to the closest previous date for each unique feature.
#'
#' @param given_date The reference date in the format "YYYY-MM-DD".
#' @param listed_files A character vector of file names.
#'
#' @return A character vector of selected file names based on the closest previous date for each feature.
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' given_date <- "2022-03-15"
#' listed_files <- c(
#'   "mydir/tile_YYYY-MM-DD_feature.tif",
#'   "mydir/tile_YYYY-MM-DD_feature.tif",
#'   "mydir/tile_YYYY-MM-DD_feature.tif",
#'   "mydir/tile_YYYY-MM-DD_feature.tif",
#'   "mydir/tile_YYYY-MM-DD_feature.tif",
#' )
#'
#' result <- select_files_date(given_date, listed_files)
#' print(result)
#' }
#'
#' @seealso \code{\link{grep}}, \code{\link{lubridate::ymd}}
#'
#' @importFrom lubridate ymd
#'
#' @export
select_files_date <- function(given_date, listed_files) {
  matching_indices <- grep(".*_\\d{4}-\\d{2}-\\d{2}_([^_]+).*", listed_files)
  if (length(listed_files) != length(matching_indices)) {
    ff_cat("Listed_files contains files with incorrect name.
           The correct file name should be in the format: tile_YYYY-MM-DD_feature.tif.
           Incorect files will be excluded and not processed", color = "yellow")
  }
  matching_files <- listed_files[matching_indices]
  unique_names <- unique(gsub(".*_\\d{4}-\\d{2}-\\d{2}_([^_]+).*", "\\1", matching_files))
  selected_files <- character(0)

  for (feature in sort(unique_names)) {
    matching_files_feature <- grep(feature, matching_files, value = TRUE)
    dates_only <- gsub(".*_(\\d{4}-\\d{2}-\\d{2}).*", "\\1", matching_files_feature)
    date_vector <- lubridate::ymd(dates_only)
    indices <- which(date_vector <= lubridate::ymd(given_date))
    if (length(indices) > 0) {
      closest_previous_date <- date_vector[which.min(abs(date_vector[indices] - lubridate::ymd(given_date)))]
      selected_file <- grep(paste0(closest_previous_date, "_", feature), matching_files_feature, value = TRUE)
      selected_files <- c(selected_files, selected_file)
    }
  }
  return(selected_files)
}

#' Check if an object has a meaningful value
#'
#' This function determines whether an object contains meaningful data by checking
#' for NULL, NA, empty strings, and other edge cases. For vectors with length > 1,
#' it returns TRUE even if they contain some NA values. For SpatRaster objects,
#' it returns TRUE if the raster exists and has valid dimensions.
#'
#' @param x An R object to be tested.
#'
#' @return Logical. TRUE if the object contains meaningful data, FALSE otherwise.
#'   Returns TRUE for vectors with length > 1 even if they contain NA values.
#'   Returns FALSE for NULL, single NA values, empty strings, and empty vectors.
#'   For SpatRaster objects, returns TRUE if the raster exists and has valid dimensions.
#'
#' @examples
#' has_value(c(1, 2, NA, 4)) # TRUE (vector with length > 1)
#' has_value(NULL) # FALSE
#' has_value(NA) # FALSE
#' has_value("") # FALSE
#' has_value(character(0)) # FALSE
#' has_value(NA_character_) # FALSE
#' has_value(factor("a")) # TRUE
#' has_value(list()) # FALSE
#' \dontrun{
#' r <- terra::rast() # Empty raster
#' has_value(r) # FALSE
#' }
#'
#' @export
has_value <- function(x) {
  # Early returns for NULL and empty vectors
  if (is.null(x) || length(x) == 0) {
    return(FALSE)
  }

  # Handle SpatRaster objects
  if (inherits(x, "SpatRaster")) {
    return(terra::ncell(x) > 0 && !is.null(terra::ext(x)))
  }

  # Always return TRUE for vectors longer than 1
  if (length(x) > 1) {
    return(TRUE)
  }

  # Handle different types of single values
  if (is.na(x) || # Handles all NA types
    identical(x, "") || # Empty string
    identical(x, logical(0)) || # Empty logical
    identical(x, list()) || # Empty list
    identical(x, numeric(0))) { # Empty numeric
    return(FALSE)
  }

  return(TRUE)
}

#' Generate a vector with the first of every month between two given dates
#'
#' @param start_date A character string representing the start date in "YYYY-MM-DD" format.
#' @param end_date A character string representing the end date in "YYYY-MM-DD" format.
#' @return A character vector with the first of every month between start_date and end_date.
#' @examples
#' daterange("2022-01-15", "2022-04-15")
#' # Returns: [1] "2022-01-01" "2022-02-01" "2022-03-01" "2022-04-01"
#'
#' @export
daterange <- function(start_date, end_date) {
  # Convert input strings to Date objects using lubridate
  start_date <- lubridate::ymd(start_date)
  end_date <- lubridate::ymd(end_date)

  # Generate the sequence of first days of each month
  result <- seq(lubridate::floor_date(start_date, "month"), lubridate::floor_date(end_date, "month"), by = "1 month")

  # Convert the result to character
  result <- as.character(result)

  return(result)
}

#' Calculate F-score
#'
#' This function calculates the F-score given the ground truth, threshold and predicted values.
#'
#' @param gt The ground truth vector.
#' @param pred The predicted vector.
#' @param threshold The threshold for prediction.
#' @param beta The weight of precision in the F-score calculation.
#' @param pr Logical. whether it should also return the precision and recall
#' @return F-score value.
#' @export
get_f_score <- function(gt, pred, threshold = 0.5, beta = 0.5, pr = FALSE) {
  gt[is.na(gt)] <- 0
  pred[is.na(pred)] <- 0
  # Convert predictions to binary
  pred <- as.numeric(pred >= threshold) + gt * 2

  # Calculate true positives, false positives, and false negatives
  tp <- sum(pred == 3)
  fp <- sum(pred == 1)
  fn <- sum(pred == 2)

  # Calculate precision and recall
  precision <- tp / (tp + fp)
  recall <- tp / (tp + fn)

  # Calculate F-score
  f_score <- (1 + beta^2) * (precision * recall) / ((beta^2 * precision) + recall)
  if (!pr) {
    return(f_score)
  }
  return(list("F05" = f_score, "precision" = precision, "recall" = recall))
}

#' Retrieve raster files based on specified criteria.
#'
#' This function retrieves raster files from a specified data folder based on the given date, feature, and tile pattern.
#'
#' @param datafolder A character string specifying the path to the data folder where raster files are stored.
#' This should be in the preprocessed folder of Forest Foresight.
#' @param date A Date object representing the date for which raster files are to be retrieved.
#' @param feature A character string specifying the feature of interest to filter raster files.
#' @param tile A character string specifying the pattern to filter specific tiles of raster files.
#' @param shape A spatial shape that can be used instead of tile to return the files for one or more files.
#' @param return_raster Logical. Whether it should return the (mosaiced) raster instead of just the file names.
#' @param verbose Logical. Whether print statements should be output.
#' @return A character vector containing the file paths of the selected raster files.


#' @export
#' @examples
#' \dontrun{
#' datafolder <- "/path/to/data"
#' date <- as.Date("2022-01-01")
#' feature <- "initialforestcover"
#' tile <- "00N_070W"
#' get_raster(datafolder, date, feature, tile)
#' }
#' # Returns: Vector of file paths to selected raster files.
get_raster <- function(datafolder, date, feature, tile = NULL, shape = NULL, return_raster = FALSE, verbose = FALSE) {
  if ((has_value(tile) + has_value(shape)) != 1) {
    stop("either a tile or a shape should be given")
  }
  if (has_value(shape)) {
    gfw_tiles <- terra::vect(get(data("gfw_tiles", envir = environment())))
    tile <- gfw_tiles[terra::project(shape, gfw_tiles), ]$tile_id
    if (verbose) {
      ff_cat("getting data from tiles", paste(tile, collapse = ", "))
    }
  }
  allfiles <- unlist(sapply(tile, function(x) list.files(datafolder, recursive = TRUE, pattern = x, full.names = TRUE)))
  allfiles <- allfiles[grep("tif$", allfiles)]
  allfiles <- allfiles[grep(paste0("_", feature, "\\."), allfiles)]
  filename <- ForestForesight::select_files_date(date, allfiles)
  if (!has_value(filename)) {
    stop("no files were found in this folder with this combination of date, tile and feature")
  }
  if (verbose) {
    ff_cat("found files", paste(filename, collapse = ", "))
  }
  if (return_raster) {
    if (length(tile) == 1) {
      return(terra::rast(filename))
    } else {
      if (verbose) {
        ff_cat("merging tiles")
      }
      return(do.call(terra::merge, unname(sapply(filename, function(x) terra::rast(x)))))
    }
  } else {
    return(filename)
  }
}

#' Find Best Threshold
#'
#' This function finds the best threshold for a given prediction function by maximizing the evaluation function.
#'
#' @param prediction A vector of predictions (numeric) or SpatRaster.
#' @param groundtruth A vector of ground truth values (binary) or SpatRaster.
#' @param optimize_function The evaluation function to optimize. Default is get_f_score.
#' @param a Initial guess for the lower bound of threshold search.
#' @param b Initial guess for the upper bound of threshold search.
#' @param tol Tolerance for convergence.
#' @param maxiter Maximum number of iterations.
#' @param beta The weight of precision in the F-score calculation.
#' @return A list containing the best threshold and the corresponding F-score.
#' @examples
#' find_best_threshold(c(0.2, 0.6, 0.7), c(0, 1, 1))
#' @export
find_best_threshold <- function(prediction, groundtruth, optimize_function = get_f_score,
                                a = 0.45, b = 0.55, tol = 0.001, maxiter = 100, beta = 0.5) {
  if (inherits(prediction, "SpatRaster")) {
    prediction <- as.vector(prediction)
  }
  if (inherits(groundtruth, "SpatRaster")) {
    groundtruth <- as.vector(groundtruth)
  }
  # Golden ratio
  phi <- (1 + sqrt(5)) / 2

  # Calculate step sizes
  inv_phi <- 1 / phi

  # Initialize variables
  x1 <- b - inv_phi * (b - a)
  x2 <- a + inv_phi * (b - a)
  f1 <- optimize_function(groundtruth, prediction, x1, beta)
  f2 <- optimize_function(groundtruth, prediction, x2, beta)

  # Iteration loop
  for (i in 1:maxiter) {
    if (f1 > f2) {
      b <- x2
      x2 <- x1
      f2 <- f1
      x1 <- b - inv_phi * (b - a)
      f1 <- optimize_function(groundtruth, prediction, x1, beta)
    } else {
      a <- x1
      x1 <- x2
      f1 <- f2
      x2 <- a + inv_phi * (b - a)
      f2 <- optimize_function(groundtruth, prediction, x2, beta)
    }

    # Check for convergence
    if (abs(b - a) < tol) break
  }

  # Return the best threshold and the corresponding F-score
  return(list(best_threshold = (a + b) / 2, max_f_score = optimize_function(groundtruth, prediction, ((a + b) / 2), beta)))
}
