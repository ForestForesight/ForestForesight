#' Sync ForestForesight Data from S3
#'
#' This function synchronizes ForestForesight data from a public S3 bucket to a local folder.
#' It can sync data for a specific tile or an entire country, including input data, ground truth data,
#' and optionally model data.
#'
#' @param ff_folder Character. Local folder to sync data to.
#' @param identifier Character or SpatVector.
#' When a character it should be either a tile ID (e.g., "00N_000E") or a country ISO3 code.
#' @param features Vector or Character. Either a vector of feature names or one of "Highest", "High",
#' "Medium", "Low", "Everything" (default), or "Small Model" (case-insensitive).
#' @param date_start Character. Start date in format "YYYY-MM-DD". Must be first of month and not before 2021-01-01.
#' @param date_end Character. End date in format "YYYY-MM-DD". Must be first of month and not after current month.
#' @param download_model Logical. Whether to download the corresponding model.
#'  Only works when downloading for entire countries. Default is FALSE.
#' @param download_data Logical. Whether to download the preprocessed input data. Default is TRUE.
#' @param download_groundtruth Logical. Whether to download the groundtruth data as well.
#' This should be turned off when you want to use your own data as groundtruth. Default is TRUE.
#' @param groundtruth_pattern The pattern to search for.
#' This is normally groundtruth6m for 6 months but can be set to groundtruth1m, groundtruth3m
#' or groundtruth12m for one, three or twelve months respectively
#' @param download_predictions Logical. Whether to download the prediction data.
#' Only works when downloading for entire countries. Default is FALSE.
#' @param bucket Character. Name of the S3 bucket. Default is "forestforesight-public".
#' @param region Character. AWS region of the bucket. Default is "eu-west-1".
#' @param verbose Logical. Whether the function should be verbose.
#'
#' @import aws.s3
#' @import terra
#'
#' @return Invisible NULL. The function is called for its side effects.
#'
#' @examples
#' \dontrun{
#' ff_sync("path/to/local/folder", "00N_000E")
#' ff_sync("path/to/local/folder", "BRA", features = "High", date_start = "2021-01-01")
#' ff_sync("path/to/local/folder", "BRA", features = c("feature1", "feature2"))
#' }
#'
#' @export
ff_sync <- function(ff_folder = get_variable("FF_FOLDER"),
                    identifier = get_variable("DEFAULT_COUNTRY"), features = "Everything",
                    date_start = NULL, date_end = NULL,
                    download_model = FALSE, download_data = TRUE,
                    download_predictions = FALSE, download_groundtruth = TRUE,
                    groundtruth_pattern = get_variable("DEFAULT_GROUNDTRUTH"),
                    bucket = "forestforesight-public", region = "eu-west-1",
                    verbose = TRUE) {
  # Validate and process dates
  sync_dates <- sync_initialize_and_check(ff_folder, date_start, date_end, features)
  date_start <- sync_dates$date_start
  date_end <- sync_dates$date_end


  # Process features parameter


  # Create ff_folder if it doesn't exist

  # Determine if identifier is a tile, country code, or SpatVector
  identifier_lookup <- get_tiles_and_country_codes(identifier)
  tiles <- identifier_lookup$tiles
  country_codes <- identifier_lookup$country_codes

  # Generate date range if dates are provided
  dates_to_check <- NULL
  if (!is.null(date_start) && !is.null(date_end)) {
    dates_to_check <- daterange(date_start, date_end)
  }
  # Download model if requested
  if (download_model) {
    model_downloader(ff_folder, country_codes, bucket, region, verbose)
  }
  feature_list <- ff_sync_get_features(features = features, ff_folder = ff_folder)
  # Sync input and ground truth data for each tile
  if (download_data || download_groundtruth) {
    ff_cat("Downloading input and ground truth data", verbose = verbose)
    for (tile in tiles) {
      # Handle input data
      if (download_data) {
        data_downloader(
          ff_folder = ff_folder, tile = tile, feature_list = feature_list,
          dates_to_check = dates_to_check, bucket = bucket, region = region, verbose = verbose
        )
      }

      # Handle ground truth data (unchanged)
      if (download_groundtruth) {
        groundtruth_downloader(
          ff_folder = ff_folder, tile = tile, dates_to_check =
            dates_to_check, bucket = bucket, region = region, verbose = verbose,
          groundtruth_pattern = groundtruth_pattern
        )
      }
    }
  }



  # Download predictions if requested
  if (download_predictions) {
    prediction_downloader(
      ff_folder = ff_folder, country_codes = country_codes,
      dates_to_check = dates_to_check, bucket = bucket, region = region,
      verbose = verbose
    )
  }

  invisible(NULL)
}
ff_sync_get_features <- function(features, ff_folder) {
  feature_list <- NULL
  if (is.character(features)) {
    features <- tolower(features)
    if (length(features) == 1) {
      if (features == "small model") {
        # Find and load small model RDA files
        model_files <- list.files(file.path(ff_folder, "models"),
          pattern = "small\\.rda$",
          recursive = TRUE,
          full.names = TRUE
        )
        if (length(model_files) == 0) {
          stop("no models were found. Either change download_model to TRUE or choose another option for features")
        }
        feature_list <- unique(unlist(lapply(model_files, function(f) {
          get(load(f))
        })))
      } else if (features %in% c("highest", "high", "medium", "low", "everything")) {
        # Load feature metadata
        feature_metadata <- get(data("feature_metadata", envir = environment()))
        importance_levels <- switch(features,
          "highest" = c("Highest"),
          "high" = c("Highest", "High"),
          "medium" = c("Highest", "High", "Medium"),
          "low" = c("Highest", "High", "Medium", "Low"),
          "everything" = unique(feature_metadata$importance)
        )
        feature_list <- feature_metadata$name[feature_metadata$importance %in% importance_levels]
      } else {
        return(features)
      }
    } else {
      # Vector of feature names provided
      feature_metadata <- get("feature_metadata")
      if (!all(features %in% feature_metadata$name)) {
        missing_features <- features[!features %in% feature_metadata$name]
        stop(
          "The following features are not valid: ",
          paste(missing_features, collapse = ", "),
          "available features are:", paste(feature_metadata$name, collapse = "\n")
        )
      }
      feature_list <- features
    }
  }
  if (!"initialforestcover" %in% feature_list) {
    feature_list <- c(feature_list, "initialforestcover")
  }
  return(feature_list)
}

groundtruth_downloader <- function(ff_folder, tile, dates_to_check, bucket, region, verbose, groundtruth_pattern) {
  groundtruth_folder <- file.path(ff_folder, "preprocessed", "groundtruth", tile)
  dir.create(groundtruth_folder, recursive = TRUE, showWarnings = FALSE)
  prefix <- paste0("preprocessed/groundtruth/", tile)
  s3_files <- aws.s3::get_bucket(bucket, prefix = prefix, region = region, max = Inf)

  groundtruth_pattern <- paste0("_", groundtruth_pattern, "\\.tif$")
  matching_files <- grep(groundtruth_pattern, sapply(s3_files, function(x) x$Key), value = TRUE)
  if (!is.null(dates_to_check)) {
    # Filter by dates
    date_matches <- sapply(matching_files, function(f) {
      file_date <- sub(paste0(".*_([0-9]{4}-[0-9]{2}-[0-9]{2})_.*"), "\\1", f)
      file_date %in% dates_to_check
    })

    if (any(date_matches)) {
      matching_files <- matching_files[date_matches]
    } else {
      ff_cat("no predictions found in daterange, downloading latest available")
      # If no files within date range, get the latest available
      matching_files <- select_files_date(given_date = min(dates_to_check), listed_files = matching_files)
    }
  }

  # Sync matched files
  for (file in matching_files) {
    if (!file.exists(file.path(ff_folder, file))) {
      ff_cat(file, verbose = verbose)
      aws.s3::save_object(file,
        bucket = bucket, region = region,
        file = file.path(ff_folder, file), verbose = FALSE
      )
    }
  }
}

get_tiles_and_country_codes <- function(identifier) {
  countries <- terra::vect(get(data("countries", envir = environment())))
  gfw_tiles <- terra::vect(get(data("gfw_tiles", envir = environment())))
  # test if identifier is a tile
  if (is.character(identifier) && nchar(identifier) == 8 && grepl("^[0-9]{2}[NS]_[0-9]{3}[EW]$", identifier)) {
    tiles <- identifier
    identifier <- gfw_tiles[gfw_tiles$tile_id == identifier]
  }
  # if the identifier is a spatvector, do this
  if (inherits(identifier, "SpatVector")) {
    check_spatvector(identifier, check_size = FALSE)
    identifier <- terra::buffer(identifier, width = -1)

    # Intersect the provided SpatVector with countries to get ISO3 codes
    intersected <- terra::intersect(identifier, countries)
    country_codes <- unique(intersected$iso3)

    # Get tiles that intersect with the provided SpatVector
    if (!exists("tiles")) {
      tiles <- terra::intersect(gfw_tiles, identifier)
      tiles <- tiles$tile_id
    }
  } else {
    # if the identifier was a country, get tiles by intersecting
    country_shape <- countries[countries$iso3 == identifier, ]
    if (nrow(country_shape) == 0) stop("Invalid country code")
    country_codes <- identifier
    tiles <- terra::intersect(gfw_tiles, country_shape)
    tiles <- tiles$tile_id
  }

  return(list(tiles = tiles, country_codes = country_codes))
}

model_downloader <- function(ff_folder, country_codes, bucket, region, verbose) {
  countries <- terra::vect(get(data("countries")))
  groups <- countries$group[countries$iso3 == country_codes]
  for (group in groups) {
    prefix <- file.path("models", group)
    s3_files <- aws.s3::get_bucket(bucket, prefix = prefix, region = region, max = Inf)
    s3_files <- sapply(s3_files, function(x) x$Key)
    model_folder <- file.path(ff_folder, "models", group)
    ff_cat("Downloading model to", model_folder, verbose = verbose)
    if (!dir.exists(model_folder)) dir.create(model_folder, recursive = TRUE)
    for (file in s3_files) {
      ff_cat(file, verbose = verbose)
      aws.s3::save_object(file,
        bucket = bucket, region = region,
        file = file.path(ff_folder, file), verbose = FALSE
      )
    }
  }
}

data_downloader <- function(ff_folder, tile, feature_list, dates_to_check, bucket, region, verbose) {
  input_folder <- file.path(ff_folder, "preprocessed", "input", tile)
  dir.create(input_folder, recursive = TRUE, showWarnings = FALSE)

  # List available files in S3
  prefix <- paste0("preprocessed/input/", tile)
  s3_files <- aws.s3::get_bucket(bucket, prefix = prefix, region = region, max = Inf)

  for (feature in feature_list) {
    feature_pattern <- paste0("_", feature, "\\.tif$")
    matching_files <- grep(feature_pattern, sapply(s3_files, function(x) x$Key), value = TRUE)

    if (!is.null(dates_to_check)) {
      # Filter by dates
      date_matches <- sapply(matching_files, function(f) {
        file_date <- sub(paste0(".*_([0-9]{4}-[0-9]{2}-[0-9]{2})_.*"), "\\1", f)
        file_date %in% dates_to_check
      })

      if (any(date_matches)) {
        matching_files <- matching_files[date_matches]
      } else {
        # If no files within date range, get the latest available
        matching_files <- select_files_date(given_date = min(dates_to_check), listed_files = matching_files)
      }
    }

    # Sync matched files
    for (file in matching_files) {
      if (!file.exists(file.path(ff_folder, file))) {
        ff_cat(file, verbose = verbose)
        aws.s3::save_object(file,
          bucket = bucket, region = region,
          file = file.path(ff_folder, file), verbose = FALSE
        )
      }
    }
  }
}

prediction_downloader <- function(ff_folder, country_codes, dates_to_check, bucket, region, verbose) {
  for (country_code in country_codes) {
    pred_folder <- file.path(ff_folder, "predictions", country_code)
    if (!dir.exists(pred_folder)) dir.create(pred_folder, recursive = TRUE)
    prefix <- file.path("predictions", country_code)
    s3_files <- aws.s3::get_bucket(bucket, prefix = prefix, region = region, max = Inf)
    s3_files <- as.character(sapply(s3_files, function(x) x$Key))
    if (!is.null(dates_to_check)) {
      # Filter by dates
      date_matches <- sapply(s3_files, function(f) {
        file_date <- sub(paste0(".*_([0-9]{4}-[0-9]{2}-[0-9]{2}).*"), "\\1", f)
        file_date %in% dates_to_check
      })

      if (any(date_matches)) {
        s3_files <- s3_files[date_matches]
      } else {
        # If no files within date range, get the latest available
        s3_files <- select_files_date(given_date = min(dates_to_check), listed_files = s3_files)
      }
    }
    if (has_value(s3_files)) {
      ff_cat("Downloading predictions to", pred_folder,
        verbose = verbose
      )
    } else {
      ff_cat("no prediction files found for given dates",
        color = "yellow", verbose = verbose
      )
    }
    for (file in s3_files) {
      if (!file.exists(file.path(ff_folder, file))) {
        ff_cat(file, verbose = verbose)
        aws.s3::save_object(file,
          bucket = bucket, region = region,
          file = file.path(ff_folder, file), verbose = FALSE
        )
      }
    }
  }
}

#' Initialize and Validate ForestForesight Sync Parameters
#'
#' This function validates sync parameters and initializes the folder structure for
#' ForestForesight data synchronization. It performs several validation checks on dates
#' and feature settings, and creates the necessary directory structure if it doesn't exist.
#'
#' @param ff_folder Character; path to the ForestForesight data folder
#' @param date_start Character; start date in "YYYY-MM-01" format. Must be after
#'        the earliest available data date and be the first day of a month
#' @param date_end Character; end date in "YYYY-MM-01" format. Must not be after
#'        the current month and must be the first day of a month
#' @param features Character; level of features to sync. Must be one of:
#'        "Highest", "High", "Medium", "Low", "Everything", or "Small Model"
#'
#' @return A list containing validated and potentially adjusted parameters:
#'   \item{date_start}{Character; validated or default start date}
#'   \item{date_end}{Character; validated or default end date}
#'
#' @details
#' The function performs these checks:
#' * Validates feature option against allowed values
#' * Creates ff_folder if it doesn't exist
#' * Ensures dates are first day of month
#' * Validates date ranges against available data
#' * Sets default dates when not provided:
#'   - If only end date given, start date defaults to earliest available
#'   - If only start date given, end date defaults to current month
#'
#' @note
#' The function assumes the existence of environment variable "EARLIEST_DATA_DATE"
#' which defines the earliest available data date in the system.
#'
#' @noRd
sync_initialize_and_check <- function(ff_folder, date_start, date_end, features) {
  ff_features <- get(data("feature_metadata", envir = environment()))
  # get feature names of only features that are in SpatRaster format
  ff_features <- ff_features[ff_features$source != "autogenerated", ]$name
  if (length(features) == 1) {
    if (!((casefold(features) %in%
      c(ff_features, c("highest", "high", "medium", "low", "everything", "small model"))))) {
      stop("incorrect feature option given. please give a vector of feature names
    or choose between:
         Highest, High, Medium, Low, Everything, Small Model")
    }
  } else {
    if (!all((casefold(features) %in%
      ff_features))) {
      stop("incorrect feature option given. please give a vector of feature names
    or choose between:
         Highest, High, Medium, Low, Everything, Small Model")
    }
  }
  current_month <- format(Sys.Date(), "%Y-%m-01")
  if (!dir.exists(ff_folder)) {
    dir.create(ff_folder, recursive = TRUE)
  }


  if (!is.null(date_start)) {
    if (!grepl("-01$", date_start)) {
      stop("date_start must be the first day of a month")
    }
    if (date_start < Sys.getenv("EARLIEST_DATA_DATE")) {
      stop(paste0("date start cannot be before ", Sys.getenv("EARLIEST_DATA_DATE")))
    }
  } else if (!is.null(date_end)) {
    date_start <- Sys.getenv("EARLIEST_DATA_DATE")
  }

  if (!is.null(date_end)) {
    if (!grepl("-01$", date_end)) {
      stop("date_end must be the first day of a month")
    }
    if (date_end > current_month) {
      stop("date_end cannot be after the first of the current month")
    }
  } else if (!is.null(date_start)) {
    date_end <- current_month
  }
  return(list(date_start = date_start, date_end = date_end))
}
