#' Prepare data for XGBoost training, validating and predicting
#'
#' This function prepares data for the ForestForesight training and predicting algorithm based on specified parameters.
#'
#' @param datafolder Path to the main data directory,
#' which should contain the "input" and "groundtruth" subdirectories.
#' These subdirectories must include the respective degree folders.
#' Default is the system variable ff_datafolder.
#' @param country ISO3 code of the country or countries for which the data is prepared.
#' Optional if either shape or tiles is given.
#' @param shape SpatVector for which the data is prepared.
#' Optional if either country or tiles is given.
#' @param tiles Vector of tiles in the syntax of e.g., "10N_080W" for which the data is prepared.
#' Optional if either shape or country is given.
#' @param groundtruth_pattern Pattern to identify ground truth files.
#' Default is "groundtruth6m" (set in config.json, groundtruth of future six months in binary format).
#' @param dates vector of dates in the format "YYYY-MM-DD". Default is "2021-01-01".
#' @param inc_features Vector of features to exclusively include in the data preparation.
#' @param exc_features Vector of features to exclude from the data preparation.
#' @param filter_features Vector of features for filtering data. Default is NULL.
#' Example: 'initialforestcover'.
#' @param filter_conditions Vector of conditions corresponding to `filter_features`.
#' Each entry should consist of an operator and a value.
#' Should be of the same length as `filter_features`. Default is NULL. Example: '>0'.
#' @param validation_sample Float between 0 and 1 indicating how much of the
#' training dataset should be used for validation. Default is 0.
#' Advised not to set above 0.3.
#' @param sample_size Fraction size of the random sample. Should be > 0 and <= 1. Default is 0.3.
#' @param add_date Boolean indicating whether to add date-related features
#' ("sinmonth", "month","monthssince2019"). Default is TRUE.
#' @param verbose Boolean indicating whether to display progress messages. Default is TRUE.
#' @param shrink Option to modify the input area when a country is selected.
#' This parameter determines how the spatial extent of the data is
#' adjusted based on the selected country.
#' Options are:
#' \describe{
#'   \item{"none"}{No modification to the input area. The entire extent is used as-is. (Default)}
#'   \item{"crop"}{Crops the input area to the boundaries of the selected country
#'   using the exact extent of the country's shape.}
#'   \item{"crop-deg"}{Similar to "crop", but the resulting extent is adjusted
#'   to the nearest whole degree (latitude/longitude). This ensures the extent aligns with whole degree boundaries.}
#'   \item{"extract"}{Extracts data only within the boundaries of the selected country.
#'   The data is limited to the exact extent of the country's shape,
#'   and the function `terra::extract` is used to retrieve the data within this area.}
#' }
#' @param window Set the extent on which to process. Default is NA to derive it from the data.
#' @param label_threshold Threshold for labeling the ground truth.
#' Default is 1, meaning a pixel is labeled as deforested if at least one deforestation event has occurred.
#' If `label_threshold = NA`, the ground truth will represent the total number of
#' deforestation events rather than a binary label.
#' @param add_xy Boolean indicating whether to add x and y coordinates as features. Default is FALSE.
#'
#' @return A list containing:
#'   \item{feature_dataset}{A list with features and labels for training}
#'   \item{validation_matrix}{A list with features and labels for validation (if validation_sample > 0)}
#'   \item{test_indices}{Indices of the filtered samples}
#'   \item{groundtruth_raster}{A SpatRaster of the ground truth}
#'   \item{features}{A vector of feature names}
#'   \item{has_groundtruth}{A boolean stating that the groundtruthraster is
#'   actually the groundtruth and not just a template}
#'
#' @export
#'
#' @references
#' Zillah Calle (2023)
#' Jonas van Duijvenbode (2023)
#'
#' @examples
#' \dontrun{
#' prepared_data <- ff_prep(
#'   datafolder = "path/to/data",
#'   country = "BRA",
#'   dates = ForestForesight::daterange("2022-01-01", "2022-12-31"),
#'   filter_features = "initialforestcover",
#'   filter_conditions = ">0"
#' )
#' }
#'
#' @seealso
#' \code{\link{ff_train}} for training a model with the prepared data
#' \code{\link{ff_predict}} for making predictions using a trained model
#'
#' @keywords machine-learning data-preparation forestry

ff_prep <- function(datafolder = get_variable("FF_FOLDER"), country = get_variable("DEFAULT_COUNTRY"),
                    shape = NA, tiles = NULL,
                    groundtruth_pattern = get_variable("DEFAULT_GROUNDTRUTH"), dates = "2023-01-01",
                    inc_features = NA, exc_features = NA, filter_features = NULL,
                    filter_conditions = NULL, sample_size = 0.3, validation_sample = 0,
                    add_date = TRUE, verbose = TRUE, shrink = "none", window = NA,
                    label_threshold = 1, add_xy = FALSE) {
  ######## pre-conditions check########
  if (!has_value(groundtruth_pattern)) {
    ff_cat("no environment variable for DEFAULT_GROUNDTRUTH, reverting to groundtruth6m",
      color = "yellow", verbose = verbose, log_level = "WARNING"
    )
    groundtruth_pattern <- "groundtruth6m"
  }
  check_pre_conditions(dates, country, shape, tiles, shrink,
    inc_features, exc_features, datafolder,
    verbose = verbose
  )

  ######## Get tiles and shape based on country or custom geometry ########
  tiles_vector <- terra::vect(get(data("gfw_tiles", envir = environment())))
  tile_and_shape <- get_tiles_and_shape(country, shape, tiles_vector, tiles, verbose)
  shape <- tile_and_shape$shape
  tiles <- tile_and_shape$tiles

  ########## list files and filter features######
  list_of_feature_files <- list_and_filter_tile_files(datafolder = datafolder, tiles, groundtruth_pattern, verbose)

  # filter out the features based on inc_features and exc_features
  list_of_feature_files <- filter_files_by_features(
    list_of_feature_files,
    exc_features, inc_features, groundtruth_pattern, verbose
  )

  if (length(tiles) > 1) {
    ff_cat("No groundtruth raster will be returned because multiple tiles are processed together",
      verbose = verbose
    )
  }

  ####### load raster data as matrix#########
  raw_tile_data <- process_tile_data(
    tiles, list_of_feature_files, shape, shrink, window, verbose,
    dates, groundtruth_pattern,
    has_groundtruth = FALSE, add_xy, add_date,
    sample_size, filter_features, filter_conditions
  )

  feature_dataset <- raw_tile_data$feature_dataset
  pixel_indices <- raw_tile_data$pixel_indices
  groundtruth_raster <- raw_tile_data$groundtruth_raster
  has_groundtruth <- raw_tile_data$has_groundtruth


  # split data into feature data and label data

  split_result <- split_feature_and_label_data(
    feature_dataset,
    groundtruth_pattern, label_threshold, groundtruth_raster, verbose
  )
  feature_dataset <- split_result$feature_dataset
  data_label <- split_result$data_label
  groundtruth_raster <- split_result$groundtruth_raster

  ########## create validation sample#######

  validation_result <- create_validation_sample(feature_dataset, data_label, validation_sample)
  feature_dataset <- validation_result$feature_dataset
  validation_matrix <- validation_result$validation_matrix
  ########## return data from prep function to main function####
  if (has_value(feature_dataset$label) && sum(feature_dataset$label) == 0) {
    ff_cat("Data contains no actuals, all labels are 0", color = "yellow", verbose = verbose, log_level = "WARNING")
  }

  return(list(
    "feature_dataset" = feature_dataset,
    "validation_matrix" = validation_matrix,
    "test_indices" = pixel_indices,
    "groundtruth_raster" = groundtruth_raster,
    "features" = colnames(feature_dataset$features),
    "has_groundtruth" = has_groundtruth
  ))
}

check_pre_conditions <- function(dates, country, shape, tiles, shrink,
                                 inc_features, exc_features, verbose, datafolder) {
  ff_cat("Checking ff_prep function input", verbose = verbose)
  if (!has_value(datafolder)) {
    stop("no environment variable for DATA_FOLDER found and no other option given")
  }
  # Check date validity
  if (!has_value(dates) || any(is.na(dates)) || any(dates == "")) {
    stop("No dates were given")
  }
  earliest_date <- get_variable("EARLIEST_DATA_DATE")
  if (!has_value(earliest_date)) {
    ff_cat("no environment variable for EARLIEST_DATA_DATE, reverting to 2021-01-01",
      color = "yellow", verbose = verbose, log_level = "WARNING"
    )
    earliest_date <- "2021-01-01"
  }
  if (as.Date(min(dates)) < as.Date(earliest_date)) {
    stop(paste0("The earliest date available is ", earliest_date))
  }

  # Check input parameters validity
  if (!has_value(tiles) && !has_value(country) && !has_value(shape)) {
    stop("Unknown what to process since no tiles, country, or shape were given")
  }
  if (has_value(shape)) {
    shape <- check_spatvector(shape)
  }
  if (!has_value(country) && !has_value(shape) && shrink != "none") {
    stop("Shrink parameter must be 'none' when neither country nor shape are provided")
  }
  if (all(c(has_value(inc_features), has_value(exc_features)))) {
    stop("Only supply either inc_features or exc_features, not both")
  }
}

get_tiles_and_shape <- function(country, shape, tiles_vector, tiles, verbose) {
  if (has_value(country)) {
    ff_cat("Selecting based on country", verbose = verbose)
    countries <- terra::vect(get(data("countries", envir = environment())))
    shape <- countries[which(countries$iso3 %in% country)]
    tiles_vector <- tiles_vector[shape]$tile_id
    if (is.null(tiles)) {
      tiles <- tiles_vector
    }
    ff_cat("Processing tiles:", paste(tiles, collapse = ", "), verbose = verbose)
  } else if (has_value(shape)) {
    if (!terra::is.lonlat(shape)) {
      shape <- terra::project(shape, "epsg:4326")
    }

    tiles <- tiles_vector[shape]$tile_id

    ff_cat("Selecting based on shape\nProcessing tiles:", paste(tiles, collapse = ", "), verbose = verbose)
  }
  return(list(shape = shape, tiles = tiles))
}

list_and_filter_tile_files <- function(datafolder = NA, tiles, groundtruth_pattern, verbose) {
  input_datafolder <- file.path(datafolder, "preprocessed", "input")
  groundtruth_datafolder <- file.path(datafolder, "preprocessed", "groundtruth")


  ff_cat("Searching", input_datafolder, "for tiles", paste(tiles, collapse = ", "), verbose = verbose)


  # List all files from the input and ground truth directories
  list_of_feature_files <- as.character(unlist(sapply(tiles, function(x) {
    list.files(
      path = file.path(input_datafolder, x), full.names = TRUE, recursive = TRUE,
      pattern = "tif$"
    )
  })))
  groundtruth_files <- as.character(unlist(sapply(tiles, function(x) {
    list.files(
      path = file.path(groundtruth_datafolder, x), full.names = TRUE,
      recursive = TRUE, pattern = paste0(groundtruth_pattern, ".tif$")
    )
  })))

  list_of_feature_files <- c(list_of_feature_files, groundtruth_files)

  # Error handling if no files are found
  if (length(list_of_feature_files) == 0) {
    stop(paste("No folders with tif-files found that correspond to the given tile IDs:", paste(tiles, collapse = ",")))
  } else {
    ff_cat("found tif files for", paste(tiles, collapse = ","), verbose = verbose)
  }

  return(list_of_feature_files)
}

filter_files_by_features <- function(list_of_feature_files, exc_features, inc_features, groundtruth_pattern, verbose) {
  if (has_value(exc_features)) {
    ff_cat("Excluding features", verbose = verbose)
    # Find indices of files that end its basename with a feature in exc_features
    exc_indices <- unique(unlist(sapply(exc_features, function(x) {
      which(endsWith(gsub(".tif", "", basename(list_of_feature_files)), x))
    })))
    if (length(exc_indices) > 0) {
      list_of_feature_files <- list_of_feature_files[-exc_indices]
    }
  }

  if (has_value(inc_features)) {
    # Find indices of files that end its basename with a feature in inc_features
    inc_indices <- unique(unlist(sapply(c(inc_features, groundtruth_pattern), function(x) {
      which(endsWith(gsub(".tif", "", basename(list_of_feature_files)), x))
    })))
    if (length(inc_indices) > 0) {
      list_of_feature_files <- list_of_feature_files[inc_indices]
    }
  }

  if (length(list_of_feature_files) == 0) {
    stop("After including and excluding the requested variables there are no files left")
  }

  return(list_of_feature_files)
}

prepare_raster_data_by_tile <- function(selected_files, shape, shrink, window, verbose) {
  for (file in selected_files) {
    if (!exists("extent")) {
      extent <- terra::ext(terra::rast(file))
    } else {
      extent <- terra::intersect(extent, terra::ext(terra::rast(file)))
    }
  }

  if (shrink %in% c("extract", "crop")) {
    extent <- terra::ext(terra::crop(terra::as.polygons(extent), terra::ext(shape)))
  }

  if (shrink == "crop-deg") {
    extent <- terra::ext(terra::crop(terra::as.polygons(extent), terra::ext(shape)))
    extent[1] <- floor(extent[1])
    extent[2] <- ceiling(extent[2])
    extent[3] <- floor(extent[3])
    extent[4] <- ceiling(extent[4])
  }

  # TODO: check inherits(window, "SpatExtent")'s affect, added here to pass the unit test
  if (!is.null(window) && inherits(window, "SpatExtent")) {
    extent <- terra::intersect(extent, window)
  }

  # TODO: check if !is.null(extent) && is.numeric(extent) doesnt affect much, added to pass unit test
  if (!is.null(extent) && is.numeric(extent)) {
    ff_cat("with extent", round(extent[1], 5), round(extent[2], 5),
      round(extent[3], 5), round(extent[4], 5),
      verbose = verbose
    )
  }
  rasstack <- terra::rast(sapply(selected_files, function(x) terra::rast(x, win = extent)))
  return(list(extent = extent, rasstack = rasstack))
}

load_groundtruth_raster <- function(selected_files, groundtruth_pattern,
                                    first_loop_iteration, verbose, extent, has_groundtruth) {
  if (first_loop_iteration) {
    if (length(grep(groundtruth_pattern, selected_files)) > 0) {
      has_groundtruth <- TRUE
      groundtruth_file <- selected_files[grep(groundtruth_pattern, selected_files)]
      groundtruth_raster <- terra::rast(groundtruth_file, win = extent)
    } else {
      ff_cat("no groundtruth raster was found, first regular raster selected as a template raster.", verbose = verbose)
      groundtruth_raster <- terra::rast(selected_files[1], win = extent)
      groundtruth_raster[] <- 0
    }
  } else {
    groundtruth_raster <- NA
  }
  list_gt_raster <- list(
    groundtruth_raster = groundtruth_raster, has_groundtruth = has_groundtruth,
    first_loop_iteration = first_loop_iteration
  )
  return(list_gt_raster)
}


filter_files_by_date <- function(date, files, groundtruth_pattern) {
  selected_files <- select_files_date(date, files)
  # Remove groundtruth if it is not of the same month
  if (!(grep(groundtruth_pattern, selected_files) %in% grep(date, selected_files))) {
    selected_files <- selected_files[-grep(groundtruth_pattern, selected_files)]
  }

  return(selected_files)
}

transform_raster_to_dataset <- function(rasstack, shape, shrink, add_xy, feature_dataset) {
  if (shrink == "extract") {
    feature_dataset <- terra::extract(rasstack, shape, raw = TRUE, ID = FALSE, xy = add_xy)
  } else {
    feature_dataset <- as.matrix(rasstack)
    if (add_xy) {
      coords <- terra::xyFromCell(rasstack, seq(ncol(rasstack) * nrow(rasstack)))
      feature_dataset <- cbind(feature_dataset, coords)
    }
  }
  # replace  NA with 0's because XGBoost cannot handle NA
  feature_dataset[is.na(feature_dataset)] <- 0
  return(feature_dataset)
}

append_date_based_features <- function(feature_dataset, date) {
  feature_dataset <- as.data.frame(feature_dataset)
  feature_dataset$sinmonth <- sin((2 * pi * as.numeric(format(as.Date(date), "%m"))) / 12)
  feature_dataset$month <- as.numeric(format(as.Date(date), "%m"))
  feature_dataset$monthssince2019 <- round(as.numeric(lubridate::as.period(
    as.Date(date) - as.Date("2019-01-01"), "months"
  ), "months"))
  return(as.matrix(feature_dataset))
}

sample_and_combine_data <- function(date, current_tile_feature_dataset, feature_dataset,
                                    filtered_indices, sample_size, first_loop_iteration, pixel_indices, verbose) {
  # take a random sample if that was applied
  if (sample_size < 1) {
    sample_indices <- sample(
      seq_len(nrow(current_tile_feature_dataset)),
      max(round(nrow(current_tile_feature_dataset) * sample_size), 1)
    )
    current_tile_feature_dataset <- current_tile_feature_dataset[sample_indices, ]
    filtered_indices <- filtered_indices[sample_indices]
  }

  if (has_value(dim(current_tile_feature_dataset))) {
    if (first_loop_iteration) {
      feature_dataset <- current_tile_feature_dataset
      pixel_indices <- filtered_indices
    } else {
      pixel_indices <- c(pixel_indices, filtered_indices + length(pixel_indices))
      common_cols <- intersect(colnames(current_tile_feature_dataset), colnames(feature_dataset))
      notin1 <- colnames(current_tile_feature_dataset)[which(
        !(colnames(current_tile_feature_dataset) %in% common_cols)
      )]
      notin2 <- colnames(feature_dataset)[which(!(colnames(feature_dataset) %in% common_cols))]
      if (length(c(notin1, notin2)) > 0) {
        ff_cat(paste(
          date, ": the following columns are dropped because
                     they are not present in the entire time series: ",
          paste(c(notin1, notin2),
            collapse = ", "
          )
        ), color = "yellow", verbose = verbose, log_level = "WARNING")
      }

      # Subset matrices based on common column names
      # Merge matrices by column names
      feature_dataset <- rbind(
        feature_dataset[, common_cols, drop = FALSE],
        current_tile_feature_dataset[, common_cols, drop = FALSE]
      )
    }
    feature_dataset <- feature_dataset[, order(colnames(feature_dataset))]
    first_loop_iteration <- FALSE
  }

  return(list(
    feature_dataset = feature_dataset, pixel_indices = pixel_indices,
    first_loop_iteration = first_loop_iteration
  ))
}

create_validation_sample <- function(feature_dataset, data_label, validation_sample) {
  if (validation_sample > 0) {
    sample_indices <- sample(seq_len(nrow(feature_dataset)), round(validation_sample * nrow(feature_dataset)))
    train_matrix <- list(features = feature_dataset[-sample_indices, ], label = data_label[-sample_indices])
    validation_matrix <- list(features = feature_dataset[sample_indices, ], label = data_label[sample_indices])
  } else {
    train_matrix <- list(features = feature_dataset, label = data_label)
    validation_matrix <- NA
  }

  return(list(feature_dataset = train_matrix, validation_matrix = validation_matrix))
}

split_feature_and_label_data <- function(feature_dataset, groundtruth_pattern,
                                         label_threshold, groundtruth_raster, verbose) {
  groundtruth_index <- which(colnames(feature_dataset) == groundtruth_pattern)

  if (length(groundtruth_index) == 1) {
    data_label <- feature_dataset[, groundtruth_index]

    if (has_value(label_threshold)) {
      data_label <- as.numeric(data_label >= label_threshold)
      if (inherits(groundtruth_raster, "SpatRaster")) {
        groundtruth_raster <- as.numeric(groundtruth_raster >= label_threshold)
      }
    }
    # Remove groundtruth column from features
    feature_dataset <- feature_dataset[, -groundtruth_index]
  } else {
    ff_cat("No groundtruth rasters found", color = "yellow", verbose = verbose, log_level = "WARNING")
    data_label <- NA
  }

  return(list(feature_dataset = feature_dataset, data_label = data_label, groundtruth_raster = groundtruth_raster))
}

process_tile_data <- function(tiles, list_of_feature_files, shape,
                              shrink, window, verbose, dates,
                              groundtruth_pattern, has_groundtruth, add_xy,
                              add_date, sample_size, filter_features, filter_conditions) {
  first_loop_iteration <- TRUE
  pixel_indices <- NULL
  feature_dataset <- NA
  ####### load raster data as matrix#########
  for (tile in tiles) {
    current_tile_files <- list_of_feature_files[grep(tile, list_of_feature_files)]

    raw_extracted_data <- process_tile_dates(
      tiles, tile, current_tile_files, shape, shrink, window,
      groundtruth_pattern, dates, verbose, add_xy, add_date,
      first_loop_iteration, feature_dataset, sample_size,
      pixel_indices, has_groundtruth, filter_features, filter_conditions
    )

    feature_dataset <- raw_extracted_data$feature_dataset
    pixel_indices <- raw_extracted_data$pixel_indices
    first_loop_iteration <- raw_extracted_data$first_loop_iteration
    groundtruth_raster <- raw_extracted_data$groundtruth_raster
    feature_names <- raw_extracted_data$feature_names
    has_groundtruth <- raw_extracted_data$has_groundtruth


    ff_cat("loading finished, features:", paste(feature_names, collapse = ", "), verbose = verbose)
  }
  return(list(
    feature_dataset = feature_dataset, pixel_indices = pixel_indices,
    groundtruth_raster = groundtruth_raster, has_groundtruth = has_groundtruth
  ))
}

process_tile_dates <- function(tiles, tile, current_tile_files, shape, shrink, window, groundtruth_pattern,
                               dates, verbose, add_xy, add_date, first_loop_iteration, feature_dataset, sample_size,
                               pixel_indices, has_groundtruth, filter_features, filter_conditions) {
  for (date in dates) {
    ff_cat("loading tile data from", tile, "for", date, verbose = verbose)
    selected_files <- filter_files_by_date(date, current_tile_files, groundtruth_pattern)
    raw_extracted_data <- prepare_raster_data_by_tile(selected_files, shape, shrink, window, verbose)
    extent <- raw_extracted_data$extent
    rasstack <- raw_extracted_data$rasstack

    if (length(tiles) > 1) {
      groundtruth_raster <- NA
    } else {
      groundtruth_result <- load_groundtruth_raster(
        selected_files, groundtruth_pattern, first_loop_iteration,
        verbose, extent, has_groundtruth
      )
      groundtruth_raster <- groundtruth_result$groundtruth_raster
      has_groundtruth <- groundtruth_result$has_groundtruth
      first_loop_iteration <- groundtruth_result$first_loop_iteration
    }
    current_tile_feature_dataset <- transform_raster_to_dataset(
      rasstack, shape,
      shrink, add_xy, current_tile_feature_dataset
    ) # the most memory consumptive function
    gc() # garbage collection: to free up memory usage
    # Add date features if necessary
    feature_names <- feature_names <- gsub(
      ".tif", "",
      sapply(basename(selected_files), function(x) strsplit(x, "_")[[1]][4])
    )
    colnames(current_tile_feature_dataset)[seq_along(feature_names)] <- feature_names
    if (add_date) {
      current_tile_feature_dataset <- append_date_based_features(current_tile_feature_dataset, date)
    }
    # filter on filter conditions

    feature_filter_result <- filter_by_feature(filter_features,
      filter_conditions, current_tile_feature_dataset,
      verbose = verbose
    )
    current_tile_feature_dataset <- feature_filter_result$filtered_matrix
    filtered_indices <- feature_filter_result$filtered_indices

    combine_result <- sample_and_combine_data(date, current_tile_feature_dataset, feature_dataset,
      filtered_indices, sample_size, first_loop_iteration,
      pixel_indices,
      verbose = verbose
    )
    feature_dataset <- combine_result$feature_dataset
    pixel_indices <- combine_result$pixel_indices
    first_loop_iteration <- combine_result$first_loop_iteration
  }
  return(list(
    feature_dataset = feature_dataset, pixel_indices = pixel_indices,
    first_loop_iteration = first_loop_iteration,
    groundtruth_raster = groundtruth_raster,
    feature_names = colnames(feature_dataset),
    has_groundtruth = has_groundtruth
  ))
}

#' Filter matrix by feature
#'
#' This function filters a matrix based on specified features and conditions.
#'
#' @param filter_features A character vector specifying the features to filter.
#' @param filter_conditions A character vector specifying the conditions for each feature.
#' @param matrix The input matrix to filter.
#' @param verbose Logical indicating whether to display verbose output (default is TRUE).
#'
#' @return A list containing the filtered matrix and the indices of the filtered rows.
#'
#' @examples
#' filter_by_feature(
#'   filter_features = c("landpercentage", "forestmask"),
#'   filter_conditions = c(">100", ">0"),
#'   matrix = my_matrix
#' )
#'
#' @noRd
filter_by_feature <- function(filter_features, filter_conditions, matrix, verbose = TRUE) {
  merged_spatial_indices <- c()
  if (length(filter_features) > 0) {
    ff_cat("filtering features", verbose = verbose)
    for (i in seq_along(filter_features)) {
      operator <- gsub("[[:alnum:]]", "", filter_conditions[i])
      value <- as.numeric(gsub("[^0-9]", "", filter_conditions[i]))
      filtercolumn <- which(colnames(matrix) == filter_features[i])
      if (length(filtercolumn) == 0) {
        ff_cat("The feature", filter_features[i], "was not found, skipping filtering for this feature",
          color = "yellow", log_level = "WARNING"
        )
        spatial_indices <- seq(dim(matrix)[1])
      } else {
        ff_cat("filtering feature", filter_features[i], "on", filter_conditions[i], verbose = verbose)
        if (operator == ">") {
          spatial_indices <- which(matrix[, filtercolumn] > value)
        }
        if (operator == "<") {
          spatial_indices <- which(matrix[, filtercolumn] < value)
        }
        if (operator == "==") {
          spatial_indices <- which(matrix[, filtercolumn] == value)
        }
        if (operator == "!=") {
          spatial_indices <- which(matrix[, filtercolumn] != value)
        }
        if (operator == ">=") {
          spatial_indices <- which(matrix[, filtercolumn] >= value)
        }
        if (operator == "<=") {
          spatial_indices <- which(matrix[, filtercolumn] <= value)
        }
      }
      if (length(merged_spatial_indices) == 0) {
        merged_spatial_indices <- c(merged_spatial_indices, spatial_indices)
      } else {
        merged_spatial_indices <- intersect(merged_spatial_indices, spatial_indices)
      }
    }
    spatial_indices <- unique(merged_spatial_indices)
  } else {
    spatial_indices <- NULL
  }
  if (length(spatial_indices) > 0) {
    matrix <- matrix[spatial_indices, ]
  }
  return(list("filtered_matrix" = matrix, "filtered_indices" = spatial_indices))
}
