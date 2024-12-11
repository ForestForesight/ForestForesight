#' Make Predictions Using a ForestForesight Model
#'
#' This function makes predictions using a trained ForestForesight model and optionally
#' evaluates the model performance if ground truth data is provided.
#'
#' @param model An xgb.Booster object or path to a saved xgboost model file. Comes out of ff_train
#' @param test_matrix A list containing 'features' matrix for predictions. Comes out of ff_prep
#' @param thresholds Numeric vector of classification thresholds. Default is 0.5.
#' @param groundtruth Optional vector or SpatRaster of actual values for model evaluation.
#' @param indices Optional vector of indices for filling a raster. Default is NA.
#' @param templateraster Optional SpatRaster template for spatial predictions. Default is NA.
#' @param verbose Boolean indicating whether to display progress messages. Default is FALSE.
#' @param certainty Boolean indicating whether to return raw probabilities instead of binary predictions.
#' Default is FALSE.
#'
#' @return A list containing:
#'   \item{threshold}{The threshold(s) used for classification}
#'   \item{precision_vector}{Precision values for each threshold}
#'   \item{recall_vector}{Recall values for each threshold}
#'   \item{F0.5}{F0.5 scores for each threshold}
#'   \item{predicted_raster}{SpatRaster of predictions if templateraster was provided}
#'   \item{predictions}{Raw prediction values}
#'
#' @examples
#' \dontrun{
#' # Load a saved model and make predictions
#' predictions <- ff_predict(
#'   model = "forest_model.model",
#'   test_matrix = test_data,
#'   thresholds = c(0.3, 0.5, 0.7)
#' )
#'
#' # Make predictions with spatial output
#' predictions <- ff_predict(
#'   model = model_object,
#'   test_matrix = test_data,
#'   templateraster = my_raster,
#'   certainty = TRUE
#' )
#' }
#'
#' @import xgboost terra
#' @export

ff_predict <- function(model, test_matrix, thresholds = as.numeric(Sys.getenv("DEFAULT_THRESHOLD")),
                       groundtruth = NA, indices = NA,
                       templateraster = NA, verbose = FALSE, certainty = FALSE) {
  # Load and validate model
  loaded_model <- load_model(model)

  # Handle feature matching

  test_matrix <- remove_extra_features(test_matrix, loaded_model)
  # Convert to xgb_matrix
  if (has_value(test_matrix$label)) {
    xgb_matrix <- xgboost::xgb.DMatrix(test_matrix$features, label = test_matrix$label)
  } else {
    xgb_matrix <- xgboost::xgb.DMatrix(test_matrix$features)
  }

  ff_cat("calculating predictions", verbose = verbose)
  predictions <- stats::predict(loaded_model, xgb_matrix)
  metrics <- calculate_metrics(predictions, groundtruth, thresholds, verbose)


  # Handle spatial predictions
  if (inherits(templateraster, "SpatRaster")) {
    predicted_raster <- fill_raster(templateraster, predictions, indices, certainty, thresholds, verbose)
  } else {
    predicted_raster <- NA
  }

  if (has_value(metrics$accuracy_f05)) {
    ff_cat("F0.5:", metrics$accuracy_f05,
      "precision:", metrics$precision,
      "recall:", metrics$recall,
      verbose = verbose
    )
  }

  return(list(
    threshold = thresholds,
    precision_vector = metrics$precision,
    recall_vector = metrics$recall,
    "F0.5" = metrics$accuracy_f05,
    predicted_raster = predicted_raster,
    predictions = predictions
  ))
}



#' Remove Extra Features from Test Matrix
#'
#' Removes features from the test matrix that are not present in the model's feature names.
#' If extra features are found, they are removed and a warning message is displayed.
#'
#' @param test_matrix A list containing a 'features' matrix for predictions
#' @param loaded_model An xgb.Booster object with optional feature_names attribute
#'
#' @return The modified test_matrix with extra features removed
#'
#' @noRd
remove_extra_features <- function(test_matrix, loaded_model) {
  if (has_value(loaded_model$feature_names)) {
    test_features <- colnames(test_matrix$features)
    extra_features <- setdiff(test_features, loaded_model$feature_names)

    if (length(extra_features) > 0) {
      ff_cat(
        "Removing extra features from the test matrix:",
        paste(extra_features, collapse = ", "),
        color = "yellow", log_level = "WARNING"
      )
      test_matrix$features <- test_matrix$features[, setdiff(test_features, extra_features),
        drop = FALSE
      ]
    }
  }
  return(test_matrix)
}

#' Calculate Performance Metrics
#'
#' Calculates precision, recall, and F0.5 score for model predictions against ground truth data.
#'
#' @param predictions Numeric vector of model predictions
#' @param groundtruth Vector or SpatRaster of actual values
#' @param thresholds Numeric vector of classification thresholds
#'
#' @return List containing precision, recall, and F0.5 scores for each threshold
#'
#' @noRd
calculate_metrics <- function(predictions, groundtruth, thresholds, verbose) {
  if (!has_value(groundtruth)) {
    ff_cat("no groundtruth found, returning NA for precision, recall and F0.5")
    return(list(precision = NA, recall = NA, accuracy_f05 = NA))
  }
  ff_cat("calculating scores", verbose = verbose)
  if (inherits(groundtruth, "SpatRaster")) {
    groundtruth <- as.numeric(as.matrix(groundtruth))
  }

  precision_vector <- recall_vector <- accuracy_f05 <- numeric(length(thresholds))

  for (i in seq_along(thresholds)) {
    threshold <- thresholds[i]
    crosstable <- table(2 * (predictions > threshold) + groundtruth)
    precision_vector[i] <- as.numeric(crosstable[4] / (crosstable[4] + crosstable[3]))
    recall_vector[i] <- as.numeric(crosstable[4] / (crosstable[4] + crosstable[2]))
    accuracy_f05[i] <- 1.25 * precision_vector[i] * recall_vector[i] /
      (0.25 * precision_vector[i] + recall_vector[i])
  }

  return(list(
    precision = precision_vector,
    recall = recall_vector,
    accuracy_f05 = accuracy_f05
  ))
}

#' Fill Raster with Predictions
#'
#' Creates a spatial raster from model predictions using a template raster
#'
#' @param templateraster SpatRaster template for spatial predictions
#' @param predictions Numeric vector of model predictions
#' @param indices Optional vector of indices for filling specific raster cells
#' @param certainty Boolean indicating whether to return probabilities or binary predictions
#' @param thresholds Numeric vector of classification thresholds
#' @param verbose Boolean indicating whether to display progress messages
#'
#' @return SpatRaster containing predictions or NA if dimensions don't match
#'
#' @noRd
fill_raster <- function(templateraster, predictions, indices, certainty, thresholds, verbose) {
  filed_raster <- templateraster
  filed_raster[] <- 0

  if (length(indices) > 1) {
    ff_cat("filling raster", verbose = verbose)
    filed_raster[indices] <- if (!certainty) {
      predictions > thresholds
    } else {
      predictions
    }
    return(filed_raster)
  } else if (terra::ncell(templateraster) == length(predictions)) {
    ff_cat("filling raster", verbose = verbose)
    filed_raster[] <- if (!certainty) {
      predictions > thresholds
    } else {
      predictions
    }
    return(filed_raster)
  } else {
    return(NA)
  }
}
