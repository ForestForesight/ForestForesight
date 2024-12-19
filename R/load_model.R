#' Load and Validate XGBoost Model
#'
#' Loads an XGBoost model from a file or validates an existing model object.
#' If loading from a file, also attempts to load associated feature names from an RDA file.
#'
#' @param model Character string path to model file or xgb.Booster object
#'
#' @return An xgb.Booster object with optional feature names attribute
#'
#' @details
#' The function performs the following:
#' * For file paths: loads the model and attempts to load feature names from a companion .rda file
#' * For xgb.Booster objects: validates the object's integrity
#' * Handles both model formats while maintaining feature name consistency
#'
#' @examples
#' \dontrun{
#' # Load model from file
#' model <- load_model("path/to/model.model")
#'
#' # Validate existing model object
#' model <- load_model(existing_model)
#' }
#'
#' @seealso
#' * [xgboost::xgb.load()] for the underlying XGBoost loading function
#' * [ff_train()] for training new models
#'
#' @export
load_model <- function(model) {
  if (is.character(model)) {
    model_filename <- model
    if (!file.exists(model)) {
      stop("Model file does not exist")
    }
    if (!test_feature_model_match(model)) {
      stop("Number of features in model and corresponding feature names RDA file do not match")
    }

    model <- xgboost::xgb.load(model)
    rda_file <- gsub("\\.model", "\\.rda", model_filename)

    if (file.exists(rda_file)) {
      model_features <- get(load(rda_file))
      model$feature_names <- model_features
    }
  }
  return(model)
}

#' Test Feature and Model Match
#'
#' This function tests whether the provided feature names match the features in the given XGBoost model.
#' It checks if the model file and the corresponding feature names file exist, and if so, loads them.
#' It then attempts to generate the importance matrix to validate the match.
#'
#' @param model Either a character string representing the path to the XGBoost model file, or an `xgb.Booster` object
#' @param feature_names A character vector of feature names.
#' This parameter should be provided if `model` is an `xgb.Booster` object.
#' If `model` is a file path, the function will look for the `.rda` file with feature names in the same directory.
#'
#' @return A logical value: `TRUE` if the feature names match the features in the model, `FALSE` otherwise
#'
#' @noRd
test_feature_model_match <- function(model, feature_names = NULL) {
  if (is.character(model)) {
    if (!file.exists(model)) {
      stop("model file does not exist")
    }
    modelfile <- model
    if (!file.exists(gsub("\\.model", "\\.rda", model))) {
      stop("feature names were not found as RDA file in same folder as the model")
    } else {
      model <- xgboost::xgb.load(model)
      feature_names <- get(load(gsub("\\.model", "\\.rda", modelfile)))
    }
  } else {
    if (!has_value(feature_names)) {
      stop("feature names should be given if model is an xgb.Booster object")
    }
  }

  result <- tryCatch(
    {
      xgboost::xgb.importance(feature_names = feature_names, model = model)
      return(TRUE)
    },
    error = function(e) {
      # Print the error message
      return(FALSE)
    }
  )

  return(result)
}
