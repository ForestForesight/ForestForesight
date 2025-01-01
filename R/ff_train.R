#' Train an XGBoost Model for ForestForesight
#'
#' This function trains an XGBoost model with optimized default parameters derived from worldwide
#' data analysis. It supports both training from scratch and fine-tuning existing models, with
#' optional validation data for early stopping.
#'
#' @param train_matrix A list containing 'features' (matrix or data.frame) and 'label' (vector),
#'        or an xgb.DMatrix object. The training data for the model.
#' @param validation_matrix Optional; similar structure as train_matrix. If provided, used for
#'        validation during training and early stopping.
#' @param nrounds Integer; maximum number of boosting rounds. Default is 200.
#' @param eta Numeric; learning rate (between 0 and 1). Default is 0.1.
#' @param max_depth Integer; maximum depth of trees. Default is 5.
#' @param subsample Numeric; subsample ratio of training instances (0 to 1). Default is 0.75.
#' @param eval_metric Character; metric used for evaluation. Default is "aucpr"
#'        (Area Under the Precision-Recall Curve).
#' @param early_stopping_rounds Integer; training stops if performance doesn't improve
#'        for this many rounds. Default is 10.
#' @param gamma Numeric; minimum loss reduction required to make a partition. Default is NULL.
#' @param maximize Logical; whether to maximize the evaluation metric. Default is NULL.
#' @param min_child_weight Numeric; minimum sum of instance weight needed in a child. Default is 1.
#' @param verbose Logical; whether to print training progress. Default is FALSE.
#' @param xgb_model Optional; previously trained model to continue training from. Default is NULL.
#' @param modelfilename Optional; path to save the trained model. Default is NULL.
#' @param objective Character; learning objective for XGBoost. Default is "binary:logistic".
#'
#' @return A trained XGBoost model (xgb.Booster object). If modelfilename is provided,
#'         the model is also saved to disk.
#'
#' @details
#' The function implements several best practices for deforestation prediction:
#' * Uses AUCPR as default metric due to class imbalance in deforestation data
#' * Implements early stopping to prevent overfitting
#' * Saves both model and feature names when a filename is provided
#' * Supports continuing training from a previous model state
#'
#' @examples
#' \dontrun{
#' # Basic training
#' model <- ff_train(
#'   train_matrix = list(features = feature_matrix, label = labels),
#'   nrounds = 100
#' )
#'
#' # Training with validation and model saving
#' model <- ff_train(
#'   train_matrix = training_data,
#'   validation_matrix = validation_data,
#'   modelfilename = "deforestation_model.model",
#'   verbose = TRUE
#' )
#' }
#'
#' @seealso
#' * [xgboost::xgb.train()] for underlying XGBoost training function
#' * [ff_predict()] for making predictions with the trained model
#'
#' @import xgboost
#' @export
ff_train <- function(train_matrix,
                     validation_matrix = NULL,
                     nrounds = 200,
                     eta = 0.1,
                     max_depth = 5,
                     subsample = 0.75,
                     eval_metric = "aucpr",
                     early_stopping_rounds = 10,
                     gamma = NULL,
                     maximize = NULL,
                     min_child_weight = 1,
                     verbose = FALSE,
                     xgb_model = NULL,
                     modelfilename = NULL,
                     objective = "binary:logistic") {

  ff_train_input_check(train_matrix, validation_matrix, nrounds,
                       eta, max_depth, subsample,
                       eval_metric, early_stopping_rounds,
                       gamma, maximize, min_child_weight,
                       verbose, xgb_model, modelfilename,
                       objective)
  # Validate inputs
  validate_inputs(train_matrix)

  # Convert matrices to DMatrix format
  train_dataset <- convert_to_dmatrix(train_matrix)
  watchlist <- create_watchlist(train_dataset, validation_matrix)

  # Set up training parameters
  params <- create_params(
    objective = objective,
    eval_metric = eval_metric,
    eta = eta,
    max_depth = max_depth,
    subsample = subsample,
    gamma = gamma,
    min_child_weight = min_child_weight
  )

  # Train model
  if ("eval" %in% names(watchlist)) {
    ff_cat("Starting training with validation matrix", verbose = verbose)
  } else {
    ff_cat("Starting training without validation matrix", verbose = verbose)
  }
  xgbmodel <- train_model(
    params = params,
    train_dataset = train_dataset,
    watchlist = watchlist,
    nrounds = nrounds,
    early_stopping_rounds = early_stopping_rounds,
    maximize = maximize,
    verbose = verbose,
    xgb_model = xgb_model
  )

  # Save model if filename provided
  if (!is.null(modelfilename)) {
    save_model(xgbmodel, modelfilename, verbose)
  }

  return(xgbmodel)
}

#' Convert input matrix to XGBoost DMatrix format
#' @param matrix Input matrix or DMatrix object
#' @return XGBoost DMatrix object
#' @noRd
convert_to_dmatrix <- function(matrix) {
  if (inherits(matrix, "xgb.DMatrix")) {
    return(matrix)
  } else {
    if (inherits(matrix, "list")) {
      return(xgboost::xgb.DMatrix(matrix$features, label = matrix$label))
    } else {
      stop("the input train_matrix is not of class list or xgb.DMatrix")
    }
  }
}

#' Create watchlist for model training
#' @param train_dataset Training DMatrix
#' @param validation_matrix Validation matrix or NULL
#' @return Named list of matrices for monitoring
#' @noRd
create_watchlist <- function(train_dataset, validation_matrix) {
  if (is.null(validation_matrix)) {
    return(list(train = train_dataset))
  }

  evaluation_dataset <- convert_to_dmatrix(validation_matrix)
  return(list(train = train_dataset, eval = evaluation_dataset))
}

#' Create parameter list for XGBoost training
#' @param ... Named parameters for XGBoost
#' @return List of parameters
#' @noRd
create_params <- function(...) {
  params <- list(...)

  # Remove NULL parameters
  params[!vapply(params, is.null, logical(1))]
}

#' Train XGBoost model
#' @param params Model parameters
#' @param dtrain Training data
#' @param ... Additional parameters passed to xgb.train
#' @return Trained XGBoost model
#' @noRd
train_model <- function(params, train_dataset, xgb_model = xgb_model, ...) {
  xgboost::xgb.train(
    params = params,
    data = train_dataset,
    ...
  )
}

#' Save trained model to file
#' @param model Trained XGBoost model
#' @param filename Output filename
#' @param verbose Whether to print progress messages
#' @noRd
save_model <- function(model, filename, verbose = FALSE) {
  ff_cat("Saving model to", filename, verbose = verbose)

  feature_names <- model$feature_names
  rda_filename <- gsub("\\.model$", ".rda", filename)

  tryCatch(
    {
      suppressWarnings({
        saved <- xgboost::xgb.save(model, filename)
        if (saved) {
          save(feature_names, file = rda_filename)
        } else {
          ff_cat("Warning: Failed to save model", color = "yellow")
        }
      })
    },
    error = function(e) {
      ff_cat("Error saving model:", e$message, color = "red")
    }
  )
}

#' Validate input data
#' @param train_matrix Training data
#' @return NULL invisibly
#' @noRd
validate_inputs <- function(train_matrix) {
  if (!has_value(train_matrix$label)) {
    stop(
      "The input data has no label.",
      "This likely means no ground truth was available for this date or area."
    )
  }
  invisible(NULL)
}


#' Run input parameter checks for ff_train
#'
#' @param train_matrix Training data list or xgb.DMatrix
#' @param validation_matrix Optional validation data
#' @param nrounds Maximum boosting rounds
#' @param eta Learning rate
#' @param max_depth Tree depth
#' @param subsample Instance subsample ratio
#' @param eval_metric Evaluation metric
#' @param early_stopping_rounds Early stopping threshold
#' @param gamma Minimum loss reduction
#' @param maximize Maximize metric flag
#' @param min_child_weight Minimum child weight
#' @param verbose Verbosity flag
#' @param xgb_model Previous model
#' @param modelfilename Save path
#' @param objective Learning objective
#' @noRd
ff_train_input_check <- function(train_matrix, validation_matrix, nrounds,
                                 eta, max_depth, subsample,
                                 eval_metric, early_stopping_rounds,
                                 gamma, maximize, min_child_weight,
                                 verbose, xgb_model, modelfilename,
                                 objective) {
  # Check train_matrix can be either list or xgb.DMatrix
  check_object_class(train_matrix, c("list","xgb.DMatrix"))
  # Check validation_matrix same as train_matrix
  check_object_class(validation_matrix, c("list","xgb.DMatrix"))
  check_object_class(nrounds, "numeric")
  check_object_class(eta, "numeric")
  check_object_class(max_depth, "numeric")
  check_object_class(subsample, "numeric")
  check_object_class(eval_metric, "character")
  check_object_class(early_stopping_rounds, "numeric")
  check_object_class(gamma, "numeric") ||
  check_object_class(maximize, "logical")
  check_object_class(min_child_weight, "numeric")
  check_object_class(verbose, "logical")
  check_object_class(xgb_model, c("xgb.Booster", "character"))
  check_object_class(modelfilename, "character")
  check_object_class(objective, "character")
}
