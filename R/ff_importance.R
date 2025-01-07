#' Extract Feature Importance from ForestForesight Model
#'
#' This function loads a ForestForesight model (.model file) and its corresponding
#' feature names (.rda file), extracts feature importance, and saves the results to a CSV file.
#'
#' @param model xgb.model class object or Character string for Path to the .model file.
#' @param output_csv Character string. Path to the output CSV file.
#' @param name Character string. Name to be given to the model if the model is an xgb.model
#' @param append Logical. If TRUE, append to existing CSV file. If FALSE, overwrite. Default is TRUE
#'
#' @return Invisibly returns the importance dataframe.
#'
#' @details
#' The function expects a .model file and a corresponding .rda file in the same directory
#' with the same name (different extension). The .rda file should contain the feature names.
#'
#' @import xgboost
#' @importFrom utils write.table
#'
#' @references
#' Jonas van Duijvenbode (2023)
#' Zillah Calle (2023)
#'
#' @examples
#' \dontrun{
#' ff_importance("path/to/model.model", "output_importance.csv")
#' ff_importance("path/to/another_model.model", "output_importance.csv", append = TRUE)
#' }
#'
#' @export
ff_importance <- function(model, output_csv = NULL, name = NA, append = TRUE) {
  ff_importance_input_check(model, output_csv, name, append)
  if (!has_value(name)) {
    if (is.character(model)) {
      name <- sub("\\.model$", "", basename(model))
    if(!has_value(name)){
      name <- NA
    }
      } else {
      name <- NA
    }
  }
  model <- load_model(model)
  # Get importance
  importance_matrix <- xgboost::xgb.importance(model = model)
  # create importance dataset structure

  importance_dataframe <- data.frame(
    model_name = rep(name, nrow(importance_matrix)),
    feature = importance_matrix$Feature,
    rank = seq_len(nrow(importance_matrix)),
    importance = importance_matrix$Gain
  )

  # Write to CSV
  if (has_value(output_csv)) {
    if (!file.exists(output_csv)) {
      append <- FALSE
    }
    write.table(importance_dataframe,
      file = output_csv, sep = ",", row.names = FALSE,
      col.names = !append, append = append
    )
  }
  # Return dataframe invisibly
  invisible(importance_dataframe)
}

#' Run input parameter checks for ff_importance
#'
#' @param model_path Model object or path
#' @param output_csv CSV output path
#' @param name Model name
#' @param append Append flag
#' @noRd
ff_importance_input_check <- function(model, output_csv, name, append) {
  # Model can be either xgb.model or character path
  check_object_class(model, c("xgb.Booster", "character"))
  check_object_class(output_csv, "character")
  check_object_class(name, "character")
  check_object_class(append, "logical")
}
