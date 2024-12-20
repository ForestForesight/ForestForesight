#' Generate Forest Foresight Accuracy and Importance Analysis Plots
#'
#' @description
#' Creates visualization plots for Forest Foresight model accuracy and feature importance analysis.
#' Can handle both file paths and data frames as input.
#'
#' @param accuracy_data List of file paths or a single data frame containing accuracy metrics
#' @param importance_data List of file paths or a single data frame containing importance metrics
#' @param output_path Character string specifying the output file path (optional)
#' @param title Character string for the plot title
#' @param return_plot Logical indicating whether to return a plotting object (default: FALSE)
#'
#' @return If return_plot is TRUE, returns a list containing the plot objects
#'
#' @import terra
#' @import graphics
#' @import stats
#' @import utils
#'
#' @examples
#' \dontrun{
#' # Using file paths
#' ff_accuracyreport(
#'   accuracy_paths = c("path/to/accuracy1.csv", "path/to/accuracy2.csv"),
#'   importance_paths = c("path/to/importance1.csv"),
#'   output_path = "output/report.png",
#'   title = "Forest Foresight Analysis"
#' )
#'
#' # Using data frames
#' ff_accuracyreport(
#'   accuracy_data = accuracy_df,
#'   importance_data = importance_df,
#'   return_plot = TRUE
#' )
#' }
#' @export
ff_accuracyreport <- function(accuracy_data = NULL,
                              importance_data = NULL,
                              output_path = NULL,
                              title = "Accuracy Analysis: Forest Foresight",
                              return_plot = FALSE) {

  # Load and prepare data
  results <- load_accuracy_data(accuracy_data)
  importance_results <- load_importance_data(importance_data)

  # Prepare spatial data
  spatial_data <- prepare_spatial_data(results)

  # Calculate metrics by date
  results_by_date <- calculate_metrics_by_date(results)

  # Initialize plotting device if output path is provided
  if (!is.null(output_path)) {
    output_path <- sub("\\.pdf$", ".png", output_path)
    png(output_path, width = 16.5, height = 11.7, units = "in", res = 300)
  }

  # Set up layout
  setup_plot_layout(importance_results)

  # Create plots
  plots <- list()
  plots$map <- create_f05_map(spatial_data)
  plots$metrics <- create_metrics_plot(results_by_date)

  if (!is.null(importance_results)) {
    plots$importance <- create_importance_plot(importance_results)
  }

  # Add title
  mtext(title, outer = TRUE, line = -2, cex = 1.5)

  # Close device if output path was provided
  if (!is.null(output_path)) {
    dev.off()
  }

  # Return plots if requested
  if (return_plot) {
    return(plots)
  }
}

#' Load and Process Accuracy Data
#'
#' @param accuracy_data List of file paths or data frame
#' @return Data frame of processed accuracy data
#' @keywords internal
#' Load and Process Accuracy Data
#'
#' @param accuracy_data List of file paths or data frame
#' @return Data frame of processed accuracy data
#' @keywords internal
#' @noRd
load_accuracy_data <- function(accuracy_data) {
  if (is.null(accuracy_data)) {
    stop("Accuracy data must be provided")
  }

  if (is.data.frame(accuracy_data)) {
    results <- accuracy_data
  } else if (is.character(accuracy_data)) {
    results <- do.call(rbind, lapply(accuracy_data, read.csv))
  } else {
    stop("accuracy_data must be either a data frame or vector of file paths")
  }

  # Only create UUID if it doesn't exist
  if (!"UUID" %in% names(results)) {
    # Check if required columns exist
    if (!all(c("iso3", "coordname") %in% names(results))) {
      missing_cols <- setdiff(c("iso3", "coordname"), names(results))
      stop(sprintf(
        "Cannot create UUID column. Missing required columns: %s. Either provide these columns or include a pre-computed UUID column.",
        paste(missing_cols, collapse = ", ")
      ))
    }
    results$UUID <- paste0(results$iso3, "_", results$coordname)
    warning("UUID column created from iso3 and coordname columns")
  }

  # Rename 'name' to 'country' if it exists
  if ("name" %in% names(results)) {
    names(results)[which(names(results) == "name")] <- "country"
  }

  return(results)
}

#' Load and Process Importance Data
#'
#' @param importance_data List of file paths or data frame
#' @return Data frame of processed importance data
#' @keywords internal
#' @noRd
load_importance_data <- function(importance_data) {
  if (is.null(importance_data)) {
    return(NULL)
  }

  if (is.data.frame(importance_data)) {
    importance_results <- importance_data
  } else if (is.character(importance_data)) {
    importance_results <- do.call(rbind, lapply(importance_data, read.csv))
  } else {
    stop("importance_data must be either a data frame or vector of file paths")
  }

  return(importance_results)
}

#' Calculate Accuracy Metrics
#'
#' @param true_positives Numeric value of true positives
#' @param false_positives Numeric value of false positives
#' @param true_negatives Numeric value of true negatives
#' @param false_negatives Numeric value of false negatives
#' @return Vector of calculated metrics
#' @keywords internal
#' @noRd
calculate_metrics <- function(true_positives, false_positives, true_negatives, false_negatives) {
  precision <- true_positives / (true_positives + false_positives)
  recall <- true_positives / (true_positives + false_negatives)
  f0_5_score <- (1.25 * precision * recall) / (0.25 * precision + recall)
  events <- true_positives + false_negatives
  return(c(precision = precision, recall = recall, F0.5 = f0_5_score, events = events))
}

#' Prepare Spatial Data for Plotting
#'
#' @param results Data frame of accuracy results
#' @return Spatial vector object with metrics
#' @keywords internal
#' @noRd
prepare_spatial_data <- function(results) {
  pols <- terra::vect(get(data("degree_polygons", envir = environment())))
  pols$UUID <- paste0(pols$iso3, "_", pols$coordname)

  results_by_uuid <- aggregate(
    cbind(true_positives, false_positives, true_negatives, false_negatives) ~ UUID,
    data = results, FUN = sum
  )

  metrics_by_uuid <- t(apply(
    results_by_uuid[, c("true_positives", "false_positives", "true_negatives", "false_negatives")],
    1,
    function(row) calculate_metrics(row[1], row[2], row[3], row[4])
  ))

  results_by_uuid <- cbind(results_by_uuid, metrics_by_uuid)
  spatialdata <- merge(pols, results_by_uuid, by = "UUID")
  names(spatialdata)[12:15] <- c("precision", "recall", "F05", "events")
  spatialdata$F05 <- as.numeric(spatialdata$F05)
  spatialdata <- spatialdata[!is.nan(spatialdata$F05), ]

  return(spatialdata)
}

#' Calculate Metrics by Date
#'
#' @param results Data frame of accuracy results
#' @return Data frame of metrics aggregated by date
#' @keywords internal
#' @noRd
calculate_metrics_by_date <- function(results) {
  results_by_date <- aggregate(
    cbind(true_positives, false_positives, true_negatives, false_negatives) ~ date,
    data = results, FUN = sum
  )

  metrics_by_date <- as.data.frame(t(apply(
    results_by_date[, c("true_positives", "false_positives", "true_negatives", "false_negatives")],
    1,
    function(row) calculate_metrics(row[1], row[2], row[3], row[4])
  )))

  results_by_date <- cbind(results_by_date, metrics_by_date)
  results_by_date$date <- as.Date(results_by_date$date)

  return(results_by_date)
}

#' Set Up Plot Layout
#'
#' @param importance_results Data frame of importance results
#' @keywords internal
#' @noRd
setup_plot_layout <- function(importance_results) {
  if (!is.null(importance_results)) {
    layout(matrix(c(1, 2, 3, 3), nrow = 2, ncol = 2, byrow = TRUE))
  } else {
    layout(matrix(c(1, 2), nrow = 1, ncol = 2, byrow = TRUE))
  }
}

#' Create F0.5 Score Distribution Map
#'
#' @param spatialdata Spatial vector object with metrics
#' @return Plot object
#' @keywords internal
#' @noRd
create_f05_map <- function(spatialdata) {
  par(mar = c(5, 4, 4, 2) + 0.1)
  col_palette <- colorRampPalette(c("red", "yellow", "green"))(100)
  maxf05 <- max(spatialdata$F05, na.rm = TRUE) + 0.05
  minf05 <- min(spatialdata$F05, na.rm = TRUE) - 0.05
  breaks <- seq(minf05, maxf05, length.out = 10)

  plot(spatialdata, "F05",
       main = "F0.5 Score Distribution",
       col = col_palette,
       border = "#00000000",
       breaks = breaks,
       legend = TRUE)
}

#' Create Metrics Over Time Plot
#'
#' @param results_by_date Data frame of metrics by date
#' @return Plot object
#' @keywords internal
#' @noRd
create_metrics_plot <- function(results_by_date) {
  par(mar = c(7, 5, 4, 5) + 0.1)
  max_events <- max(results_by_date$events)
  y_breaks <- pretty(c(0, max_events), n = 10)

  plot(results_by_date$date, results_by_date$events,
       type = "h", col = "lightgrey", lwd = 10,
       xlab = "", ylab = "",
       main = "Precision, Recall, and F0.5 Over Time",
       ylim = c(0, max(y_breaks)),
       axes = FALSE)

  # Add axes and formatting
  format_metrics_plot(results_by_date, max_events, y_breaks)

  return(invisible(NULL))
}

#' Format Metrics Plot
#'
#' @param results_by_date Data frame of metrics by date
#' @param max_events Maximum number of events
#' @param y_breaks Y-axis break points
#' @keywords internal
#' @noRd
format_metrics_plot <- function(results_by_date, max_events, y_breaks) {
  axis(2, at = y_breaks, labels = format(y_breaks, scientific = FALSE, big.mark = ","), las = 1)
  mtext("Number of Events", side = 2, line = 3.5, bg = "white")

  # Format x-axis
  axis(1, at = results_by_date$date, labels = FALSE)
  text(
    x = results_by_date$date,
    y = par("usr")[3] - 0.05 * (par("usr")[4] - par("usr")[3]),
    labels = format(results_by_date$date, "%Y-%m-%d"),
    srt = 45, adj = 1, xpd = TRUE, cex = 0.7
  )

  # Add metrics lines and points
  add_metrics_lines(results_by_date, max_events)
}

#' Add Metrics Lines to Plot
#'
#' @param results_by_date Data frame of metrics by date
#' @param max_events Maximum number of events
#' @keywords internal
#' @noRd
add_metrics_lines <- function(results_by_date, max_events) {
  scalfac <- max_events

  # Add lines
  lines(results_by_date$date, results_by_date$precision * scalfac, col = "blue", lwd = 2)
  lines(results_by_date$date, results_by_date$recall * scalfac, col = "red", lwd = 2)
  lines(results_by_date$date, results_by_date$F0.5 * scalfac, col = "green", lwd = 2)

  # Add points
  points(results_by_date$date, results_by_date$precision * scalfac, col = "blue", pch = 16)
  points(results_by_date$date, results_by_date$recall * scalfac, col = "red", pch = 16)
  points(results_by_date$date, results_by_date$F0.5 * scalfac, col = "green", pch = 16)

  # Add legend
  legend("topright",
         legend = c("Precision", "Recall", "F0.5", "Events"),
         col = c("blue", "red", "green", "lightgrey"),
         lty = c(1, 1, 1, 1), lwd = c(2, 2, 2, 10),
         pch = c(16, 16, 16, NA))
}

#' Create Feature Importance Plot
#'
#' @param importance_results Data frame of importance results
#' @return Plot object
#' @keywords internal
#' @noRd
create_importance_plot <- function(importance_results) {
  # Calculate average importance if multiple models
  avg_importance <- prepare_importance_data(importance_results)

  # Create plot
  par(mar = c(5, 15, 4, 2))
  plot_importance_data(avg_importance)

  return(invisible(NULL))
}

#' Prepare Importance Data for Plotting
#'
#' @param importance_results Data frame of importance results
#' @return Data frame of processed importance data
#' @keywords internal
prepare_importance_data <- function(importance_results) {
  model_names <- paste(unique(importance_results$model_name), collapse = ", ")
  avg_importance <- aggregate(importance ~ feature, data = importance_results, FUN = mean)
  avg_importance$rank <- rank(-avg_importance$importance, ties.method = "first")
  avg_importance <- avg_importance[order(avg_importance$rank), ]

  data.frame(
    model_name = model_names,
    feature = avg_importance$feature,
    rank = avg_importance$rank,
    importance = avg_importance$importance,
    percentage = avg_importance$importance * 100
  )
}

#' Plot Importance Data
#'
#' @param importance_data Processed importance data
#' @keywords internal
#' @noRd
plot_importance_data <- function(importance_data) {
  plot(importance_data$importance,
       1:nrow(importance_data),
       type = "n",
       log = "x",
       xlim = c(min(importance_data$importance) / 2, max(importance_data$importance) * 1.2),
       ylim = c(0, nrow(importance_data) + 1),
       xlab = "Importance (log scale)",
       ylab = "",
       yaxt = "n",
       main = importance_data$model_name[1],
       cex.main = 1.5,
       cex.lab = 1.2)

  # Add horizontal bars
  barplot_height <- 0.8
  for (i in 1:nrow(importance_data)) {
    rect(min(importance_data$importance) / 2,
         i - barplot_height / 2,
         importance_data$importance[i],
         i + barplot_height / 2,
         col = "lightgreen",
         border = NA)
  }

  # Add feature names on the left side
  text(min(importance_data$importance) / 2,
       1:nrow(importance_data),
       labels = importance_data$feature,
       pos = 2,
       xpd = TRUE,
       cex = 0.7)

  # Add percentage values on the right side of bars
  text(importance_data$importance,
       1:nrow(importance_data),
       labels = sprintf("%.2f%%", importance_data$percentage),
       pos = 4,
       cex = 0.7)

  # Add gridlines
  grid(nx = NULL,
       ny = NA,
       lty = 2,
       col = "gray")
}
