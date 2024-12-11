#' Colorized Cat Function with Enhanced Logging
#'
#' This function provides colorized output capabilities with structured logging
#' using the logging package.
#'
#' @param ... Arguments to be concatenated and printed.
#' @param color Character string specifying the color to apply to the text.
#'   Must be a valid crayon color (e.g., "red", "blue", "green").
#' @param sep Character string to separate the arguments when concatenating.
#' @param fill Logical. If TRUE, a newline is appended after the output.
#' @param labels Character vector of labels for the lines printed.
#' @param style Character string specifying additional style to apply.
#'   Must be a valid crayon style (e.g., "bold", "underline").
#' @param verbose Logical. Whether to print to console.
#' @param log_level Character. Logging level ("DEBUG", "INFO", "WARN", "ERROR"). Defaults to "INFO".
#' @param log_file Character. Path to the log file. If NULL, uses default logging configuration.
#' @param timestamp Logical. Whether to include timestamp in console output.
#' @param auto_newline Logical. Whether to automatically add newline.
#'
#' @return No return value, called for side effects.
#'
#' @examples
#' ff_cat("Hello, world!")
#' ff_cat("Error message", color = "red", log_level = "ERROR")
#' ff_cat("Debug info", color = "blue", log_level = "DEBUG")
#'
#' @import crayon
#' @import logging
#' @export
ff_cat <- function(...,
                   color = NULL,
                   sep = " ",
                   fill = FALSE,
                   labels = NULL,
                   style = NULL,
                   verbose = TRUE,
                   log_level = "INFO",
                   log_file = NULL,
                   timestamp = as.logical(Sys.getenv("TIMESTAMP")),
                   auto_newline = TRUE) {
  if (!has_value(timestamp)) {
    timestamp <- TRUE
  }
  logging_enabled <- as.logical(Sys.getenv("LOGGING"))
  if (logging_enabled) {
    if (!has_value(log_file)) {
      log_directory <- Sys.getenv("LOGFILE_FOLDER")
      if (has_value(log_directory)) {

      } else {
        logging_enabled <- FALSE
      }
      log_file <- file.path(log_directory, paste0("FF_", Sys.Date(), ".log"))
    } else {
      log_directory <- dirname(log_file)
    }
    if (!dir.exists(log_directory)) {
      dir.create(log_directory, recursive = TRUE)
    }
    setup_logging(log_file)
  }
  # Format the text
  text <- format_text(..., sep = sep, auto_newline = auto_newline)

  # Handle console output and logging
  handle_output(
    text = text,
    color = color,
    style = style,
    verbose = verbose,
    timestamp = timestamp,
    fill = fill,
    labels = labels,
    log_level = log_level,
    logging_enabled = logging_enabled
  )
}

#' Setup Logging Configuration
#'
#' Initializes the logging system with appropriate handlers and formatters.
#' Creates log directories if needed and configures logging format.
#'
#' @param log_file Character. Path to the log file.
#'
#' @return No return value, called for side effects.
#'
#' @noRd
setup_logging <- function(log_file) {
  # Get or create logger
  logging::removeHandler("basic.stdout", "")
  logger <- logging::getLogger("ForestForesight")
  logging::removeHandler("console", logger)
  # If logger doesn't exist or has no handlers, set it up
  if (!has_value(logging::getHandler("logging::writeToFile", "ForestForesight"))) {
    logger <- logging::getLogger("ForestForesight")
    logging::removeHandler("console", logger)
    # Configure file logging if log_file is specified
    if (!is.null(log_file)) {
      # Create log directory if it doesn't exist
      log_dir <- dirname(log_file)
      if (!dir.exists(log_dir)) {
        dir.create(log_dir, recursive = TRUE)
      }

      # Create custom layout function
      custom_layout <- function(record) {
        timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        sprintf("%s [%s] %s", timestamp, record$level, record$msg)
      }

      # Add new file handler with custom layout
      logging::addHandler(
        logging::writeToFile,
        file = log_file,
        logger = "ForestForesight",
        level = "DEBUG", # Allow all log levels for the handler
        layout = custom_layout
      )
    }
  }
}
#' Format Text for Output
#'
#' Combines and formats text with optional newline.
#'
#' @param ... Arguments to be concatenated.
#' @param sep Character string to separate the arguments.
#' @param auto_newline Logical. Whether to add newline.
#'
#' @return Character string of formatted text.
#'
#' @noRd
format_text <- function(..., sep = " ", auto_newline = TRUE) {
  text <- paste(..., sep = sep)
  if (auto_newline && !endsWith(text, "\n")) {
    text <- paste0(text, "\n")
  }
  text
}

#' Handle Output Operations
#'
#' Manages console output and logging operations with proper styling.
#'
#' @param text Character string to output.
#' @param color Character string specifying text color.
#' @param style Character string specifying text style.
#' @param verbose Logical. Whether to print to console.
#' @param timestamp Logical. Whether to include timestamp.
#' @param fill Logical. Whether to add newline after output.
#' @param labels Character vector of labels.
#' @param log_level Character string specifying logging level.
#'
#' @return No return value, called for side effects.
#'
#' @noRd
handle_output <- function(text,
                          color = NULL,
                          style = NULL,
                          verbose = TRUE,
                          timestamp = TRUE,
                          fill = FALSE,
                          labels = NULL,
                          log_level = "INFO",
                          logging_enabled = TRUE) {
  # Create console output text
  console_text <- if (timestamp) {
    paste(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), text)
  } else {
    text
  }
  # Apply color if valid
  if (!is.null(color)) {
    if (color %in% names(crayon:::builtin_styles)) {
      console_text <- do.call(color, list(console_text))
    } else {
      warning("Invalid color specified. Using default.")
    }
  }
  # Apply style if valid
  if (!is.null(style)) {
    if (style %in% names(crayon:::builtin_styles)) {
      console_text <- do.call(style, list(console_text))
    } else {
      warning("Invalid style specified. Ignoring.")
    }
  }

  # Console output
  if (verbose) {
    cat(console_text, sep = "", fill = fill, labels = labels)
  }
  if (logging_enabled) {
    # Logging
    log_func <- switch(toupper(log_level),
      "DEBUG" = logging::logdebug,
      "INFO"  = logging::loginfo,
      "WARN"  = logging::logwarn,
      "ERROR" = logging::logerror,
      logging::loginfo
    )

    # Remove ANSI color codes for log file
    clean_text <- gsub("\033\\[[0-9;]*m", "", text)
    log_func(clean_text, logger = "ForestForesight")
  }
}
