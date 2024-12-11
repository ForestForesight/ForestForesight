#' Set up ForestForesight environment configuration
#'
#' @description
#' This function guides users through the setup process of ForestForesight configuration
#' parameters. It interactively prompts for each parameter, validates inputs, and saves
#' the configuration to a user-specific location. If a configuration already exists,
#' it will ask for confirmation before overwriting.
#'
#' @details
#' The function handles several types of configuration parameters:
#' * Date parameters (must be first of month, not before 2021-01-01)
#' * Groundtruth patterns (1m, 3m, 6m, or 12m)
#' * Forest mask filters (operator followed by number)
#' * General text and path parameters
#'
#' If LOGGING is set to FALSE, the LOGFILE_FOLDER parameter will be skipped.
#'
#' Configuration is stored in:
#' * Windows: %APPDATA%/forestforesight/config.yml
#' * Unix/Mac: ~/.forestforesight/config.yml
#'
#' @param config_file_path Character string. Path to a custom yml configuration template.
#'                        If empty, uses the default template from the package.
#'
#' @return Invisible TRUE if configuration completes successfully, invisible FALSE if
#'         cancelled by user.
#'
#' @examples
#' \dontrun{
#' # Use default package configuration template
#' ff_environment()
#'
#' # Use custom configuration template
#' ff_environment("path/to/my/config.yml")
#' }
#'
#' @section Validation Rules:
#' * EARLIEST_DATA_DATE: Must be YYYY-MM-01 format, not before 2021-01-01
#' * DEFAULT_GROUNDTRUTH: Must be one of: groundtruth1m, groundtruth3m, groundtruth6m, groundtruth12m
#' * FOREST_MASK_FILTER: Must be operator (>, <, >=, <=, =, ==) followed by number
#'
#' @seealso
#' * [yaml::read_yaml()] for the underlying YAML parsing
#' * [get_config_dir()] for configuration directory location
#'
#' @export
ff_environment <- function(config_file_path = "") {
  env_file <- system.file("env.yml", package = "ForestForesight")
  if (config_file_path != "") {
    env_file <- config_file_path
  }


  # Get path for user configuration
  config_path <- file.path(get_config_dir(), "config.yml")

  # If configuration exists and is initiated, confirm before proceeding
  if (file.exists(config_path)) {
    response <- readline("Configuration already exists. Do you want to reconfigure? (y/N): ")
    if (tolower(response) != "y") {
      message("Setup cancelled. Using existing configuration.")
      return(invisible(FALSE))
    }
    env_file <- config_path
  }
  # Read template configuration
  config <- yaml::read_yaml(env_file)

  cat("Welcome to ForestForesight! Let's set up your environment.\n\n")

  # Process each parameter
  logging <- TRUE
  for (param in names(config)) {
    # Skip internal parameters
    if (param == "TEST") next
    if (!logging && param == "LOGFILE_FOLDER") next
    # Get current value and description
    current_value <- config[[param]]
    description <- parameter_descriptions[[param]] %||% "No description available"

    # Show prompt with description and current value
    cat(sprintf(
      "\n%s:\n%s\nCurrent value: %s\n",
      param, description,
      if (is.null(current_value)) "Not set" else current_value
    ))

    # Get user input
    new_value <- readline("Enter new value (press Enter to keep current): ")
    if (param == "LOGGING" && new_value == "FALSE") {
      logging <- FALSE
    }
    # If user provided input, validate and update
    if (nchar(new_value) > 0) {
      # Check if parameter has a validator
      if (!is.null(parameter_validators[[param]])) {
        while (!parameter_validators[[param]](new_value)) {
          cat("Invalid input. Please try again.\n")
          new_value <- readline("Enter new value (press Enter to keep current): ")
          if (nchar(new_value) == 0) break
        }
      }

      if (nchar(new_value) > 0) {
        config[[param]] <- new_value
      }
    }
  }

  # Set initiated flag
  # Save configuration
  yaml::write_yaml(config, config_path)

  cat(sprintf("\nConfiguration saved to: %s\n", config_path))
  cat("You can modify these settings anytime by editing this file.\n")

  invisible(TRUE)
}

#' Get configuration directory path
#' @return String path to configuration directory
#' @noRd
get_config_dir <- function() {
  if (.Platform$OS.type == "windows") {
    path <- file.path(Sys.getenv("APPDATA"), "forestforesight")
  } else {
    path <- file.path(Sys.getenv("HOME"), ".forestforesight")
  }
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)
  path
}

#' Parameter validation functions
#' @noRd
parameter_validators <- list(
  EARLIEST_DATA_DATE = function(value) {
    tryCatch(
      {
        date <- as.Date(value)
        min_date <- as.Date("2021-01-01")

        # Check if date is before minimum allowed date
        if (date < min_date) {
          message("Warning: Date is before 2021-01-01. This might affect data availability.")
        }

        # Check if date is first of month
        if (format(date, "%d") != "01") {
          message("Warning: Date must be the first day of the month.")
          return(FALSE)
        }

        TRUE
      },
      error = function(e) {
        message("Invalid date format. Please use YYYY-MM-01 format.")
        FALSE
      }
    )
  },
  DEFAULT_GROUNDTRUTH = function(value) {
    valid_patterns <- c("groundtruth1m", "groundtruth3m", "groundtruth6m", "groundtruth12m")
    if (!value %in% valid_patterns) {
      message(sprintf(
        "Warning: groundtruth pattern should be one of: %s",
        paste(valid_patterns, collapse = ", ")
      ))
      FALSE
    } else {
      TRUE
    }
  },
  FF_FOLDER = function(value) {
    if (!dir.exists(value)) {
      message(sprintf(
        "Warning: given folder does not exist, will try to create: %s",
        value
      ))
      dir.create(value)
      if (dir.exists(value)) {
        TRUE
      } else {
        FALSE
      }
    } else {
      TRUE
    }
  },
  LOGFILE_FOLDER = function(value) {
    if (!dir.exists(value)) {
      message(sprintf(
        "Warning: given folder does not exist, will try to create: %s",
        value
      ))
      dir.create(value)
      if (dir.exists(value)) {
        TRUE
      } else {
        FALSE
      }
    } else {
      TRUE
    }
  },
  TIMESTAMP = function(value) {
    valid_patterns <- c("TRUE", "FALSE")
    if (!value %in% valid_patterns) {
      message(sprintf(
        "Warning: TIMESTAMP should be one of: %s",
        paste(valid_patterns, collapse = ", ")
      ))
      FALSE
    } else {
      TRUE
    }
  },
  LOGGING = function(value) {
    valid_patterns <- c("TRUE", "FALSE")
    if (!value %in% valid_patterns) {
      message(sprintf(
        "Warning: LOGGING should be one of: %s",
        paste(valid_patterns, collapse = ", ")
      ))
      FALSE
    } else {
      TRUE
    }
  },
  DEFAULT_THRESHOLD = function(value) {
    if (!(value > 0 && value < 1)) {
      message("DEFAULT_THRESHOLD should be between 0 and 1 (and favorably around 0.5")
      FALSE
    } else {
      TRUE
    }
  },
  DEFAULT_COUNTRY = function(value) {
    if (value == "") {
      return(TRUE)
    }
    valid_countries <- get(data(countries, envir = environment()))$iso3
    if (!(value %in% valid_countries)) {
      message(sprintf(
        "Warning: COUNTRY should be one of: %s",
        paste(valid_countries, collapse = ", ")
      ))
      FALSE
    } else {
      TRUE
    }
  },
  FOREST_MASK_FILTER = function(value) {
    # Regular expression to match operator followed by number
    # Matches >, <, >=, <=, = or == followed by an optional space and a number
    # The number can be negative and can have decimals
    pattern <- "^(>|<|>=|<=|=|==)\\s*-?\\d+(\\.\\d+)?$"

    if (!grepl(pattern, value)) {
      message("Invalid format. Must be an operator (>, <, >=, <=, =, ==)
              followed by a number (e.g., '>0', '<=1', '=0.5')")
      return(FALSE)
    }
    TRUE
  }
)

#' Parameter descriptions
#' @noRd
parameter_descriptions <- list(
  URL_MARKDOWN = "URL to the project's README file",
  AWS_BUCKET_NAME = "Name of the AWS S3 bucket containing ForestForesight data",
  AWS_BUCKET_REGION = "AWS region where the bucket is located",
  EARLIEST_DATA_DATE = "Earliest date for which data should be processed (YYYY-MM-DD)",
  DEFAULT_GROUNDTRUTH = "Default groundtruth pattern (groundtruth1m/3m/6m/12m)",
  FOREST_MASK = "Default forest mask feature (your own or downloaded from the FF repository",
  FOREST_MASK_FILTER = "Condition to apply to the forest mask, for instance >0 to check if the value is bigger than 0",
  LOGGING = "Whether to log the print statements",
  TIMESTAMP = "Whether to always add timestamps to print messages",
  FF_FOLDER = "Local folder path for ForestForesight data",
  DEFAULT_COUNTRY = "Country code (iso-3) for all ff functions (e.g., BRN)",
  LOGFILE_FOLDER = "Folder to store any log files",
  DEFAULT_THRESHOLD = "Default classification threshold, between 0 and 1"
)

#' Helper operator for default values
#' @noRd
`%||%` <- function(x, y) if (is.null(x) || nchar(x) == 0) y else x
