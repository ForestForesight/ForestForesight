#' @export
config_load <- function(config_file_path = "") {
  library(here)

  # Locate env.yml in the package
  config_file <- system.file("env.yml", package = "ForestForesight")
  if (config_file_path != "") {
    config_file <- config_file_path
  }

  if (file.exists(config_file)) {
    # user_config_file is used by users to replace or supplement default configuration
    user_config_file <- file.path(get_config_dir(), "config.yml")
    if (file.exists(user_config_file)) { # optionally load the user config_file
      print("user_config_file, config.yml was found!")
      load_variables(user_config_file)
    } else {
      message("User config file is not found in the user folder.
              We recommend running ff_environment to create
              your own working environment parameters.")
      load_variables(config_file)
      message("\nDefault config, env.yml loaded successfully.")
    }
  } else {
    stop("Default config file, env.yml does not exist. Please check the file path....")
  }
}
#' Loads the variables in the environment
#' @noRd
load_variables <- function(config_file) {
  library(yaml)
  # Load the YML file
  config <- yaml::yaml.load_file(config_file)
  set_env_vars <- function(config_list, prefix = "") {
    for (name in names(config_list)) {
      value <- config_list[[name]]
      var_name <- paste0(prefix, toupper(name))
      if (is.list(value)) {
        set_env_vars(value, paste0(var_name, "_"))
      } else {
        # Only set environment variables if the value is not NULL or an empty string
        if (!is.null(value) && nzchar(value)) {
          # Resolve relative paths to absolute
          if (grepl("^tests/", value)) {
            value <- system.file(value, package = "ForestForesight")
          }
          library(base)
          do.call(Sys.setenv, stats::setNames(list(value), var_name))
        }
      }
    }
  }
  set_env_vars(config)
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
