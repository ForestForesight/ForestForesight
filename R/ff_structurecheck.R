#' Check and Report ForestForesight File Structure
#'
#' This function checks the file structure for ForestForesight data based on a given
#' spatial vector and folder path. It reports on the presence and correctness of
#' various folders and files required for ForestForesight operations.
#'
#' @param shape A SpatVector object representing the area of interest.
#' @param country The ISO-3 code of a country for which you want to do the structure check.
#' @param folder_path Character string. Path to the main ForestForesight folder.
#' @param check_date Character string. Date to check for in the format "YYYY-MM-DD".
#'        If NULL, uses the first of the current month.
#' @param error_on_issue Logical. Whether the function should return an error if critical
#' issues are not handled correctly
#' @param silent_on_pass Logical. Whether correct checks should not be output
#' @param groundtruth_pattern pattern for groundtruth. Default is groundtruth6m for 6 months
#'
#' @return Invisible NULL. The function is called for its side effects (printing).
#'
#' @import terra
#' @importFrom lubridate floor_date
#'
#' @export
ff_structurecheck <- function(shape = NULL,
                              country = Sys.getenv("DEFAULT_COUNTRY"),
                              folder_path = Sys.getenv("FF_FOLDER"),
                              check_date = NULL,
                              error_on_issue = FALSE,
                              silent_on_pass = FALSE,
                              groundtruth_pattern = Sys.getenv("DEFAULT_GROUNDTRUTH")) {
  # Get info from shape
  if (!has_value(shape)) {
    if (!has_value(country)) {
      stop("either a country should be given or a shape in the form of a SpatVector")
    }
    countrylist <- get(data(countries, envir = environment()))
    shape <- vect(countrylist)[which(countrylist$iso3 == country)]
    if (nrow(shape) == 0) {
      stop("incorrect country name, check iso-3 codes")
    }
  }
  info <- get_info(shape, verbose = FALSE)

  # Set check_date if not provided
  if (is.null(check_date)) {
    check_date <- format(lubridate::floor_date(Sys.Date(), "month"), "%Y-%m-01")
  }

  check_main_folders(folder_path, error_on_issue, silent_on_pass)
  check_preprocessed_folders(folder_path, info, check_date, error_on_issue,
    silent_on_pass,
    groundtruth_pattern = groundtruth_pattern
  )
  check_models_folder(folder_path, info, error_on_issue, silent_on_pass)
  check_predictions_folder(folder_path, info, error_on_issue, silent_on_pass)

  invisible(NULL)
}

check_main_folders <- function(folder_path, error_on_issue, silent_on_pass) {
  ff_cat("Checking main folder", verbose = !silent_on_pass)
  main_folders <- c("preprocessed", "models", "predictions")
  all_correct <- TRUE

  for (folder in main_folders) {
    if (!dir.exists(file.path(folder_path, folder))) {
      all_correct <- FALSE
      print_result("No", folder, "folder present",
        color = "red"
      )
    } else {
      print_result(folder, "folder present",
        color = "green",
        silent_on_pass = silent_on_pass
      )
    }
  }

  if (!all_correct && error_on_issue) {
    stop("some main folders are missing. Fix issues as printed above and try again")
  }
}

check_tile_files <- function(folder, tile, subfolder, check_date, silent_on_pass, groundtruth_pattern) {
  files <- list.files(file.path(folder, tile), pattern = "\\.tif$")
  if (length(files) == 0) {
    print_result("No tif files in", subfolder, "for tile", tile,
      color = "red", error_on_issue = TRUE
    )
    invisible(NULL)
  }

  check_file_naming(files, tile, subfolder, silent_on_pass)
  check_date_files(files, tile, check_date, subfolder, silent_on_pass, groundtruth_pattern = groundtruth_pattern)
}

check_file_naming <- function(files, tile, subfolder, silent_on_pass) {
  correct_name_pattern <- paste0("^", tile, "_\\d{4}-\\d{2}-01_[^_]+\\.tif$")
  incorrect_files <- files[!grepl(correct_name_pattern, files)]

  if (length(incorrect_files) > 0) {
    print_result("Incorrect file naming in", subfolder, "for tile", tile, ":",
      color = "red",
      error_on_issue = FALSE
    )
    for (file in incorrect_files) {
      print_result("  ", basename(file),
        color = "red"
      )
    }
    stop("please fix issues above and try again")
  } else {
    print_result(paste("Correct file naming in", subfolder, "for tile", tile, "\n"),
      color = "green",
      silent_on_pass = silent_on_pass
    )
  }
}

check_date_files <- function(files, tile, check_date, subfolder, silent_on_pass, groundtruth_pattern) {
  ewsdatasets <- c(
    "lastsixmonths", "lastmonth", "patchdensity", "confidence",
    "previoussameseason", "smoothedsixmonths", "smoothedtotaldeforestation"
  )
  has_check_date <- any(grepl(paste0(
    "^", tile, "_", check_date, "_",
    paste(ewsdatasets, collapse = "|"), ".tif$"
  ), files))

  if (subfolder != "groundtruth") {
    check_ews_features(has_check_date, check_date, subfolder, tile, silent_on_pass)
  } else {
    check_groundtruth(
      files = files, tile = tile,
      check_date = check_date, silent_on_pass = silent_on_pass,
      groundtruth_pattern = groundtruth_pattern
    )
  }
}

check_ews_features <- function(has_check_date, check_date, subfolder, tile, silent_on_pass) {
  if (!has_check_date) {
    print_result(
      "No EWS features for check date", check_date, "in", subfolder,
      "for tile", tile,
      color = "yellow"
    )
  } else {
    print_result(
      "EWS features for check date", check_date, "present in", subfolder,
      "for tile", tile,
      color = "green",
      silent_on_pass = silent_on_pass
    )
  }
}

check_groundtruth <- function(files, tile, check_date, silent_on_pass, groundtruth_pattern) {
  has_groundtruth <- any(grepl(paste0("^", tile, "_", check_date, paste0("_", groundtruth_pattern, "\\.tif$")), files))
  if (!has_groundtruth) {
    print_result(paste(
      "No", groundtruth_pattern, "file for check date", check_date,
      "in groundtruth for tile", tile, "\n"
    ), color = "yellow")
  } else {
    print_result(
      groundtruth_pattern, "file present for check date", check_date,
      "in groundtruth for tile", tile,
      color = "green", silent_on_pass = silent_on_pass
    )
  }
}

check_preprocessed_folders <- function(folder_path, info,
                                       check_date, error_on_issue, silent_on_pass, groundtruth_pattern) {
  ff_cat("Checking preprocessed folder", verbose = !silent_on_pass)
  prep_subfolders <- c("input", "groundtruth")

  for (subfolder in prep_subfolders) {
    folder <- file.path(folder_path, "preprocessed", subfolder)
    if (!dir.exists(folder)) {
      print_result(paste("No", subfolder, "subfolder in preprocessed"),
        color = "red", error_on_issue = error_on_issue
      )
    } else {
      print_result(subfolder, "subfolder present in preprocessed",
        color = "green",
        silent_on_pass = silent_on_pass
      )
      check_tile_subfolders(folder, info, subfolder, check_date,
        error_on_issue, silent_on_pass,
        groundtruth_pattern = groundtruth_pattern
      )
    }
  }
}

check_tile_subfolders <- function(folder, info, subfolder, check_date,
                                  error_on_issue, silent_on_pass, groundtruth_pattern) {
  for (tile in info$tile_ids) {
    if (!dir.exists(file.path(folder, tile))) {
      print_result("No subfolder for tile", tile, "in", subfolder,
        color = "red",
        error_on_issue = error_on_issue
      )
    } else {
      print_result("Subfolder for tile", tile, "present in", subfolder,
        color = "green",
        silent_on_pass = silent_on_pass
      )
      check_tile_files(folder, tile, subfolder, check_date, silent_on_pass,
        groundtruth_pattern = groundtruth_pattern
      )
    }
    cat("\n")
  }
}

check_models_folder <- function(folder_path, info, error_on_issue, silent_on_pass) {
  ff_cat("Checking models folder", verbose = !silent_on_pass)
  for (group in info$country_groups) {
    check_model_group(folder_path, group, error_on_issue, silent_on_pass)
  }
}

check_model_group <- function(folder_path, group, error_on_issue, silent_on_pass) {
  group_folder <- file.path(folder_path, "models", group)
  if (!dir.exists(group_folder)) {
    print_result("No subfolder for group", group, "in models",
      color = "red",
      error_on_issue = error_on_issue
    )
    invisible(NULL)
  }

  print_result("Subfolder for group", group, "present in models",
    color = "green",
    silent_on_pass = silent_on_pass
  )

  check_model_files(group_folder, group, error_on_issue, silent_on_pass)
}

check_model_files <- function(group_folder, group, error_on_issue, silent_on_pass) {
  model_file <- file.path(group_folder, paste0(group, ".model"))
  rda_file <- file.path(group_folder, paste0(group, ".rda"))

  if (!file.exists(model_file)) {
    print_result(paste("No .model file for group", group),
      color = "yellow"
    )
  } else {
    print_result(paste(".model file present for group", group),
      color = "green",
      silent_on_pass = silent_on_pass
    )
  }

  if (!file.exists(rda_file)) {
    if (file.exists(model_file)) {
      print_result(paste("No .rda file for group", group),
        color = "red",
        error_on_issue = error_on_issue
      )
    } else {
      print_result(paste("No .rda file for group", group),
        color = "yellow"
      )
    }
  } else {
    print_result(paste(".rda file present for group", group),
      color = "green",
      silent_on_pass = silent_on_pass
    )
  }
}

check_predictions_folder <- function(folder_path, info, error_on_issue, silent_on_pass) {
  ff_cat("Checking predictions folder", verbose = !silent_on_pass)
  for (cname in info$overlapping_countries) {
    check_country_predictions(folder_path, cname, error_on_issue, silent_on_pass)
  }
}

check_country_predictions <- function(folder_path, cname, error_on_issue, silent_on_pass) {
  countries <- get(data("countries", envir = environment()))
  iso3 <- countries$iso3[countries$name == cname]
  iso3_folder <- file.path(folder_path, "predictions", iso3)

  if (!dir.exists(iso3_folder)) {
    print_result(paste("No subfolder for ISO3 code", iso3, "in predictions"),
      color = "red",
      error_on_issue = error_on_issue
    )
    invisible(NULL)
  }

  print_result(paste("Subfolder for ISO3 code", iso3, "present in predictions"),
    color = "green",
    silent_on_pass = silent_on_pass
  )

  check_prediction_files(iso3_folder, iso3, error_on_issue, silent_on_pass)
}

check_prediction_files <- function(iso3_folder, iso3, error_on_issue, silent_on_pass) {
  files <- list.files(iso3_folder, pattern = "\\.tif$")
  if (length(files) == 0) {
    print_result(paste("No tif files in predictions for ISO3 code", iso3),
      color = "yellow"
    )
    invisible(NULL)
  }

  correct_name <- all(grepl(paste0("^", iso3, "_\\d{4}-\\d{2}-01\\.tif$"), files))
  if (!correct_name) {
    print_result("Incorrect file naming in predictions for ISO3 code", iso3,
      color = "red",
      error_on_issue = error_on_issue
    )
  } else {
    print_result("Correct file naming in predictions for ISO3 code", iso3,
      color = "green",
      silent_on_pass = silent_on_pass
    )
  }
}

print_result <- function(...,
                         color = "black",
                         silent_on_pass = FALSE,
                         error_on_issue = FALSE) {
  statement <- paste(..., sep = " ")
  if (silent_on_pass && color %in% c("green", "black")) {
    invisible(NULL)
  } else {
    if (color == "red" && error_on_issue) {
      ff_cat(statement, verbose = FALSE, log_level = "ERROR")
      stop(statement)
    } else {
      ff_cat(statement, color = color, verbose = TRUE)
    }
  }
}
