check_python_env <- function(pkgname = "forestforesight", verbose = TRUE) {
  required_packages <- c("numpy", "pandas", "mlflow", "torch", "boto3")

  # Set environment path
  if (.Platform$OS.type == "windows") {
    venv_root <- file.path(Sys.getenv("APPDATA"), "forestforesight", "pythonenv")
  } else {
    venv_root <- file.path(Sys.getenv("HOME"), ".forestforesight", "pythonenv")
  }

  # Create venv directory if it doesn't exist
  dir.create(venv_root, recursive = TRUE, showWarnings = FALSE)

  # Check if environment exists
  venv_name <- paste0(pkgname, "-env")
  env_missing <- !reticulate::virtualenv_exists(venv_name)

  # Helper function for package installation with verbose output
  install_packages <- function(packages, venv) {
    for (pkg in packages) {
      ff_cat(sprintf("Installing %s...", pkg), verbose = verbose)
      result <- tryCatch(
        {
          # Try pip install with verbose output
          py_path <- reticulate::py_config()$python
          cmd <- sprintf('"%s" -m pip install %s -v', py_path, pkg)
          output <- system(cmd, intern = TRUE)
          ff_cat(paste(output, collapse = "\n"), verbose = verbose)
          TRUE
        },
        error = function(e) {
          ff_cat(sprintf("Error installing %s: %s", pkg, e$message), verbose = verbose, level = "ERROR")
          FALSE
        }
      )

      if (!result) {
        return(FALSE)
      }
    }
    return(TRUE)
  }

  if (env_missing) {
    ff_cat("Python environment '", venv_name, "' not found.", verbose = TRUE)
    response <- readline(prompt = "Would you like to create it? (y/n): ")
    if (tolower(response) == "y") {
      ff_cat("\nCreating Python environment...", verbose = TRUE)
      tryCatch(
        {
          reticulate::virtualenv_create(venv_name)
          ff_cat("Installing required packages...", verbose = verbose)
          if (!install_packages(required_packages, venv_name)) {
            return(FALSE)
          }
        },
        error = function(e) {
          ff_cat("Error creating environment: ", e$message, verbose = TRUE, level = "ERROR")
          return(FALSE)
        }
      )
    } else {
      ff_cat("Environment creation cancelled.", verbose = TRUE)
      return(FALSE)
    }
  }

  # Try to use the environment
  tryCatch(
    {
      reticulate::use_virtualenv(file.path(venv_root, venv_name), required = TRUE)

      # Get installed packages with direct Python command
      py_path <- reticulate::py_config()$python
      cmd <- sprintf('"%s" -c "import pkg_resources; print(\\"\\n\\".join([dist.project_name for dist in pkg_resources.working_set]))"', py_path)
      installed_pkgs <- system(cmd, intern = TRUE)

      # Check each required package
      missing_pkgs <- required_packages[!tolower(required_packages) %in% tolower(installed_pkgs)]

      if (length(missing_pkgs) > 0) {
        ff_cat("\nMissing packages detected: ", paste(missing_pkgs, collapse = ", "), verbose = TRUE)
        response <- readline(prompt = "Would you like to install them? (y/n): ")
        if (tolower(response) == "y") {
          ff_cat("Installing missing packages...", verbose = TRUE)
          if (!install_packages(missing_pkgs, venv_name)) {
            return(FALSE)
          }

          # Verify installation
          cmd <- sprintf('"%s" -c "import pkg_resources; print(\\"\\n\\".join([dist.project_name for dist in pkg_resources.working_set]))"', py_path)
          installed_pkgs <- system(cmd, intern = TRUE)
          still_missing <- missing_pkgs[!tolower(missing_pkgs) %in% tolower(installed_pkgs)]

          if (length(still_missing) > 0) {
            ff_cat("Failed to install packages: ", paste(still_missing, collapse = ", "), verbose = TRUE, level = "ERROR")
            return(FALSE)
          }

          ff_cat("Packages installed successfully!", verbose = TRUE)
          return(TRUE)
        } else {
          ff_cat("Package installation cancelled.", verbose = TRUE)
          return(FALSE)
        }
      }

      ff_cat("All required packages are installed!", verbose = verbose)
      return(TRUE)
    },
    error = function(e) {
      ff_cat("Error checking Python environment: ", e$message, verbose = TRUE)
      return(FALSE)
    }
  )
}
