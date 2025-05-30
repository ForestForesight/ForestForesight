test_that("ff_structurecheck handles folder structure correctly", {
  library(ForestForesight)
  # Create temporary test directory
  test_dir <- tempdir()
  on.exit(unlink(test_dir, recursive = TRUE))
  # Create test shape
  countries <- vect(get(data("countries")))
  shape <- countries[countries$iso3 == "BRN"]
  # Create basic folder structure
  expect_error(ff_structurecheck(shape = shape, folder_path = test_dir, error_on_issue = TRUE), regexp = "some main folders are missing")
  dir.create(file.path(test_dir, "preprocessed"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(test_dir, "models"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(test_dir, "predictions"), recursive = TRUE, showWarnings = FALSE)
  expect_error(ff_structurecheck(shape = shape, folder_path = test_dir, error_on_issue = TRUE), regexp = "subfolder in preprocessed")
  dir.create(file.path(test_dir, "preprocessed", "input"), recursive = TRUE)
  dir.create(file.path(test_dir, "preprocessed", "groundtruth"), recursive = TRUE)
  expect_error(ff_structurecheck(shape = shape, folder_path = test_dir, error_on_issue = TRUE), regexp = "No subfolder for tile")
  dir.create(file.path(test_dir, "preprocessed", "input", "10N_110E"), recursive = TRUE)
  dir.create(file.path(test_dir, "preprocessed", "groundtruth", "10N_110E"), recursive = TRUE)
  expect_error(ff_structurecheck(shape = shape, folder_path = test_dir, error_on_issue = TRUE), regexp = "No tif files in")
  cat("test", file = file.path(test_dir, "preprocessed", "input", "10N_110E", "10N_110E_2024-01-01_test.tif"))
  cat("test", file = file.path(test_dir, "preprocessed", "groundtruth", "10N_110E", "10N_110E_2024-01-01_groundtruth6m.tif"))
  cat("test", file = file.path(test_dir, "preprocessed", "input", "10N_110E", "test.tif"))
  expect_error(ff_structurecheck(shape = shape, folder_path = test_dir, error_on_issue = TRUE), regexp = "please fix issues above and try again")
  file.remove(file.path(test_dir, "preprocessed", "input", "10N_110E", "test.tif"))

  dir.create(file.path(test_dir, "models", "South-Eastern Asia 1"), recursive = TRUE)
  cat("test", file = file.path(test_dir, "models", "South-Eastern Asia 1", "South-Eastern Asia 1.model"), recursive = TRUE)
  expect_error(ff_structurecheck(shape = shape, folder_path = test_dir, error_on_issue = TRUE), "No .rda file for group")
  cat("test", file = file.path(test_dir, "models", "South-Eastern Asia 1", "South-Eastern Asia 1.rda"), recursive = TRUE)

  dir.create(file.path(test_dir, "predictions", "BRN"), recursive = TRUE)
  cat("test", file = file.path(test_dir, "predictions", "BRN", "incorrect.tif"), recursive = TRUE)
  expect_error(ff_structurecheck(shape = shape, folder_path = test_dir, error_on_issue = TRUE), "Incorrect file naming in predictions for ISO3 code")
  file.remove(file.path(test_dir, "predictions", "BRN", "incorrect.tif"))
  expect_no_error(ff_structurecheck(shape = shape, folder_path = test_dir, error_on_issue = TRUE, silent_on_pass = TRUE))
})
