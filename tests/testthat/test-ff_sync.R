library(testthat)
library(terra)
library(aws.s3)

test_that("ff_sync handles basic tile downloads", {
  # Setup test directory
  test_dir <- tempfile("ff_test_")

  # Test a single tile download with minimal features
  expect_no_error(
    ff_sync(
      ff_folder = test_dir,
      identifier = "00N_000E", # Using a known tile
      features = "Low", # Minimal feature set
      date_start = "2023-01-01",
      date_end = "2023-01-01", # Just one of data
      download_model = FALSE, # Skip model download
      download_predictions = FALSE,
      verbose = FALSE
    )
  )

  # Check that directories were created
  expect_true(dir.exists(file.path(test_dir, "preprocessed", "input", "00N_000E")))
  expect_true(dir.exists(file.path(test_dir, "preprocessed", "groundtruth", "00N_000E")))

  # Cleanup
  unlink(test_dir, recursive = TRUE)
})

test_that("ff_sync validates dates correctly", {
  test_dir <- tempdir()

  # Test invalid start date
  expect_error(
    ff_sync(test_dir, "00N_000E", date_start = "2021-01-15"),
    "date_start must be the first day of a month"
  )

  # Test invalid end date
  expect_error(
    ff_sync(test_dir, "00N_000E", date_end = "2023-12-15"),
    "date_end must be the first day of a month"
  )

  # Test date before earliest allowed
  expect_error(
    ff_sync(test_dir, "00N_000E", date_start = "2020-01-01"),
    "date start cannot be before"
  )

  unlink(test_dir, recursive = TRUE)
})

test_that("ff_sync handles country downloads", {
  test_dir <- tempdir()

  # Test a small country download with minimal features
  expect_no_error(
    ff_sync(
      ff_folder = test_dir,
      identifier = "BRN", # Using Brazil as an example
      features = "Low", # Minimal feature set
      date_start = "2023-01-01",
      date_end = "2023-01-01", # Just one month of data
      download_model = FALSE, # Skip model download
      download_predictions = FALSE,
      verbose = FALSE
    )
  )

  # Check that some data was downloaded
  expect_true(length(list.files(file.path(test_dir, "preprocessed"), recursive = TRUE)) > 0)

  unlink(test_dir, recursive = TRUE)
})

test_that("ff_sync handles different feature sets", {
  test_dir <- tempdir()

  # Test with explicit feature names
  expect_no_error(
    ff_sync(
      ff_folder = test_dir,
      identifier = "00N_000E",
      features = c("initialforestcover", "elevation"), # Specific features
      date_start = "2023-01-01",
      date_end = "2023-02-01",
      download_model = FALSE,
      download_predictions = FALSE,
      verbose = FALSE
    )
  )

  expect_error(
    ff_sync(
      ff_folder = test_dir,
      identifier = "00N_000E",
      features = c("Low", "elevation"), # Specific features
      date_start = "2023-01-01",
      date_end = "2023-02-01",
      download_model = FALSE,
      download_predictions = FALSE,
      verbose = FALSE
    )
  )

  expect_error(
    ff_sync(
      ff_folder = test_dir,
      identifier = "00N_000E",
      features = c("Lowest"), # Specific features
      date_start = "2023-01-01",
      date_end = "2023-02-01",
      download_model = FALSE,
      download_predictions = FALSE,
      verbose = FALSE
    )
  )

  # Check that feature files were downloaded
  files <- list.files(file.path(test_dir, "preprocessed", "input"), recursive = TRUE)
  expect_true(any(grepl("initialforestcover", files)))
  expect_true(any(grepl("elevation", files)))

  unlink(test_dir, recursive = TRUE)
})

test_that("ff_sync creates necessary directories", {
  test_dir <- tempdir()

  expect_no_error(
    ff_sync(
      ff_folder = test_dir,
      identifier = "00N_000E",
      features = "Low",
      date_start = "2023-01-01",
      date_end = "2023-02-01",
      download_model = TRUE,
      download_predictions = TRUE,
      verbose = FALSE
    )
  )

  # Check that directories were created
  expect_true(dir.exists(file.path(test_dir, "preprocessed", "input")))
  expect_true(length(list.files(path = file.path(test_dir, "models"), recursive = TRUE)) > 0)
  expect_true(length(list.files(path = file.path(test_dir, "models"), recursive = TRUE, pattern = "model$")) > 0)
  expect_true(length(list.files(path = file.path(test_dir, "models"), recursive = TRUE, pattern = "rda$")) > 0)
  expect_true(length(list.files(path = file.path(test_dir, "predictions"), recursive = TRUE, pattern = "tif$")) > 0)

  unlink(test_dir, recursive = TRUE)
})
