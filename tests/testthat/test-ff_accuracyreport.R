test_that("ff_accuracyreport handles different input types correctly", {
  # Setup test data
  test_dir <- tempdir()
  datadir <- file.path(getwd(), "../test_data/")
  # Run ff_run to get test data
  accuracy_file <- file.path(test_dir, "accuracy.csv")
  importance_file <- file.path(test_dir, "importance.csv")

  result <- ff_run(
    country = "BRN",
    train_dates = "2023-01-01",
    prediction_dates = c("2023-01-01", "2023-02-01"),
    ff_folder = datadir,
    accuracy_output_path = accuracy_file,
    importance_output_path = importance_file,
    verbose = FALSE
  )

  # Get test data from ff_run results
  accuracy_data <- result$accuracy_dataframe
  importance_data <- result$importance_dataframe

  # Test 1: Path inputs
  testthat::expect_no_error(
    ff_accuracyreport(
      accuracy_data = accuracy_file,
      importance_data = importance_file
    )
  )

  # Test 2: Data frame inputs
  testthat::expect_no_error(
    ff_accuracyreport(
      accuracy_data = accuracy_data,
      importance_data = importance_data
    )
  )

  # Test 3: NULL accuracy data
  testthat::expect_no_error(
    ff_accuracyreport(
      accuracy_data = NULL,
      importance_data = importance_data
    )
  )

  # Test 4: NULL importance data
  testthat::expect_no_error(
    ff_accuracyreport(
      accuracy_data = accuracy_data,
      importance_data = NULL
    )
  )

  # Test 5: Error when both inputs are NULL
  testthat::expect_error(
    ff_accuracyreport(
      accuracy_data = NULL,
      importance_data = NULL
    ),
    "Either accuracy_data or importance_data must be provided"
  )

  # Test 6: Path output
  temp_output <- tempfile(fileext = ".png")
  testthat::expect_no_error(
    ff_accuracyreport(
      accuracy_data = accuracy_data,
      importance_data = importance_data,
      output_path = temp_output
    )
  )
  testthat::expect_true(file.exists(temp_output))
  unlink(temp_output) # Clean up
})
