test_that("ff_dqc_file processes raster file correctly", {
  # Test 1: Basic functionality and return structure
  test_file <- "../test_data/preprocessed/input/10N_110E/10N_110E_2023-01-01_lastsixmonths.tif"
  result <- ff_dqc_file(test_file)

  # Check that the function returns a list with exactly 11 elements
  expect_type(result, "list")
  expect_length(result, 11)

  # Test 2: Check maximum value exists and is numeric
  expect_false(is.na(result$max))
  expect_type(result$max, "double")

  # Test 3: Check required elements are present with correct types
  expected_names <- c(
    "pixel_count", "xmin", "xmax", "ymin", "ymax", "resolution",
    "crs_name", "crs_code", "mean", "max", "has_na"
  )
  expect_named(result, expected_names)

  # Additional validations for Test 3
  expect_type(result$pixel_count, "double")
  expect_type(result$resolution, "double")
  expect_type(result$crs_name, "character")
})
