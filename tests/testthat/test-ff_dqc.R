test_that("ff_dqc processes folder of raster files correctly", {
  # Test 1: Basic functionality and return structure
  test_folder <- "../test_data/preprocessed/input/10N_110E/"
  result <- ff_dqc(test_folder)

  # Check that the function returns a list with exactly 7 elements
  testthat::expect_type(result, "list")
  testthat::expect_length(result, 7)
  testthat::expect_named(result, c(
    "tile", "byfeature", "all", "equalextent",
    "equaldaterange", "incorrect_dateformats", "minextent"
  ))

  # Test 2: Check the byfeature data frame structure and content
  testthat::expect_s3_class(result$byfeature, "data.frame")
  expected_cols <- c(
    "feature", "type", "min_date", "max_date", "has_gaps", "has_doubles",
    "pixel_count", "xmin", "xmax", "ymin", "ymax", "resolution",
    "crs_name", "crs_code", "mean_value", "max_value", "has_na"
  )
  testthat::expect_named(result$byfeature, expected_cols)

  # Test 3: Validate output types and data integrity
  testthat::expect_type(result$tile, "character")
  testthat::expect_equal(result$tile, "10N_110E")
  testthat::expect_type(result$equalextent, "logical")
  testthat::expect_type(result$equaldaterange, "logical")
  testthat::expect_type(result$incorrect_dateformats, "integer")
  testthat::expect_s4_class(result$minextent, "SpatExtent")

  # Additional validations
  testthat::expect_true(all(result$byfeature$type %in% c("static", "dynamic")))
  testthat::expect_true(all(result$byfeature$has_gaps %in% c("yes", "no")))
  testthat::expect_true(all(result$byfeature$has_doubles %in% c("yes", "no")))
  testthat::expect_true(all(result$byfeature$pixel_count == "differing" |
    is.numeric(result$byfeature$pixel_count)))
})

test_that("ff_dqc handles edge cases correctly", {
  # Test 4: Test with return_values = FALSE
  test_folder <- "../test_data/preprocessed/input/10N_110E/"
  result <- ff_dqc(test_folder, return_values = FALSE)
  testthat::expect_true(all(is.na(result$byfeature$mean_value)))
  testthat::expect_true(all(is.na(result$byfeature$max_value)))
  testthat::expect_true(all(is.na(result$byfeature$has_na)))
})

test_that("ff_dqc validates date formats correctly", {
  # Test 5: Check date format validation
  test_folder <- "../test_data/preprocessed/input/10N_110E/"
  result <- ff_dqc(test_folder)

  # Check date format in all rows
  dates <- result$all$dates
  testthat::expect_true(all(nchar(dates) == 10))
  testthat::expect_true(all(!is.na(as.Date(dates, format = "%Y-%m-%d"))))

  # Check min_date and max_date formats in byfeature
  valid_dates <- result$byfeature[!is.na(result$byfeature$min_date), ]
  testthat::expect_true(all(!is.na(as.Date(valid_dates$min_date))))
  testthat::expect_true(all(!is.na(as.Date(valid_dates$max_date))))
})

test_that("summary_by_feature handles single features correctly", {
  # Test 6: Test summary_by_feature function with single row
  test_df <- data.frame(
    feature_names = "test_feature",
    dates = "2023-01-01",
    pixel_count = 100,
    xmin = 0,
    xmax = 1,
    ymin = 0,
    ymax = 1,
    resolution = 0.1,
    crs_name = "test_crs",
    crs_code = "EPSG:4326",
    mean = 5,
    max = 10,
    has_na = FALSE
  )
  test_df <- rbind(test_df, test_df)

  result <- summary_by_feature(test_df, "test_feature")
  testthat::expect_equal(result$type, "dynamic")
  testthat::expect_equal(result$pixel_count, 100)
  testthat::expect_equal(result$has_doubles, "yes")
})
