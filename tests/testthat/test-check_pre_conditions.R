# Mock input data

dates <- Sys.getenv("TEST_EARLIEST_DATA_DATE")
country <- Sys.getenv("TEST_COUNTRY")
datafolder <- Sys.getenv("TEST_FF_FOLDER")
shape <- NULL
tiles <- NULL
shrink <- "none"

test_that("check_pre_conditions works as expected", {
  # Test valid input, expecting to not throw an error
  expect_no_error(
    check_pre_conditions(dates, country, shape, tiles, shrink, verbose = T, datafolder = datafolder, inc_features = NA, exc_features = NA),
    message = "valid input should not throw an error"
  )

  # Test missing dates with NA
  expect_error(
    check_pre_conditions(NA, country, shape, tiles, shrink, verbose = T, datafolder = datafolder, inc_features = NA, exc_features = NA),
    "No dates were given"
  )

  # Test missing dates with NULL
  expect_error(
    check_pre_conditions(NULL, country, shape, tiles, shrink, verbose = T, datafolder = datafolder, inc_features = NA, exc_features = NA),
    "No dates were given"
  )

  # Test missing dates with an empty string
  expect_error(
    check_pre_conditions("", country, shape, tiles, shrink, verbose = T, datafolder = datafolder, inc_features = NA, exc_features = NA),
    "No dates were given"
  )

  # Test missing tiles, country, and shape
  expect_error(
    check_pre_conditions(dates, NULL, NULL, NULL, shrink, verbose = T, datafolder = datafolder, inc_features = NA, exc_features = NA),
    "Unknown what to process since no tiles, country, or shape were given"
  )

  # Test shape is not SpatVector
  expect_error(
    check_pre_conditions(dates, country, "invalid_shape", tiles, shrink, verbose = T, datafolder = datafolder, inc_features = NA, exc_features = NA),
    "Input must be a SpatVector object, received "
  )
})
