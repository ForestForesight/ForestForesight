test_that("get_info handles valid ISO codes correctly", {
  result <- get_info("BRN", verbose = FALSE)

  expect_type(result, "list")
  expect_true(result$num_tiles > 0)
  expect_true(length(result$tile_ids) > 0)
  expect_true(result$area > 0)
  expect_length(result$bbox, 4)
  expect_true("Brunei Darussalam" %in% result$overlapping_countries)
})

test_that("get_info handles SpatVector input correctly", {
  countries <- vect(get(data("countries", envir = environment())))
  brunei_shape <- countries[countries$iso3 == "BRN", ]

  result <- get_info(brunei_shape, verbose = FALSE)

  expect_type(result, "list")
  expect_true(result$num_tiles > 0)
  expect_true(length(result$tile_ids) > 0)
  expect_true(result$area > 0)
  expect_length(result$bbox, 4)
  expect_true("Brunei Darussalam" %in% result$overlapping_countries)
})

test_that("get_info fails with invalid ISO code", {
  expect_error(get_info("XXX"), "Invalid ISO code")
})

test_that("get_info fails with invalid input type", {
  expect_error(get_info(123), "Input must be either")
})

test_that("feature detection works with valid directory", {
  skip_if_not(dir.exists("../test_data/"))
  result <- get_info("BRN", ff_dir = "../test_data/", verbose = FALSE)
  expect_type(result$available_features, "character")
})

test_that("verbose output doesn't affect return value", {
  result_verbose <- get_info("BRN", verbose = TRUE)
  result_quiet <- get_info("BRN", verbose = FALSE)
  expect_equal(result_verbose, result_quiet)
})
