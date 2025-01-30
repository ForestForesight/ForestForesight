test_that("ff_polygonize handles basic functionality", {
  # Setup test data
  test_raster <- terra::rast("../test_data/predictions/BRN/BRN_2023-01-01.tif")

  # Test 1: Basic functionality with default parameters
  result <- ff_polygonize(test_raster)
  expect_true(inherits(result$polygons, "SpatVector"))
  expect_true(all(c("risk", "size", "riskfactor", "threshold", "date") %in% names(result$polygons)))
  expect_true(all(result$polygons$risk >= 0 & result$risk <= 1))

  # Test 2: Test with custom threshold
  high_thresh_result <- ff_polygonize(
    test_raster,
    threshold = 0.8,
    minimum_pixel_count = 3
  )
  expect_true(all(high_thresh_result$polygons$threshold == 0.8))
  expect_true(length(high_thresh_result$polygons) <= length(result$polygons)) # Should have fewer polygons

  # Test 3: Test automatic thresholding
  auto_thresh_result <- ff_polygonize(
    test_raster,
    threshold = "medium",
    verbose = TRUE
  )
  expect_true(inherits(auto_thresh_result$polygons, "SpatVector"))
  expect_true(!is.na(auto_thresh_result$polygons$threshold[1]))

  # Test 4: Test output file writing
  temp_file <- tempfile(fileext = ".shp")
  output_result <- ff_polygonize(
    test_raster,
    output_file = temp_file,
    minimum_pixel_count = 5
  )
  expect_true(file.exists(temp_file))
  expect_true(inherits(terra::vect(temp_file), "SpatVector"))

  # Test 5: Test calculate_max_count functionality
  max_count_result <- ff_polygonize(
    test_raster,
    calculate_max_count = TRUE
  )
  expect_true(inherits(max_count_result$polygons, "SpatVector"))
  expect_true(length(max_count_result$polygons) > 0)

  # Test 6: Test error handling for invalid inputs
  expect_error(
    ff_polygonize(test_raster, threshold = "invalid_threshold"),
    "threshold must be numeric or one of: medium, high, very high"
  )
})
