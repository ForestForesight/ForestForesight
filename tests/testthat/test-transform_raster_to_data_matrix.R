test_that("transform_raster_to_data_matrix works as expected", {
  # Create a sample SpatRaster object
  r <- rast(nrows = 10, ncols = 10, xmin = 0, xmax = 10, ymin = 0, ymax = 10)
  values(r) <- matrix(1:100, nrow = 10, ncol = 10)
  rasstack <- r # No need to stack, it's already a SpatRaster

  # Define a polygon shape for the extract case
  shape <- vect(matrix(c(2, 2, 8, 2, 8, 8, 2, 8, 2, 2), ncol = 2, byrow = TRUE), type = "polygons")

  # Set parameters
  shrink <- "extract"
  addxy <- TRUE
  dts <- transform_raster_to_dataset(rasstack, shape, shrink, addxy, NULL)
  expect_true(is.data.frame(dts) || is.matrix(dts)) # Check output is matrix or data frame
  expect_true(ncol(dts) >= 2) # Check for extracted data columns
  expect_true("x" %in% colnames(dts) && "y" %in% colnames(dts)) # Check x, y coords
})

test_that("transform_raster_to_data_matrix works with 'no shrink' and addxy = FALSE", {
  # Create a sample SpatRaster object
  r <- rast(nrows = 10, ncols = 10, xmin = 0, xmax = 10, ymin = 0, ymax = 10)
  values(r) <- matrix(1:100, nrow = 10, ncol = 10)
  rasstack <- r

  # Define a polygon shape for the extract case
  # shape <- vect(matrix(c(2, 2, 8, 2, 8, 8, 2, 8, 2, 2), ncol = 2, byrow = TRUE), type = "polygons")
  shape <- NULL
  # Define parameters
  shrink <- "none"
  addxy <- FALSE
  dts <- transform_raster_to_dataset(rasstack, shape, shrink, addxy, NULL)
  rasstack <- as.matrix(rasstack)
  expect_true(is.matrix(dts)) # Check output is matrix
  expect_equal(ncol(dts), ncol(rasstack))
})

test_that("transform_raster_to_data_matrix works with 'no shrink' and addxy = TRUE", {
  # Create a sample SpatRaster object
  r <- rast(nrows = 10, ncols = 10, xmin = 0, xmax = 10, ymin = 0, ymax = 10)
  values(r) <- matrix(1:100, nrow = 10, ncol = 10)
  rasstack <- r

  # Define a polygon shape for extraction
  shape <- vect(matrix(c(2, 2, 8, 2, 8, 8, 2, 8, 2, 2), ncol = 2, byrow = TRUE), type = "polygons")

  # Parameters for the test case
  shrink <- "none"
  addxy <- TRUE

  # Call the function with addxy = TRUE
  dts <- transform_raster_to_dataset(rasstack, shape, shrink, addxy, NULL)

  # Convert rasstack to a matrix for dimension comparison
  rasstack <- as.matrix(rasstack)

  # Check if dts is a matrix
  expect_true(is.matrix(dts))

  # Ensure dts has two additional columns (for x and y coordinates)
  expect_equal(ncol(dts), ncol(rasstack) + 2)
})

test_that("transform_raster_to_data_matrix works with an empty raster input", {
  # Create an empty raster (or alternatively, use an empty matrix)
  small_raster <- rast(nrows = 1, ncols = 1, xmin = 0, xmax = 1, ymin = 0, ymax = 1)
  values(small_raster) <- NA # Set value to NA, simulating "empty" data

  # Define a simple shape for compatibility
  shape <- vect(matrix(c(0, 0, 1, 1), ncol = 2), type = "polygons")
  shrink <- "none"
  addxy <- FALSE

  # Run the function with this minimal raster
  dts <- transform_raster_to_dataset(small_raster, shape, shrink, addxy, NULL)

  # Expect the output to be an empty-like matrix structure with 1 row and 1 column containing NA
  expect_equal(dim(dts), c(1, 1))
  expect_true(dts[1, 1] == 0)
})
