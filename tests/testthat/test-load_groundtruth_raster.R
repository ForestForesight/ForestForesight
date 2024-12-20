test_that("load_groundtruth_raster handles no groundtruth file of the correct date correctly", {
  # Mock data
  datafolder <- file.path(getwd(), Sys.getenv("TEST_FF_FOLDER"))
  date <- "2024-09-01" # Missing date for ground truth
  tiles <- c("10N_110E")
  groundtruth_pattern <- Sys.getenv("DEFAULT_GROUNDTRUTH")
  verbose <- TRUE

  message("\ndatafolder: ")
  message(datafolder)
  allfiles <- list_and_filter_tile_files(datafolder = datafolder, tiles, groundtruth_pattern, verbose)
  selected_files <- filter_files_by_date(date, allfiles, groundtruth_pattern)
  extent <- terra::ext(terra::rast(selected_files[1]))
  first <- TRUE
  verbose <- FALSE
  has_groundtruth <- FALSE

  checker_raster <- terra::rast(selected_files[1], win = extent)
  checker_raster[] <- 0
  # Run the function
  result <- load_groundtruth_raster(selected_files, groundtruth_pattern, first, verbose, extent, has_groundtruth)

  # Check the results
  expect_false(result$has_groundtruth)
  expect_equal(dim(result$groundtruth_raster), dim(checker_raster))
  expect_equal(str(result$groundtruth_raster), str(checker_raster))
})
