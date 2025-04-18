test_that("tests to see if ff_analyze works", {
  test_dir <- tempdir()
  groundtruth <- file.path("../test_data/preprocessed/groundtruth/10N_110E/10N_110E_2023-01-01_groundtruth6m.tif")
  forestmask <- file.path("../test_data/preprocessed/input/10N_110E/10N_110E_2021-01-01_initialforestcover.tif")
  predictions <- file.path("../test_data/predictions/BRN/BRN_2023-01-01.tif")
  testthat::expect_no_error(polygons <- ff_analyze(predictions = predictions, groundtruth = groundtruth, forest_mask = forestmask, remove_empty = FALSE))
  predictions <- terra::rast(predictions)
  testthat::expect_no_error(polygons <- ff_analyze(
    predictions = predictions, groundtruth = groundtruth,
    forest_mask = forestmask, remove_empty = FALSE,
    csv_filename = file.path(test_dir, "test.csv")
  ))
  testthat::expect_true(file.exists(file.path(test_dir, "test.csv")))
  testthat::expect_gt(nrow(polygons), 0)
  testthat::expect_gt(sum(polygons$FP + polygons$TP + polygons$FN), nrow(polygons))
  ratio1 <- sum(polygons$FP) / sum(polygons$TP + polygons$FN)
  testthat::expect_no_error(polygons <- ff_analyze(
    predictions = predictions, groundtruth = groundtruth,
    forest_mask = forestmask, remove_empty = FALSE,
    csv_filename = file.path(test_dir, "test.csv"), calculate_best_threshold = TRUE
  ))
  ratio2 <- sum(polygons$FP) / sum(polygons$TP + polygons$FN)
  testthat::expect_gt(ratio2, ratio1)
  testthat::expect_error(polygons <- ff_analyze(predictions = predictions, groundtruth = groundtruth, forest_mask = "notexistingpath", remove_empty = FALSE), regexp = "forest mask")
})
