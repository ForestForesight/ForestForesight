test_that("ff_predict functions as expected", {
  data <- ff_prep(datafolder = file.path(getwd(), "../test_data/"), country = "BRN", dates = "2023-01-01", sample_size = 1)
  model <- file.path("../test_data/models/South-Eastern Asia 1/South-Eastern Asia 1.model")
  result <- ff_predict(
    model = model, test_matrix = data$feature_dataset,
    groundtruth = data$feature_dataset$label,
    indices = data$test_indices, templateraster = data$groundtruth_raster
  )
  testthat::expect_equal(class(result$predicted_raster)[1], "SpatRaster")
  testthat::expect_equal(has_value(result$F0.5), TRUE)
  testthat::expect_equal(as.numeric(global(result$predicted_raster, "max")), 1)
  result <- ff_predict(
    model = model, test_matrix = data$feature_dataset,
    groundtruth = data$feature_dataset$label,
    indices = data$test_indices
  )
  testthat::expect_equal(result$predicted_raster, NA)
  result <- ff_predict(
    model = model, test_matrix = data$feature_dataset, groundtruth = data$groundtruth_raster,
    indices = data$test_indices, templateraster = data$groundtruth_raster, certainty = T
  )
  testthat::expect_true(result$F0.5 > 0 && result$F0.5 < 1)
  testthat::expect_true(global(result$predicted_raster, "max") > 0)
})
