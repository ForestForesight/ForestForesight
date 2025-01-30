test_that("tests for ff_train", {
  test_dir <- tempdir()
  input_data <- ff_prep("../test_data/", country = Sys.getenv("TEST_COUNTRY"), dates = "2023-01-01", sample_size = 0.1, validation_sample = 0.02, inc_features = c("initialforestcover", "timesinceloss"), verbose = F)
  testthat::expect_no_error(ff_train(train_matrix = input_data$feature_dataset, nrounds = 10, verbose = T))
  testthat::expect_no_error(ff_train(input_data$feature_dataset, nrounds = 15, eta = 0.3, max_depth = 7, early_stopping_rounds = 3, verbose = T))
  testthat::expect_no_error(ff_train(input_data$feature_dataset, nrounds = 10, validation_matrix = input_data$validation_matrix, modelfilename = file.path(test_dir, "model.model")))
  result <- ff_train(input_data$feature_dataset, nrounds = 10, validation_matrix = input_data$validation_matrix, verbose = T)
  testthat::expect(inherits(result, "xgb.Booster"), "no booster model was returned")
  testthat::expect(!is.null(result$evaluation_log$eval_aucpr), failure_message = "no validation matrix was found in training though added")
  testthat::expect_no_error(ff_train(input_data$feature_dataset, nrounds = 10, validation_matrix = input_data$validation_matrix, xgb_model = file.path(test_dir, "model.model")))
  input_data2 <- input_data
  input_data2$feature_dataset$label <- input_data2$feature_dataset$label[1:(length(input_data2$feature_dataset$label) - 1)]
  testthat::expect_error(ff_train(train_matrix = input_data2$feature_dataset, nrounds = 10, verbose = T))
  input_data2$feature_dataset$label <- NA
  testthat::expect_error(ff_train(train_matrix = input_data2$feature_dataset, nrounds = 10, verbose = T))
})
