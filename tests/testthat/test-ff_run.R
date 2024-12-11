test_that("check that ff_run can handle all the things", {
  test_dir <- tempdir()
  tiffile <- file.path(test_dir, "result.tif")
  tiffile2 <- file.path(test_dir, "result2.tif")
  datadir <- file.path(getwd(), "../test_data/")
  modelfile <- file.path(test_dir, "model.model")
  testthat::expect_no_error(
    result1 <- ff_run(
      country = "BRN", train_dates = "2023-01-01",
      validation_dates = "2023-02-01", prediction_dates = "2023-03-01",
      ff_folder = datadir,
      ff_prep_parameters = list(inc_features = c("initialforestcover", "timesinceloss")),
      predictions_save_path = tiffile,
      model_save_path = modelfile, verbose = T
    )
  )

  testthat::expect_no_error(
    result2 <- ff_run(country = "BRN", prediction_dates = "2023-03-01", ff_folder = datadir, predictions_save_path = tiffile2, pretrained_model_path = modelfile, verbose = F)
  )
  testthat::expect_equal(global(rast(tiffile), "sum", na.rm = T), global(rast(tiffile2), "sum", na.rm = T))
  countries <- terra::vect(get(data("countries")))
  countries <- countries[countries$iso3 == "BRN"]
  testthat::expect_no_error(result3 <- ff_run(shape = countries, prediction_dates = "2023-03-01", ff_folder = datadir, predictions_save_path = tiffile, pretrained_model_path = modelfile, verbose = F))
  testthat::expect_no_error(result4 <- ff_run(shape = countries, train_dates = "2023-02-01", ff_folder = datadir, model_save_path = modelfile, verbose = F))
  countries <- disagg(countries)

  testthat::expect_no_error(result4 <- ff_run(shape = countries, train_dates = "2023-02-01", prediction_dates = "2023-03-01", ff_folder = datadir, model_save_path = modelfile, verbose = F))
})

test_that("ff_run handles various input combinations and edge cases", {
  # Setup
  test_dir <- tempdir()
  datadir <- file.path(getwd(), "../test_data/")
  tiffile <- file.path(test_dir, "result.tif")
  modelfile <- file.path(test_dir, "model.model")

  # Load test data
  countries <- terra::vect(get(data("countries")))
  test_country <- countries[countries$iso3 == "BRN"]

  # Test basic functionality with minimum required parameters
  testthat::expect_no_error(
    ff_run(
      country = "BRN",
      train_dates = "2023-01-01",
      ff_folder = datadir,
      model_save_path = modelfile
    )
  )

  # Test invalid dates
  testthat::expect_error(
    ff_run(
      country = "BRN",
      train_dates = "invalid_date",
      ff_folder = datadir
    )
  )

  # Test multiple prediction dates
  testthat::expect_no_error(
    ff_run(
      country = "BRN",
      train_dates = "2023-01-01",
      prediction_dates = c("2023-03-01", "2023-04-01"),
      ff_folder = datadir
    )
  )

  # Test custom filter features and conditions
  testthat::expect_no_error(
    ff_run(
      country = "BRN",
      train_dates = "2023-01-01",
      ff_folder = datadir,
      filter_features = c("initialforestcover", "timesinceloss"),
      filter_conditions = c(">0", ">12"),
      model_save_path = modelfile
    )
  )

  # Test autoscaling
  testthat::expect_no_error(
    ff_run(
      country = "BRN",
      train_dates = "2023-01-01",
      ff_folder = datadir,
      autoscale_sample = TRUE,
      model_save_path = modelfile
    )
  )

  # Test validation options
  testthat::expect_no_error(
    ff_run(
      country = "BRN",
      train_dates = "2023-01-01",
      ff_folder = datadir,
      validation = TRUE,
      model_save_path = modelfile
    )
  )

  # Test with pre-trained model but no prediction dates (should error)
  testthat::expect_error(
    ff_run(
      country = "BRN",
      ff_folder = datadir,
      pretrained_model_path = modelfile
    )
  )

  # Test with invalid file paths
  testthat::expect_error(
    ff_run(
      country = "BRN",
      train_dates = "2023-01-01",
      ff_folder = "nonexistent_folder"
    )
  )

  # Test output consistency
  result1 <- ff_run(
    country = "BRN",
    train_dates = "2023-01-01",
    prediction_dates = "2023-03-01",
    ff_folder = datadir,
    predictions_save_path = tiffile,
    model_save_path = modelfile
  )

  result2 <- ff_run(
    country = "BRN",
    prediction_dates = "2023-03-01",
    ff_folder = datadir,
    pretrained_model_path = modelfile
  )

  testthat::expect_equal(
    terra::global(terra::rast(tiffile), "sum"),
    terra::global(result2$predictions, "sum")
  )

  # Test with different certainty thresholds
  testthat::expect_no_error(
    ff_run(
      country = "BRN",
      train_dates = "2023-01-01",
      ff_folder = datadir,
      certainty_threshold = 0.75
    )
  )

  # Test with custom prep parameters
  testthat::expect_no_error(
    ff_run(
      country = "BRN",
      train_dates = "2023-01-01",
      ff_folder = datadir,
      ff_prep_parameters = list(
        inc_features = c("initialforestcover", "timesinceloss"),
        add_date = TRUE,
        add_xy = TRUE
      )
    )
  )

  # Test with importance output
  importance_file <- file.path(test_dir, "importance.csv")
  testthat::expect_no_error(
    ff_run(
      country = "BRN",
      train_dates = "2023-01-01",
      ff_folder = datadir,
      importance_output_path = importance_file
    )
  )
  testthat::expect_true(file.exists(importance_file))
  testthat::expect_true(nrow(read.csv(importance_file)) > 0)

  # Test with accuracy output
  accuracy_file <- file.path(test_dir, "accuracy.csv")
  testthat::expect_no_error(
    ff_run(
      country = "BRN",
      train_dates = "2023-01-01",
      prediction_dates = "2023-03-01",
      ff_folder = datadir,
      accuracy_output_path = accuracy_file
    )
  )
})
