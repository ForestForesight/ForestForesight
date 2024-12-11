#' @import testthat
#' @import crayon
#' @import logging
#' @import withr

# Test setup helper function
setup_test_env <- function() {
  # Create temporary directory for log files
  temp_dir <- tempdir()
  log_file <- file.path(temp_dir, "test.log")

  # Clean up any existing logger
  tryCatch(
    {
      logger <- logging::getLogger("ForestForesight")
      if (!is.null(logger)) {
        # Remove all handlers from the logger
        for (handler in logger$handlers) {
          logging::removeHandler(handler, logger = "ForestForesight")
        }
      }
    },
    error = function(e) {
      # If logger doesn't exist, that's fine
    }
  )

  list(
    temp_dir = temp_dir,
    log_file = log_file
  )
}

test_that("setup_logging creates necessary directories and handlers", {
  env <- setup_test_env()

  # Test directory creation
  setup_logging(env$log_file)
  expect_true(dir.exists(dirname(env$log_file)))

  # Test logger creation


  # Test handler attachment
  logger <- logging::getLogger("ForestForesight")
  expect_true(length(logger$handlers) > 0)
  logging::logReset()
})

test_that("format_text correctly handles input combinations", {
  # Test basic concatenation
  expect_equal(
    format_text("Hello", "World", sep = " ", auto_newline = FALSE),
    "Hello World"
  )

  # Test with auto_newline
  expect_equal(
    format_text("Hello", "World", sep = " ", auto_newline = TRUE),
    "Hello World\n"
  )

  # Test with custom separator
  expect_equal(
    format_text("Hello", "World", sep = "-", auto_newline = FALSE),
    "Hello-World"
  )

  # Test with existing newline
  expect_equal(
    format_text("Hello\n", auto_newline = TRUE),
    "Hello\n"
  )
})

test_that("handle_output manages console output correctly", {
  env <- setup_test_env()

  # Test console output with color
  output <- capture.output(
    handle_output("Test", color = "red", verbose = TRUE)
  )
  expect_true(length(output) > 0)

  # Test output suppression
  output <- capture.output(
    handle_output("Test", verbose = FALSE)
  )
  expect_equal(length(output), 0)

  # Test timestamp inclusion
  output <- capture.output(
    handle_output("Test", timestamp = TRUE)
  )
  expect_match(output, "\\d{4}-\\d{2}-\\d{2}")

  # Test style application
  output <- capture.output(
    handle_output("Test", style = "bold")
  )
  expect_true(length(output) > 0)
})

test_that("ff_cat integrates all components correctly", {
  env <- setup_test_env()

  # Test basic functionality
  expect_output(
    ff_cat("Test message", log_file = env$log_file)
  )

  # Test log file creation and content
  expect_true(file.exists(env$log_file))
  log_content <- readLines(env$log_file)
  expect_true(length(log_content) > 0)

  # Test different log levels
  expect_output(
    ff_cat("Error test", log_level = "ERROR", log_file = env$log_file)
  )
  log_content <- readLines(env$log_file)
  expect_match(log_content[length(log_content)], "ERROR", fixed = TRUE)

  # Test color and style combination
  expect_output(
    ff_cat("Colored bold", color = "blue", style = "bold", log_file = env$log_file)
  )
})

test_that("ff_cat handles invalid inputs gracefully", {
  env <- setup_test_env()

  # Test invalid color
  expect_warning(
    ff_cat("Test", color = "invalid_color", log_file = env$log_file)
  )

  # Test invalid style
  expect_warning(
    ff_cat("Test", style = "invalid_style", log_file = env$log_file)
  )

  # Test invalid log level (should default to INFO)
  expect_output(
    ff_cat("Test", log_level = "INVALID", log_file = env$log_file)
  )
  log_content <- readLines(env$log_file)
  expect_match(log_content[length(log_content)], "INFO", fixed = TRUE)
})

test_that("ff_cat handles special characters correctly", {
  env <- setup_test_env()

  # Test with newlines
  expect_output(
    ff_cat("Line 1\nLine 2", log_file = env$log_file)
  )

  # Test with ANSI escape sequences
  expect_output(
    ff_cat("\033[31mRed text\033[0m", log_file = env$log_file)
  )

  # Verify log file doesn't contain ANSI codes
  log_content <- readLines(env$log_file)
  expect_false(any(grepl("\033", log_content, fixed = TRUE)))
})

test_that("ff_cat maintains backward compatibility", {
  env <- setup_test_env()

  # Test old parameter combinations
  expect_output(
    ff_cat("Test", sep = "|", fill = TRUE, log_file = env$log_file)
  )

  # Test with labels
  expect_output(
    ff_cat("Test", labels = "LABEL:", log_file = env$log_file)
  )
})
