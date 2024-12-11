code_quality_checks <- function() {
  # Style the code
  cat("Styling code...\n")
  styler::style_pkg()

  # Lint the package
  cat("Running lint...\n")
  lintr::lint_package()
}

run_lintr_and_styler_for_file <- function(file_string) {
  cat("Styling file ", file_string, "\n")
  styler::style_file(file_string)
  cat("Linting file ", file_string, "\n")
  run_lintr_for_file(file_string)
}

run_lint_style_package <- function() {
  # Style the code
  cat("Styling code...\n")
  styler::style_pkg()
  # Lint the package
  cat("Running lint...\n")
  lintr::lint_package()
}

run_tests <- function() {
  devtools::test()
}

run_CRAN_tests <- function() {
  devtools::check()
}

run_lintr_for_file <- function(file_string) {
  lint_result <- lintr::lint(file_string)
  cat("Style violations: \n")
  print(lint_result)
  num_lints <- length(lint_result)
  cat("Number of style violations: ", num_lints, "\n")
}
