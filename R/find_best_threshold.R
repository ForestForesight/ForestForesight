#' Find Best Threshold
#'
#' This function finds the best threshold for a given prediction function by maximizing the evaluation function.
#'
#' @param prediction A vector of predictions (numeric) or SpatRaster.
#' @param groundtruth A vector of ground truth values (binary) or SpatRaster.
#' @param optimize_function The evaluation function to optimize. Default is get_f_score.
#' @param a Initial guess for the lower bound of threshold search.
#' @param b Initial guess for the upper bound of threshold search.
#' @param tol Tolerance for convergence.
#' @param maxiter Maximum number of iterations.
#' @param beta The weight of precision in the F-score calculation.
#' @return A list containing the best threshold and the corresponding F-score.
#' @examples
#' find_best_threshold(c(0.2, 0.6, 0.7), c(0, 1, 1))
#' @export
find_best_threshold <- function(prediction, groundtruth, optimize_function = get_f_score,
                                a = 0.45, b = 0.55, tol = 0.001, maxiter = 100, beta = 0.5) {
  if (inherits(prediction, "SpatRaster")) {
    prediction <- as.vector(prediction)
  }
  if (inherits(groundtruth, "SpatRaster")) {
    groundtruth <- as.vector(groundtruth)
  }
  # Golden ratio
  phi <- (1 + sqrt(5)) / 2

  # Calculate step sizes
  inv_phi <- 1 / phi

  # Initialize variables
  x1 <- b - inv_phi * (b - a)
  x2 <- a + inv_phi * (b - a)
  f1 <- optimize_function(groundtruth, prediction, x1, beta)
  f2 <- optimize_function(groundtruth, prediction, x2, beta)

  # Iteration loop
  for (i in 1:maxiter) {
    if (f1 > f2) {
      b <- x2
      x2 <- x1
      f2 <- f1
      x1 <- b - inv_phi * (b - a)
      f1 <- optimize_function(groundtruth, prediction, x1, beta)
    } else {
      a <- x1
      x1 <- x2
      f1 <- f2
      x2 <- a + inv_phi * (b - a)
      f2 <- optimize_function(groundtruth, prediction, x2, beta)
    }

    # Check for convergence
    if (abs(b - a) < tol) break
  }

  # Return the best threshold and the corresponding F-score
  return(list(best_threshold = (a + b) / 2, max_f_score = optimize_function(groundtruth, prediction, ((a + b) / 2), beta)))
}
