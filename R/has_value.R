#' Check if an object has a meaningful value
#'
#' This function determines whether an object contains meaningful data by checking
#' for NULL, NA, empty strings, and other edge cases. For vectors with length > 1,
#' it returns TRUE even if they contain some NA values. For SpatRaster objects,
#' it returns TRUE if the raster exists and has valid dimensions.
#'
#' @param x An R object to be tested.
#'
#' @return Logical. TRUE if the object contains meaningful data, FALSE otherwise.
#'   Returns TRUE for vectors with length > 1 even if they contain NA values.
#'   Returns FALSE for NULL, single NA values, empty strings, and empty vectors.
#'   For SpatRaster objects, returns TRUE if the raster exists and has valid dimensions.
#'
#' @examples
#' has_value(c(1, 2, NA, 4)) # TRUE (vector with length > 1)
#' has_value(NULL) # FALSE
#' has_value(NA) # FALSE
#' has_value("") # FALSE
#' has_value(character(0)) # FALSE
#' has_value(NA_character_) # FALSE
#' has_value(factor("a")) # TRUE
#' has_value(list()) # FALSE
#' \dontrun{
#' r <- terra::rast() # Empty raster
#' has_value(r) # FALSE
#' }
#'
#' @export
has_value <- function(x) {
  # Early returns for NULL and empty vectors
  if (is.null(x) || length(x) == 0) {
    return(FALSE)
  }

  # Handle SpatRaster objects
  if (inherits(x, "SpatRaster")) {
    return(terra::ncell(x) > 0 && !is.null(terra::ext(x)))
  }

  # Always return TRUE for vectors longer than 1
  if (length(x) > 1) {
    return(TRUE)
  }

  # Handle different types of single values
  if (is.na(x) || # Handles all NA types
    identical(x, "") || # Empty string
    identical(x, logical(0)) || # Empty logical
    identical(x, list()) || # Empty list
    identical(x, numeric(0))) { # Empty numeric
    return(FALSE)
  }

  return(TRUE)
}
