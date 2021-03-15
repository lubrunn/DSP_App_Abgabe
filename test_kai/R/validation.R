
#' @export
#' @rdname validation
#'
validate_MA <- function(input) {
  if (input == 1) {
    "Duplicates the column. Please select another value"
  } else {
    NULL
  }
}
