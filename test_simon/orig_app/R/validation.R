
#' #' @export
#' #' @rdname validation
#' #'
# validate_MA <- function(input) {
#   if (input == 1) {
#     "Duplicates the column. Please select another value"
#   } else {
#     NULL
#   }
# }

#' @export
#' @rdname validation
#'
validate_no_decimals <- function(input) {
  c <- stri_detect_fixed(input,".")
  if (c == TRUE) {
    "No decimals allowed"
  } else {
    NULL
  }
}
#' @export
#' @rdname validation
#'
validate_iregulars <- function(input) {
  single_nums <- unlist(str_split(input, ","))
  c <- !is.na(as.numeric(single_nums))
  if (is.element(FALSE, c)) {
    "Please choose numbers"
  } else {
    NULL
  }
}
#' @export
#' @rdname validation
#'
validate_negatives <- function(input) {
  single_nums <- unlist(str_split(input, ","))
  if (any(single_nums < 0)) {
    "Please choose numbers"
  } else {
    NULL
  }
}


#' @export
#' @rdname validation
#'
validate_Large_numbers <- function(input) {
  single_nums <- unlist(str_split(input, ","))
  if (any(single_nums > 400)) {
    "Please choose a small window"
  } else {
    NULL
  }
}



#' @export
#' @rdname validation
#'
validate_no_zeros <- function(input) {
  single_nums <- unlist(str_split(input, ","))
  if (any(single_nums == 0)) {
    "Do not include zeros"
  } else {
    NULL
  }
}
