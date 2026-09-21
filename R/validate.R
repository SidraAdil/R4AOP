#' Check that a value is a single positive whole number
#'
#' @keywords internal
#' @noRd
check_id <- function(x, arg = "id") {
  if (!is.numeric(x) || length(x) != 1 || is.na(x) ||
      x <= 0 || x != round(x)) {
    cli::cli_abort(
      "{.arg {arg}} must be a single positive whole number, not {.val {x}}.",
      call = NULL
    )
  }
  invisible(as.integer(x))
}

#' Check that a value is a whole number within a range
#'
#' @keywords internal
#' @noRd
check_range <- function(x, min, max, arg = "value") {
  if (!is.numeric(x) || length(x) != 1 || is.na(x) ||
      x != round(x) || x < min || x > max) {
    cli::cli_abort(
      "{.arg {arg}} must be a whole number between {min} and {max}, not {.val {x}}.",
      call = NULL
    )
  }
  invisible(as.integer(x))
}

#' Check that a value is one of the allowed options
#'
#' @keywords internal
#' @noRd
check_choice <- function(x, choices, arg = "value") {
  if (is.null(x)) return(invisible(NULL))
  if (length(x) != 1 || !x %in% choices) {
    cli::cli_abort(
      c("{.arg {arg}} must be one of {.val {choices}}.",
        "x" = "You supplied {.val {x}}."),
      call = NULL
    )
  }
  invisible(x)
}

#' Check that a value is a single non-empty string
#'
#' @keywords internal
#' @noRd
check_string <- function(x, arg = "value") {
  if (!is.character(x) || length(x) != 1 || is.na(x) || !nzchar(x)) {
    cli::cli_abort(
      "{.arg {arg}} must be a single non-empty text value, not {.val {x}}.",
      call = NULL
    )
  }
  invisible(x)
}
