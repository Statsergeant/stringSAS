
#' Extract Substrings Using SAS-Inspired Syntax
#'
#' Extracts a substring using a starting position and
#' a character count, following SAS SUBSTR conventions.
#'
#' @param x A character vector.
#' @param start A single numeric starting position.
#'   Positions are 1-based.
#' @param length A single non-negative integer indicating
#'   the number of characters to extract.
#'   If NULL, extracts to the end of the string.
#'
#' @return A character vector with the same length as `x`.
#'   Missing values remain missing.
#'
#' @examples
#' sas_substr("STUDY001", 1, 5)
#' sas_substr("STUDY001", 6, 3)
#' sas_substr("STUDY001", 6)
#' sas_substr(c("ABC123", "DEF456"), 1, 3)
#'
#' @export
sas_substr <- function(x, start, length = NULL) {

  # Validate x
  if (!is.character(x)) {
    stop("`x` must be a character vector.", call. = FALSE)
  }

  # Validate start
  if (!is.numeric(start) ||
      length(start) != 1L ||
      is.na(start) ||
      !is.finite(start) ||
      start != trunc(start)) {
    stop(
      "`start` must be a single finite integer.",
      call. = FALSE
    )
  }

  # Validate length
  if (!is.null(length) &&
      (!is.numeric(length) ||
       base::length(length) != 1L ||
       is.na(length) ||
       !is.finite(length) ||
       length < 0 ||
       length != trunc(length))) {
    stop(
      "`length` must be NULL or a non-negative integer.",
      call. = FALSE
    )
  }

  vapply(x, function(value) {

    if (is.na(value)) {
      return(NA_character_)
    }

    n <- nchar(value, type = "chars")

    if (n == 0L) {
      return("")
    }

    # SAS-inspired handling of nonpositive starts
    position <- max(1, start)

    if (position > n) {
      return("")
    }

    if (is.null(length)) {
      end <- n
    } else {
      if (length == 0) {
        return("")
      }

      end <- min(n, position + length - 1)
    }

    base::substr(value, position, end)

  }, character(1), USE.NAMES = FALSE)
}
