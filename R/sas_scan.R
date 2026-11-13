
#' Extract Words Using SAS-Inspired SCAN Syntax
#'
#' Extracts a word from a character string using a positive
#' or negative word position.
#'
#' Positive positions count from the beginning.
#' Negative positions count from the end.
#'
#' Consecutive delimiters are treated as one separator.
#' Unlike SAS, nonexistent words return `NA_character_`.
#'
#' @param x A character vector.
#' @param n A single nonzero integer indicating word position.
#' @param delimiters A single string containing delimiter
#'   characters. If `NULL`, whitespace is used.
#'
#' @return A character vector with the same length as `x`.
#'   Missing inputs and nonexistent words return `NA`.
#'
#' @examples
#' sas_scan("John Smith", 1)
#' sas_scan("John Smith", -1)
#' sas_scan("A-B-C", 2, "-")
#' sas_scan(c("A-B", "C-D"), 2, "-")
#'
#' @export
sas_scan <- function(x, n, delimiters = NULL) {

  # Validate x
  if (!is.character(x)) {
    stop("`x` must be a character vector.", call. = FALSE)
  }

  # Validate n
  if (!is.numeric(n) ||
      length(n) != 1L ||
      is.na(n) ||
      !is.finite(n) ||
      n == 0 ||
      n != trunc(n)) {
    stop(
      "`n` must be a single nonzero integer.",
      call. = FALSE
    )
  }

  # Validate delimiters
  if (!is.null(delimiters) &&
      (!is.character(delimiters) ||
       length(delimiters) != 1L ||
       is.na(delimiters))) {
    stop(
      "`delimiters` must be NULL or one non-missing string.",
      call. = FALSE
    )
  }

  # Build delimiter pattern
  if (is.null(delimiters)) {

    # Default: whitespace
    pattern <- "[[:space:]]+"

  } else if (identical(delimiters, "")) {

    # Empty delimiter string means no splitting
    pattern <- NULL

  } else {

    # Escape regex metacharacters by matching each
    # delimiter literally through a character class.
    delimiter_chars <- strsplit(
      delimiters, "", fixed = TRUE
    )[[1L]]

    escaped <- vapply(delimiter_chars, function(ch) {
      if (ch %in% c("\\", "-", "]", "^", "[")) {
        paste0("\\", ch)
      } else {
        ch
      }
    }, character(1))

    pattern <- paste0("[", paste0(escaped, collapse = ""), "]+")
  }

  vapply(x, function(value) {

    if (is.na(value)) {
      return(NA_character_)
    }

    if (!nzchar(value)) {
      return(NA_character_)
    }

    if (is.null(pattern)) {
      words <- value
    } else {
      words <- strsplit(
        value, pattern, perl = TRUE
      )[[1L]]

      # Remove empty tokens
      words <- words[nzchar(words)]
    }

    if (length(words) == 0L) {
      return(NA_character_)
    }

    # Convert negative position to positive index
    index <- if (n > 0) n else length(words) + n + 1

    if (index < 1 || index > length(words)) {
      return(NA_character_)
    }

    words[[index]]

  }, character(1), USE.NAMES = FALSE)
}
