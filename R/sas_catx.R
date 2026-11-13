
#' Concatenate Strings with a Delimiter Using SAS CATX Syntax
#'
#' Concatenates character strings using a delimiter,
#' removing leading and trailing spaces and skipping
#' blank or missing values.
#'
#' @param delimiter A single non-missing character string.
#' @param ... Character vectors to concatenate.
#'   Each argument must have length 1 or the common
#'   output length.
#'
#' @return A character vector containing concatenated strings.
#'   Blank and missing elements are skipped.
#'
#' @examples
#' sas_catx("-", "STUDY01", "001", "0001")
#' sas_catx("-", " ABC ", " DEF ")
#' sas_catx("-", "ABC", "", "DEF")
#' sas_catx("-", c("A", "B"), c("1", "2"))
#'
#' @export
sas_catx <- function(delimiter, ...) {

  # Validate delimiter
  if (!is.character(delimiter) ||
      length(delimiter) != 1L ||
      is.na(delimiter)) {
    stop(
      "`delimiter` must be one non-missing string.",
      call. = FALSE
    )
  }

  args <- list(...)

  # No arguments: return an empty string
  if (base::length(args) == 0L) {
    return("")
  }

  # Validate all inputs
  valid <- vapply(
    args,
    is.character,
    logical(1)
  )

  if (!all(valid)) {
    stop(
      "All arguments in `...` must be character vectors.",
      call. = FALSE
    )
  }

  sizes <- vapply(args, base::length, integer(1))

  # Empty inputs produce an empty character vector
  if (any(sizes == 0L)) {
    return(character(0))
  }

  output_size <- max(sizes)

  # Allow scalar recycling only
  if (any(sizes != 1L & sizes != output_size)) {
    stop(
      "Arguments must have length 1 or a common length.",
      call. = FALSE
    )
  }

  # Recycle scalar arguments
  args <- lapply(
    args,
    rep_len,
    length.out = output_size
  )

  vapply(seq_len(output_size), function(i) {

    values <- vapply(
      args,
      function(arg) arg[[i]],
      character(1)
    )

    # Skip missing values
    values <- values[!is.na(values)]

    # Strip leading/trailing spaces
    values <- trimws(values, whitespace = "[ ]")

    # Skip blank strings
    values <- values[nzchar(values)]

    paste(values, collapse = delimiter)

  }, character(1), USE.NAMES = FALSE)
}
