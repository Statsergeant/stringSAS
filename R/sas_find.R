
#' Find a Substring Using SAS-Inspired FIND Syntax
#'
#' Searches for a substring and returns its 1-based
#' position, or zero when no match is found.
#'
#' @param x A character vector to search.
#' @param substring A single non-missing character string.
#' @param modifier Either `""` for case-sensitive
#'   matching or `"i"` for case-insensitive matching.
#' @param startpos A single positive integer indicating
#'   where to begin searching.
#'
#' @return An integer vector of the same length as `x`.
#'   Returns 0 for no match and NA for missing inputs.
#'
#' @examples
#' sas_find("Hello World", "World")
#' sas_find("Hello World", "world", "i")
#' sas_find("ABCABC", "ABC", startpos = 4)
#' sas_find(c("ABC", "DEF"), "B")
#'
#' @export
sas_find <- function(x, substring,
                     modifier = "", startpos = 1) {

  if (!is.character(x)) {
    stop("`x` must be a character vector.", call. = FALSE)
  }

  if (!is.character(substring) ||
      length(substring) != 1L ||
      is.na(substring)) {
    stop(
      "`substring` must be one non-missing string.",
      call. = FALSE
    )
  }

  if (!is.character(modifier) ||
      length(modifier) != 1L ||
      is.na(modifier) ||
      !tolower(modifier) %in% c("", "i")) {
    stop(
      "`modifier` must be either '' or 'i'.",
      call. = FALSE
    )
  }

  if (!is.numeric(startpos) ||
      length(startpos) != 1L ||
      is.na(startpos) ||
      !is.finite(startpos) ||
      startpos < 1 ||
      startpos != trunc(startpos)) {
    stop(
      "`startpos` must be a positive integer.",
      call. = FALSE
    )
  }

  ignore_case <- tolower(modifier) == "i"

  vapply(x, function(value) {

    if (is.na(value)) {
      return(NA_integer_)
    }

    # No search result for an empty substring
    if (!nzchar(substring)) {
      return(0L)
    }

    n <- nchar(value, type = "chars")

    if (startpos > n) {
      return(0L)
    }

    # Search the portion beginning at startpos
    search_text <- base::substr(
      value, startpos, n
    )

    # Fixed literal substring search
    # Use literal matching without interpreting regex characters
    if (ignore_case) {
      search_text <- tolower(search_text)
      search_pattern <- tolower(substring)
    } else {
      search_pattern <- substring
    }

    result <- regexpr(
      search_pattern,
      search_text,
      fixed = TRUE
    )

    position <- as.integer(result[[1L]])

    if (position == -1L) {
      return(0L)
    }

    as.integer(startpos + position - 1L)

  }, integer(1), USE.NAMES = FALSE)
}
