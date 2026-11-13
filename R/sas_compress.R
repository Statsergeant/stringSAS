
#' Compress Character Strings Using SAS-Inspired Syntax
#'
#' Removes or retains characters based on literal character
#' selections and SAS-inspired modifiers.
#'
#' @param x A character vector.
#' @param chars A single string of characters to select.
#'   If `NULL`, ordinary spaces are removed when no character
#'   class modifier is supplied.
#' @param modifier A single string containing any combination
#'   of `a` (letters), `d` (ASCII digits), `s` (whitespace),
#'   and `k` (keep selected characters).
#'
#' @return A character vector of the same length as `x`.
#'   Missing values remain missing.
#'
#' @examples
#' sas_compress("A B C")
#' sas_compress("AB123", modifier = "d")
#' sas_compress("AB123", modifier = "kd")
#' sas_compress("A-B", chars = "-")
#' sas_compress(c("A B", NA_character_))
#'
#' @export
sas_compress <- function(x, chars = NULL, modifier = "") {

  if (!is.character(x)) {
    stop("`x` must be a character vector.", call. = FALSE)
  }

  if (!is.null(chars) &&
      (!is.character(chars) ||
       length(chars) != 1L ||
       is.na(chars))) {
    stop("`chars` must be NULL or one non-missing string.",
         call. = FALSE)
  }

  if (!is.character(modifier) ||
      length(modifier) != 1L ||
      is.na(modifier)) {
    stop("`modifier` must be one non-missing string.",
         call. = FALSE)
  }

  modifier <- tolower(modifier)

  mods <- strsplit(
    modifier, "", fixed = TRUE
  )[[1L]]

  valid_mods <- c("a", "d", "s", "k")

  if (any(!mods %in% valid_mods)) {
    stop(
      "Unsupported modifier. Use a, d, s, or k.",
      call. = FALSE
    )
  }

  keep <- "k" %in% mods
  use_alpha <- "a" %in% mods
  use_digits <- "d" %in% mods
  use_space <- "s" %in% mods

  # When no explicit chars or character classes exist,
  # select ordinary spaces by default.
  if (is.null(chars)) {
    if (!use_alpha && !use_digits && !use_space) {
      chars <- " "
    } else {
      chars <- ""
    }
  }

  literal_chars <- strsplit(
    chars, "", fixed = TRUE
  )[[1L]]

  vapply(x, function(value) {

    if (is.na(value)) {
      return(NA_character_)
    }

    if (!nzchar(value)) {
      return("")
    }

    characters <- strsplit(
      value, "", fixed = TRUE
    )[[1L]]

    selected <- characters %in% literal_chars

    if (use_alpha) {
      selected <- selected |
        grepl("^[[:alpha:]]$", characters)
    }

    if (use_digits) {
      selected <- selected |
        grepl("^[0-9]$", characters)
    }

    if (use_space) {
      selected <- selected |
        grepl("^[[:space:]]$", characters)
    }

    if (keep) {
      characters <- characters[selected]
    } else {
      characters <- characters[!selected]
    }

    paste0(characters, collapse = "")

  }, character(1), USE.NAMES = FALSE)
}
