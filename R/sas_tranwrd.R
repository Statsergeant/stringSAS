
#' Replace Substrings Using SAS TRANWRD Syntax
#'
#' Replaces every occurrence of a target substring with
#' a replacement string. Matching is literal and
#' case-sensitive, following SAS TRANWRD conventions.
#'
#' @param x A character vector.
#' @param target A single non-missing string to find.
#' @param replacement A single non-missing replacement string.
#'
#' @return A character vector with the same length as `x`.
#'   Missing input values remain missing.
#'
#' @examples
#' sas_tranwrd("A-B-C", "-", "_")
#' sas_tranwrd("Hello SAS", "SAS", "R")
#' sas_tranwrd(c("ABC", "ABCABC"), "ABC", "X")
#'
#' @export
sas_tranwrd <- function(x, target, replacement) {

  if (!is.character(x)) {
    stop("`x` must be a character vector.", call. = FALSE)
  }

  if (!is.character(target) ||
      length(target) != 1L ||
      is.na(target) ||
      !nzchar(target)) {
    stop(
      "`target` must be one non-empty, non-missing string.",
      call. = FALSE
    )
  }

  if (!is.character(replacement) ||
      length(replacement) != 1L ||
      is.na(replacement)) {
    stop(
      "`replacement` must be one non-missing string.",
      call. = FALSE
    )
  }

  # Base R gsub with fixed = TRUE performs
  # literal, global substring replacement.
  gsub(
    pattern = target,
    replacement = replacement,
    x = x,
    fixed = TRUE,
    useBytes = FALSE
  )
}
