# ---------------------------------------------------------------------------- #
#' Get Lagna Name(s) for a Given Julian Day
#'
#' @description
#' Returns the name(s) of the Lagna (Ascendant) for the given Julian day number(s).
#'
#' @param jd Julian day number (numeric, scalar or vector)
#'
#' @return Character vector of Lagna names, or NA for invalid values.
#'
#' @examples
#' get_lagna_name(2459778)
#' get_lagna_name(c(2459778, 2459779))
#' get_lagna_name(gregorian_to_jd(30,8,2022))
#'
#' @export
get_lagna_name <- function(jd) {
  if (!is.numeric(jd)) stop("`jd` must be numeric (scalar or vector).")
  lagna_idx <- lagna(jd)
  # Vectorized; handle NAs and invalid
  name_vec <- ifelse(
    !is.na(lagna_idx) & lagna_idx >= 1 & lagna_idx <= length(rashis),
    rashis[lagna_idx],
    NA_character_
  )
  return(name_vec)
}
# ---------------------------------------------------------------------------- #
