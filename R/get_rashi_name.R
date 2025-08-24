# ---------------------------------------------------------------------------- #
#' Get Rashi Name(s) for a Given Julian Day
#'
#' @description
#' Returns the name(s) of the Rashi (zodiac sign) for the given Julian day number(s).
#'
#' @param jd Julian day number (numeric, scalar or vector)
#'
#' @return Character vector of Rashi names (NA for invalid values).
#'
#' @examples
#' get_rashi_name(2459778)
#' get_rashi_name(c(2459778, 2459779))
#' get_rashi_name(gregorian_to_jd(30,8,2022))
#'
#' @export
get_rashi_name <- function(jd) {
  if (!is.numeric(jd)) stop("`jd` must be numeric (scalar or vector).")
  idx <- rashi(jd)
  name_vec <- ifelse(
    !is.na(idx) & idx >= 1 & idx <= length(rashis),
    rashis[idx],
    NA_character_
  )
  return(name_vec)
}
# ---------------------------------------------------------------------------- #
