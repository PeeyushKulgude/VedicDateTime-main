#' # ---------------------------------------------------------------------------- #
#' #' Rashi
#' #'
#' #' @description Rashi for a given Julian day number
#' #'
#' #' @param jd Julian day number
#' #'
#' #' @return Rashi as an integer
#' #'
#' #' @examples
#' #' rashi(2459778)
#' #' rashi(gregorian_to_jd(30,8,2022))
#' rashi <- function(jd){
#'   swephR::swe_set_sid_mode(swephR::SE$SIDM_LAHIRI,0,0)
#'   s = moon_longitude(jd)
#'   lunar_nirayana = (moon_longitude(jd) - swephR::swe_get_ayanamsa_ex_ut(jd,swephR::SE$FLG_SWIEPH + swephR::SE$FLG_NONUT)$daya) %% 360
#'   return (ceiling(lunar_nirayana / 30))
#' }
#' # ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
#' Rashi (Lunar Zodiac Sign) for a Given Julian Day Number
#'
#' @description Returns the Rashi (lunar zodiac sign index, 1–12) for the given Julian day number(s).
#'
#' @param jd Julian day number (numeric scalar or vector)
#'
#' @return Integer vector: Rashi index (1–12; NA if invalid)
#'
#' @examples
#' rashi(2459778)
#' rashi(c(2459778, gregorian_to_jd(30,8,2022)))
#'
#' @export
rashi <- function(jd) {
  if (!is.numeric(jd)) stop("`jd` must be numeric (scalar or vector).")
  swephR::swe_set_sid_mode(swephR::SE$SIDM_LAHIRI, 0, 0)
  vapply(jd, function(jd1) {
    ayan <- tryCatch(
      swephR::swe_get_ayanamsa_ex_ut(jd1, swephR::SE$FLG_SWIEPH + swephR::SE$FLG_NONUT)$daya,
      error = function(e) NA_real_
    )
    m_long <- tryCatch(moon_longitude(jd1), error = function(e) NA_real_)
    lunar_nirayana <- (m_long - ayan) %% 360
    idx <- as.integer(ceiling(lunar_nirayana / 30))
    if (is.na(idx) || idx < 1 || idx > 12) NA_integer_ else idx
  }, integer(1L))
}
# ---------------------------------------------------------------------------- #
