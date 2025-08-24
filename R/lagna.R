#' # ---------------------------------------------------------------------------- #
#' #' Lagna
#' #'
#' #' @description Lagna for a given Julian day number
#' #'
#' #' @param jd Julian day number
#' #'
#' #' @return Lagna as an integer
#' #'
#' #' @examples
#' #' lagna(2459778)
#' #' lagna(gregorian_to_jd(30,8,2022))
#' lagna <- function(jd){
#'   swephR::swe_set_sid_mode(swephR::SE$SIDM_LAHIRI,0,0)
#'   s = sun_longitude(jd)
#'   solar_nirayana = (sun_longitude(jd) - swephR::swe_get_ayanamsa_ex_ut(jd,swephR::SE$FLG_SWIEPH + swephR::SE$FLG_NONUT)$daya) %% 360
#'   return (ceiling(solar_nirayana / 30))
#' }
#' # ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
#' Lagna (Solar Rashi) for a Given Julian Day Number
#'
#' @description Returns Lagna (solar zodiac sign index, 1–12) for the given Julian day number(s).
#'
#' @param jd Julian day number (numeric, scalar or vector)
#'
#' @return Integer vector: Lagna index (1–12; NA if invalid)
#'
#' @examples
#' lagna(2459778)
#' lagna(c(2459778, gregorian_to_jd(30,8,2022)))
#'
#' @export
lagna <- function(jd) {
  if (!is.numeric(jd)) stop("`jd` must be numeric (scalar or vector).")
  swephR::swe_set_sid_mode(swephR::SE$SIDM_LAHIRI, 0, 0)
  s <- sun_longitude(jd)
  ayan <- vapply(jd, function(jd1) {
    res <- tryCatch(
      swephR::swe_get_ayanamsa_ex_ut(jd1, swephR::SE$FLG_SWIEPH + swephR::SE$FLG_NONUT)$daya,
      error = function(e) NA_real_
    )
    res
  }, numeric(1L))
  solar_nirayana <- (s - ayan) %% 360
  lagna_idx <- ceiling(solar_nirayana / 30)
  lagna_idx[is.na(solar_nirayana)] <- NA_integer_
  return(lagna_idx)
}
# ---------------------------------------------------------------------------- #
