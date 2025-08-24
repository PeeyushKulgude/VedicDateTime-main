#' # ---------------------------------------------------------------------------- #
#' #' moon_longitude
#' #'
#' #' @description Get Lunar longitude for a given Julian day number.
#' #'
#' #' @param jd Julian day
#' #'
#' #' @return Lunar longitude for \code{jd}
#' #'
#' #' @examples
#' #' moon_longitude(2459778)
#' #' moon_longitude(2459500)
#' moon_longitude <- function(jd){
#'   return (swephR::swe_calc_ut(jd, swephR::SE$MOON, swephR::SE$FLG_SWIEPH)$xx[1])
#' }
#' # ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
#' Get Lunar Longitude for Given Julian Day(s)
#'
#' @description
#' Returns the lunar longitude (degrees) for given Julian day number(s), using Swiss Ephemeris.
#'
#' @param jd Julian day (numeric, scalar or vector)
#'
#' @return Numeric vector of lunar longitudes (degrees, NA if unavailable).
#'
#' @examples
#' moon_longitude(2459778)
#' moon_longitude(c(2459778, 2459500))
#'
#' @export
moon_longitude <- function(jd) {
  if (!is.numeric(jd)) stop("`jd` must be numeric (scalar or vector).")
  get_lon <- function(jd1) {
    res <- tryCatch(
      swephR::swe_calc_ut(jd1, swephR::SE$MOON, swephR::SE$FLG_SWIEPH)$xx[1],
      error = function(e) NA_real_
    )
    return(res)
  }
  vapply(jd, get_lon, numeric(1L))
}
# ---------------------------------------------------------------------------- #
