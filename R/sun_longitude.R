#' # ---------------------------------------------------------------------------- #
#' #' sun_longitude
#' #'
#' #' @description Get Solar longitude for a given Julian day number.
#' #'
#' #' @param jd Julian day
#' #'
#' #' @return Solar longitude for \code{jd}
#' #'
#' #' @examples
#' #' sun_longitude(2459778)
#' #' sun_longitude(2459500)
#' sun_longitude <- function(jd){
#'   return (swephR::swe_calc_ut(jd, swephR::SE$SUN, swephR::SE$FLG_SWIEPH)$xx[1])
#' }
#' # ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
#' Get Solar Longitude for Given Julian Day(s)
#'
#' @description
#' Returns the solar longitude (degrees) for given Julian day number(s), using Swiss Ephemeris.
#'
#' @param jd Julian day (numeric, scalar or vector)
#'
#' @return Numeric vector of solar longitudes (degrees, NA if unavailable).
#'
#' @examples
#' sun_longitude(2459778)
#' sun_longitude(c(2459778, 2459500))
#'
#' @export
sun_longitude <- function(jd) {
  if (!is.numeric(jd)) stop("`jd` must be numeric (scalar or vector).")
  get_lon <- function(jd1) {
    res <- tryCatch(
      swephR::swe_calc_ut(jd1, swephR::SE$SUN, swephR::SE$FLG_SWIEPH)$xx[1],
      error = function(e) NA_real_
    )
    return(res)
  }
  vapply(jd, get_lon, numeric(1L))
}
# ---------------------------------------------------------------------------- #
