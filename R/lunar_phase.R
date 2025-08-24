#' # ---------------------------------------------------------------------------- #
#' #' lunar_phase
#' #'
#' #' @description Lunar phase for a given Julian day number
#' #'
#' #' @param jd Julian day number
#' #'
#' #' @return Lunar phase
#' #'
#' #' @examples
#' #' lunar_phase(2459778)
#' lunar_phase <- function(jd){
#'   sl = sun_longitude(jd)
#'   ll = moon_longitude(jd)
#'   moon_phase <- ((ll-sl) %% 360)
#'   return (moon_phase)
#' }
#' # ---------------------------------------------------------------------------- #
#'
# ---------------------------------------------------------------------------- #
#' Get Lunar Phase (Degrees) for Given Julian Day(s)
#'
#' @description
#' Returns the lunar phase angle (in degrees, 0–360) for given Julian day number(s).
#' 0 = New Moon, 180 = Full Moon, 90 = First Quarter, 270 = Last Quarter.
#'
#' @param jd Julian day number (numeric, scalar or vector)
#'
#' @return Numeric vector: lunar phase angle(s) in degrees (NA for invalid).
#'
#' @examples
#' lunar_phase(2459778)
#' lunar_phase(c(2459778, 2459500))
#'
#' @export
lunar_phase <- function(jd) {
  if (!is.numeric(jd)) stop("`jd` must be numeric (scalar or vector).")
  sl <- sun_longitude(jd)
  ll <- moon_longitude(jd)
  moon_phase <- ((ll - sl) %% 360)
  # Optionally, set NA if either ll or sl is NA
  moon_phase[is.na(sl) | is.na(ll)] <- NA_real_
  return(moon_phase)
}
# ---------------------------------------------------------------------------- #
