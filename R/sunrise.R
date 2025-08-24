#' # ---------------------------------------------------------------------------- #
#' #' sunrise
#' #'
#' #' @description Sunrise for a given date and place
#' #'
#' #' @param jd Julian day number
#' #' @param place Vector containing latitude, longitude and timezone
#' #'
#' #' @return Sunrise as Julian day number
#' #'
#' #' @examples
#' #' sunrise(2459778,c(15.34, 75.13, +5.5))
#' sunrise <- function(jd,place){
#'   lat = place[1]
#'   lon = place[2]
#'   tz = place[3]
#'   result <- swephR::swe_rise_trans_true_hor(jd - (tz/24.0),swephR::SE$SUN,"",swephR::SE$FLG_SWIEPH,swephR::SE$BIT_DISC_CENTER + swephR::SE$CALC_RISE,c(lon,lat,0),0,0,0)
#'   rise <- result$tret
#'   return (c(rise + (tz)/24.0,to_dms((rise - jd) * 24 + tz)))
#' }
#' # ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
#' Calculate Sunrise for a Given Date and Place
#'
#' @description
#' Calculates sunrise time (Julian Day and local H:M:S) for a given date and place using Swiss Ephemeris.
#'
#' @param jd Julian day number (numeric, scalar or vector)
#' @param place Numeric vector: (latitude, longitude, timezone)
#'
#' @return
#' If scalar: a named list with
#'   - `sunrise_jd`: Sunrise as Julian Day number (local time)
#'   - `sunrise_hms`: Named vector of H:M:S (local time)
#' If vectorized: a data.frame with columns `jd`, `sunrise_jd`, `hour`, `min`, `sec`
#'
#' @examples
#' sunrise(2459778, c(15.34, 75.13, 5.5))
#'
#' @export
sunrise <- function(jd, place) {
  if (!is.numeric(jd)) stop("`jd` must be numeric (scalar or vector).")
  if (!is.numeric(place) || length(place) != 3) stop("`place` must be a numeric vector (lat, lon, tz).")
  lat <- place[1]
  lon <- place[2]
  tz <- place[3]

  calc_sunrise <- function(jd1) {
    result <- tryCatch(
      swephR::swe_rise_trans_true_hor(
        jd1 - (tz/24.0),
        swephR::SE$SUN, "",
        swephR::SE$FLG_SWIEPH,
        swephR::SE$BIT_DISC_CENTER + swephR::SE$CALC_RISE,
        c(lon, lat, 0),
        0, 0, 0
      ),
      error = function(e) return(list(tret = NA))
    )
    rise <- result$tret
    # If rise is NA, return NA for all fields
    if (is.na(rise)) {
      return(list(sunrise_jd = NA_real_, sunrise_hms = c(hour = NA_integer_, min = NA_integer_, sec = NA_integer_)))
    }
    # Calculate local time in hours
    local_time <- (rise - jd1) * 24 + tz
    dms <- to_dms(local_time)
    return(list(sunrise_jd = rise + (tz/24.0), sunrise_hms = setNames(dms, c("hour", "min", "sec"))))
  }

  if (length(jd) == 1) {
    return(calc_sunrise(jd))
  } else {
    out <- lapply(jd, calc_sunrise)
    df <- data.frame(
      jd = jd,
      sunrise_jd = sapply(out, function(x) x$sunrise_jd),
      hour = sapply(out, function(x) x$sunrise_hms[1]),
      min = sapply(out, function(x) x$sunrise_hms[2]),
      sec = sapply(out, function(x) x$sunrise_hms[3])
    )
    return(df)
  }
}
# ---------------------------------------------------------------------------- #
