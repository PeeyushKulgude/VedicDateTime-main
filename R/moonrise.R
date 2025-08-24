#' # ---------------------------------------------------------------------------- #
#' #' moonrise
#' #'
#' #' @description Moonrise for a given date and place
#' #'
#' #' @param jd Julian day number
#' #' @param place Vector containing latitude, longitude and timezone
#' #'
#' #' @return Moonrise as Julian day number
#' #'
#' #' @examples
#' #' moonrise(2459778,c(15.34, 75.13, +5.5))
#' moonrise <- function(jd,place){
#'   lat = place[1]
#'   lon = place[2]
#'   tz = place[3]
#'   result <- swephR::swe_rise_trans_true_hor(jd - (tz/24.0),swephR::SE$MOON,"",swephR::SE$FLG_SWIEPH,swephR::SE$BIT_DISC_CENTER + swephR::SE$CALC_RISE,c(lon,lat,0),0,0,0)
#'   rise <- result$tret #Julian day number
#'   #Convert to the given timezone
#'   return (to_dms((rise - jd) * 24 + tz))
#' }
#' # ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
#' Calculate Moonrise for a Given Date and Place
#'
#' @description
#' Calculates moonrise time (Julian Day and local H:M:S) for a given date and place using Swiss Ephemeris.
#'
#' @param jd Julian day number (numeric, scalar or vector)
#' @param place Numeric vector: (latitude, longitude, timezone)
#'
#' @return
#' If scalar: a named list with
#'   - `moonrise_jd`: Moonrise as Julian Day number (local time)
#'   - `moonrise_hms`: Named vector of H:M:S (local time)
#' If vectorized: a data.frame with columns `jd`, `moonrise_jd`, `hour`, `min`, `sec`
#'
#' @examples
#' moonrise(2459778, c(15.34, 75.13, 5.5))
#'
#' @export
moonrise <- function(jd, place) {
  if (!is.numeric(jd)) stop("`jd` must be numeric (scalar or vector).")
  if (!is.numeric(place) || length(place) != 3) stop("`place` must be a numeric vector (lat, lon, tz).")
  lat <- place[1]
  lon <- place[2]
  tz <- place[3]

  calc_moonrise <- function(jd1) {
    result <- tryCatch(
      swephR::swe_rise_trans_true_hor(
        jd1 - (tz/24.0),
        swephR::SE$MOON, "",
        swephR::SE$FLG_SWIEPH,
        swephR::SE$BIT_DISC_CENTER + swephR::SE$CALC_RISE,
        c(lon, lat, 0),
        0, 0, 0
      ),
      error = function(e) return(list(tret = NA))
    )
    rise <- result$tret
    if (is.na(rise)) {
      return(list(moonrise_jd = NA_real_, moonrise_hms = c(hour = NA_integer_, min = NA_integer_, sec = NA_integer_)))
    }
    local_time <- (rise - jd1) * 24 + tz
    dms <- to_dms(local_time)
    return(list(moonrise_jd = rise + (tz)/24.0, moonrise_hms = setNames(dms, c("hour", "min", "sec"))))
  }

  if (length(jd) == 1) {
    return(calc_moonrise(jd))
  } else {
    out <- lapply(jd, calc_moonrise)
    df <- data.frame(
      jd = jd,
      moonrise_jd = sapply(out, function(x) x$moonrise_jd),
      hour = sapply(out, function(x) x$moonrise_hms[1]),
      min = sapply(out, function(x) x$moonrise_hms[2]),
      sec = sapply(out, function(x) x$moonrise_hms[3])
    )
    return(df)
  }
}
# ---------------------------------------------------------------------------- #
