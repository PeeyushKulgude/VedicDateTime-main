#' # ---------------------------------------------------------------------------- #
#' #' moonset
#' #'
#' #' @description Moonset for a given date and place
#' #'
#' #' @param jd Julian day number
#' #' @param place Vector containing latitude, longitude and timezone
#' #'
#' #' @return Moonset as Julian day number
#' #'
#' #' @examples
#' #' moonset(2459778,c(15.34, 75.13, +5.5))
#' moonset <- function(jd,place){
#'   lat = place[1]
#'   lon = place[2]
#'   tz = place[3]
#'   result <- swephR::swe_rise_trans_true_hor(jd - (tz/24.0),swephR::SE$MOON,"",swephR::SE$FLG_SWIEPH,swephR::SE$BIT_DISC_CENTER + swephR::SE$CALC_SET,c(lon,lat,0),0,0,0)
#'   setting <- result$tret #Julian day number
#'   #Convert to the given timezone
#'   return (to_dms((setting - jd) * 24 + tz))
#' }
#' # ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
#' Calculate Moonset for a Given Date and Place
#'
#' @description
#' Calculates moonset time (Julian Day and local H:M:S) for a given date and place using Swiss Ephemeris.
#'
#' @param jd Julian day number (numeric, scalar or vector)
#' @param place Numeric vector: (latitude, longitude, timezone)
#'
#' @return
#' If scalar: a named list with
#'   - `moonset_jd`: Moonset as Julian Day number (local time)
#'   - `moonset_hms`: Named vector of H:M:S (local time)
#' If vectorized: a data.frame with columns `jd`, `moonset_jd`, `hour`, `min`, `sec`
#'
#' @examples
#' moonset(2459778, c(15.34, 75.13, 5.5))
#'
#' @export
moonset <- function(jd, place) {
  if (!is.numeric(jd)) stop("`jd` must be numeric (scalar or vector).")
  if (!is.numeric(place) || length(place) != 3) stop("`place` must be a numeric vector (lat, lon, tz).")
  lat <- place[1]
  lon <- place[2]
  tz <- place[3]

  calc_moonset <- function(jd1) {
    result <- tryCatch(
      swephR::swe_rise_trans_true_hor(
        jd1 - (tz/24.0),
        swephR::SE$MOON, "",
        swephR::SE$FLG_SWIEPH,
        swephR::SE$BIT_DISC_CENTER + swephR::SE$CALC_SET,
        c(lon, lat, 0),
        0, 0, 0
      ),
      error = function(e) return(list(tret = NA))
    )
    setting <- result$tret
    if (is.na(setting)) {
      return(list(moonset_jd = NA_real_, moonset_hms = c(hour = NA_integer_, min = NA_integer_, sec = NA_integer_)))
    }
    local_time <- (setting - jd1) * 24 + tz
    dms <- to_dms(local_time)
    return(list(moonset_jd = setting + (tz)/24.0, moonset_hms = setNames(dms, c("hour", "min", "sec"))))
  }

  if (length(jd) == 1) {
    return(calc_moonset(jd))
  } else {
    out <- lapply(jd, calc_moonset)
    df <- data.frame(
      jd = jd,
      moonset_jd = sapply(out, function(x) x$moonset_jd),
      hour = sapply(out, function(x) x$moonset_hms[1]),
      min = sapply(out, function(x) x$moonset_hms[2]),
      sec = sapply(out, function(x) x$moonset_hms[3])
    )
    return(df)
  }
}
# ---------------------------------------------------------------------------- #
