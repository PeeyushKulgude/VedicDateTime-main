#' # ---------------------------------------------------------------------------- #
#' #' Calculate Duration of Day (Sunrise to Sunset)
#' #'
#' #' @description Calculates the length of the day (hours) for a given place and Julian day.
#' #'
#' #' @param jd Julian day number (numeric scalar or vector).
#' #' @param place Numeric vector: (latitude, longitude, timezone).
#' #'
#' #' @return A named list (if scalar input), or a data.frame if vectorized:
#' #'   - `duration_hours`: length of day in hours
#' #'   - `duration_dms`: same, as DMS character (e.g. "10:15:36")
#' #'
#' #' @examples
#' #' day_duration(2459778, c(15.34, 75.13, 5.5))
#' #' day_duration(swephR::swe_julday(2022,7,14,0,swephR::SE$GREG_CAL), c(15.34, 75.13, 5.5))
#' #' # Vectorized:
#' #' day_duration(c(2459778,2459779), c(15.34, 75.13, 5.5))
#' #'
#' #' @export
#' day_duration <- function(jd, place) {
#'   if (!is.numeric(jd)) stop("`jd` must be numeric (scalar or vector).")
#'   if (!is.numeric(place) || length(place) != 3) stop("`place` must be a numeric vector of (lat, lon, tz).")
#'
#'   get_dur <- function(jd1) {
#'     srise <- sunrise(jd1, place)[1]
#'     sset  <- sunset(jd1, place)[1]
#'     dur <- (sset - srise) * 24
#'     c(duration_hours = dur, duration_dms = to_dms(dur))
#'   }
#'
#'   if (length(jd) == 1) {
#'     vals <- get_dur(jd)
#'     return(list(duration_hours = vals[1], duration_dms = vals[2]))
#'   } else {
#'     res <- t(sapply(jd, get_dur))
#'     res_df <- data.frame(jd = jd, duration_hours = as.numeric(res[,1]), duration_dms = res[,2], row.names = NULL)
#'     return(res_df)
#'   }
#' }
#' # ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
#' Calculate Duration of Day (Sunrise to Sunset)
#'
#' @description Calculates the length of the day (hours) for a given place and Julian day.
#'
#' @param jd Julian day number (numeric scalar or vector).
#' @param place Numeric vector: (latitude, longitude, timezone).
#'
#' @return A named list (if scalar input), or a data.frame if vectorized:
#'   - `duration_hours`: length of day in hours
#'   - `duration_dms`: same, as DMS character (e.g. "10:15:36")
#'
#' @examples
#' day_duration(2459778, c(15.34, 75.13, 5.5))
#' day_duration(swephR::swe_julday(2022,7,14,0,swephR::SE$GREG_CAL), c(15.34, 75.13, 5.5))
#' # Vectorized:
#' day_duration(c(2459778,2459779), c(15.34, 75.13, 5.5))
#'
#' @export
day_duration <- function(jd, place) {
  if (!is.numeric(jd)) stop("`jd` must be numeric (scalar or vector).")
  if (!is.numeric(place) || length(place) != 3) stop("`place` must be a numeric vector of (lat, lon, tz).")

  get_dur <- function(jd1) {
    srise <- sunrise(jd1, place)$sunrise_jd
    sset  <- sunset(jd1, place)$sunset_jd
    dur <- (sset - srise) * 24
    dms <- to_dms(dur)
    c(duration_hours = dur, duration_dms = paste(dms, collapse = ":"))
  }

  if (length(jd) == 1) {
    vals <- get_dur(jd)
    return(list(duration_hours = as.numeric(vals[1]), duration_dms = vals[2]))
  } else {
    res <- t(sapply(jd, get_dur))
    res_df <- data.frame(jd = jd, duration_hours = as.numeric(res[,1]), duration_dms = res[,2], row.names = NULL)
    return(res_df)
  }
}
# ---------------------------------------------------------------------------- #
