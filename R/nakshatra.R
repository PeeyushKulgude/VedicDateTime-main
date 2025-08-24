#' # ---------------------------------------------------------------------------- #
#' #' nakshatra
#' #'
#' #' @description Nakshatra for a given place and time
#' #'
#' #' @param jd Julian day number
#' #' @param place Vector containing latitude, longitude and timezone
#' #'
#' #' @return Nakshatra and it's ending time
#' #'
#' #' @examples
#' #' nakshatra(2459778,c(15.34, 75.13, +5.5))
#' #' nakshatra(gregorian_to_jd(17,6,2022),c(15.34, 75.13, +5.5))
#' nakshatra <- function(jd,place){
#'
#'   #Set Lahiri ayanamsa
#'   swephR::swe_set_sid_mode(swephR::SE$SIDM_LAHIRI,0,0)
#'
#'   # 1. Find time of sunrise
#'   lat = place[1]
#'   lon = place[2]
#'   tz = place[3]
#'   rise = sunrise(jd,place)[1]-(tz/24)
#'
#'   # Swiss Ephemeris always gives Sayana. So subtract ayanamsa to get Nirayana
#'   offsets = c(0.0,0.25,0.5,0.75,1.0)
#'   longitudes = c()
#'   for(i in 1:length(offsets)){
#'     longitudes <- append(longitudes,((moon_longitude(rise + offsets[i]) - swephR::swe_get_ayanamsa_ex_ut(rise,swephR::SE$FLG_SWIEPH + swephR::SE$FLG_NONUT)$daya) %% 360))
#'   }
#'   # 2. Today's nakshatra is when offset = 0
#'   # There are 27 Nakshatras spanning 360 degrees
#'   nak = ceiling(longitudes[1] * 27 / 360)
#'
#'   # 3. Find end time by 5-point inverse Lagrange interpolation
#'   y = unwrap_angles(longitudes)
#'   x = offsets
#'   approx_end = inverse_lagrange(x,y,nak * 360/27)
#'   ends = (rise - jd + approx_end) * 24 + tz
#'   answer = c(as.integer(nak),to_dms(ends))
#'
#'   # 4. Check for skipped nakshatra
#'   nak_tmrw = ceiling((longitudes[length(longitudes)] * 27) / 360)
#'   if(((nak_tmrw - nak) %% 27) > 1){
#'     leap_nak = (nak + 1) %% 27
#'     approx_end = inverse_lagrange(offsets,longitudes,leap_nak*360/27)
#'     ends = (rise - jd + approx_end) * 24 + tz
#'     if(leap_nak >= 28){
#'       leap_nak = (leap_nak %% 28) + 1
#'     }
#'     answer <- append(answer,c(as.integer(leap_nak),to_dms(ends)))
#'   }
#'   return (answer)
#' }
#' # ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
#' Nakshatra for a Given Place and Time (Named Return)
#'
#' @description Returns the nakshatra and its ending time (and leap nakshatra, if present) for a given Julian day and place.
#'
#' @param jd Julian day number
#' @param place Vector containing latitude, longitude and timezone
#'
#' @return Named list: `today_nakshatra`, `end_time` (DMS vector), `leap_nakshatra`, `leap_end_time`
#'
#' @examples
#' nakshatra(2459778, c(15.34, 75.13, 5.5))
#'
#' @export
nakshatra <- function(jd, place) {
  swephR::swe_set_sid_mode(swephR::SE$SIDM_LAHIRI, 0, 0)
  tz <- place[3]
  rise <- sunrise(jd, place)$sunrise_jd

  # Compute Nirayana moon longitude at intervals
  offsets <- c(0.0, 0.25, 0.5, 0.75, 1.0)
  ayanamsa <- swephR::swe_get_ayanamsa_ex_ut(rise, swephR::SE$FLG_SWIEPH + swephR::SE$FLG_NONUT)$daya
  longitudes <- sapply(offsets, function(dx)
    (moon_longitude(rise + dx) - ayanamsa) %% 360
  )
  # 27 nakshatras
  nak <- as.integer(ceiling(longitudes[1] * 27 / 360))

  # End time for today's nakshatra
  y <- unwrap_angles(longitudes)
  x <- offsets
  approx_end <- inverse_lagrange(x, y, nak * 360 / 27)
  ends <- (rise - jd + approx_end) * 24 + tz
  end_time <- to_dms(ends)

  # Leap nakshatra
  nak_tmrw <- as.integer(ceiling(longitudes[length(longitudes)] * 27 / 360))
  leap_nakshatra <- NA_integer_
  leap_end_time <- rep(NA_integer_, 3)
  if (((nak_tmrw - nak) %% 27) > 1) {
    leap_nakshatra <- (nak + 1) %% 27
    if (leap_nakshatra == 0) leap_nakshatra <- 27
    approx_end_leap <- inverse_lagrange(x, y, leap_nakshatra * 360 / 27)
    leap_ends <- (rise - jd + approx_end_leap) * 24 + tz
    leap_end_time <- to_dms(leap_ends)
  }

  return(list(
    today_nakshatra = nak,
    end_time = setNames(end_time, c("hour", "min", "sec")),
    leap_nakshatra = leap_nakshatra,
    leap_end_time = setNames(leap_end_time, c("hour", "min", "sec"))
  ))
}
# ---------------------------------------------------------------------------- #

