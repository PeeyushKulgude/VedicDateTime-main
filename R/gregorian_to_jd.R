#' # ---------------------------------------------------------------------------- #
#' #' gregorian_to_jd
#' #'
#' #' @description Convert Gregorian date to Julian day number at 00:00 UTC
#' #'
#' #' @param day Day number
#' #' @param month Month number
#' #' @param year Year number
#' #'
#' #' @return Julian day number
#' #'
#' #' @examples
#' #' gregorian_to_jd(18,7,2022)
#' gregorian_to_jd <- function(day,month,year){
#'   return (swephR::swe_julday(year, month, day, 0.0,swephR::SE$GREG_CAL))
#' }
#' # ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
#' Convert Gregorian Date(s) to Julian Day Number(s) at 00:00 UTC
#'
#' @description Converts Gregorian date(s) to Julian day number(s) at midnight UTC.
#'
#' @param day Day number (numeric, scalar or vector)
#' @param month Month number (numeric, scalar or vector)
#' @param year Year number (numeric, scalar or vector)
#'
#' @return Numeric vector: Julian day number(s)
#'
#' @examples
#' gregorian_to_jd(18, 7, 2022)
#' gregorian_to_jd(c(18,19), 7, 2022)
#'
#' @export
gregorian_to_jd <- function(day, month, year) {
  if (!is.numeric(day) || !is.numeric(month) || !is.numeric(year))
    stop("All arguments must be numeric (scalars or vectors of same length).")
  n <- max(length(day), length(month), length(year))
  day <- rep(day, length.out = n)
  month <- rep(month, length.out = n)
  year <- rep(year, length.out = n)
  vapply(seq_len(n), function(i) {
    swephR::swe_julday(year[i], month[i], day[i], 0.0, swephR::SE$GREG_CAL)
  }, numeric(1L))
}
# ---------------------------------------------------------------------------- #
