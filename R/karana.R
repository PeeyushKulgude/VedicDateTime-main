#' # ---------------------------------------------------------------------------- #
#' #' karana
#' #'
#' #' @description Karana for a given place and time
#' #'
#' #' @param jd Julian day number
#' #' @param place Vector containing latitude, longitude and timezone
#' #'
#' #' @return Two karanas
#' #'
#' #' @examples
#' #' karana(2459778,c(15.34, 75.13, +5.5))
#' #' karana(gregorian_to_jd(17,6,2022),c(15.34, 75.13, +5.5))
#' karana <- function(jd,place){
#'   tithi_ = tithi(jd,place)
#'   answer <- c((tithi_[1] * 2) - 1,tithi_[1] * 2)
#'   return(answer)
#' }
#' # ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
#' Karana for a Given Place and Time
#'
#' @description Returns the two karanas for a given place and time.
#'
#' @param jd Julian day number
#' @param place Vector containing latitude, longitude and timezone
#'
#' @return Integer vector of two karanas
#'
#' @examples
#' karana(2459778, c(15.34, 75.13, +5.5))
#' karana(gregorian_to_jd(17,6,2022), c(15.34, 75.13, +5.5))
#'
#' @export
karana <- function(jd, place) {
  tithi_num <- tithi(jd, place)$today_tithi
  answer <- c((tithi_num * 2) - 1, tithi_num * 2)
  return(answer)
}
# ---------------------------------------------------------------------------- #
