#' # ---------------------------------------------------------------------------- #
#' #' get_yoga_name
#' #' @description Get name(s) of the Yoga for given Julian day number and place.
#' #'
#' #' @param jd Julian day number
#' #' @param place Vector containing latitude, longitude and timezone
#' #'
#' #' @return Name(s) of the Yoga and its ending time.
#' #'
#' #' @examples
#' #' get_yoga_name(2459778,c(15.34, 75.13, +5.5))
#' #' get_yoga_name(swephR::swe_julday(2022,7,14,0,swephR::SE$GREG_CAL),c(15.34, 75.13, +5.5))
#' get_yoga_name <- function(jd,place){
#'   yoga_ = yoga(jd,place)
#'   size = length(yoga_)
#'   size = size / 4
#'   j <- 1
#'   yoga_name <- ""
#'   for(i in 1:size){
#'     yoga_name <- paste(yoga_name,yogas[yoga_[j]]," till",sep = "")
#'     yoga_name <- paste(yoga_name,paste(yoga_[j+1], yoga_[j+2], yoga_[j+3], sep = ":"))
#'     if(size > 1 && i == 1){
#'       yoga_name <- paste(yoga_name,"& ")
#'     }
#'     j <- 5
#'   }
#'   return (yoga_name)
#' }
# ---------------------------------------------------------------------------- #
#' get_yoga_name
#' @description Get name(s) of the Yoga for given Julian day number and place.
#'
#' @param jd Julian day number
#' @param place Vector containing latitude, longitude and timezone
#'
#' @return Character string: Name(s) of the Yoga with ending time(s).
#'
#' @examples
#' get_yoga_name(2459778, c(15.34, 75.13, +5.5))
#' get_yoga_name(swephR::swe_julday(2022,7,14,0,swephR::SE$GREG_CAL), c(15.34, 75.13, +5.5))
#'
#' @export
get_yoga_name <- function(jd, place) {
  yoga_ <- yoga(jd, place)
  output <- character()
  # Main yoga
  yg <- yoga_$today_yoga
  if (!is.na(yg) && yg >= 1 && yg <= length(yogas)) {
    et <- yoga_$end_time
    yg_name <- paste0(
      yogas[yg], " till ",
      sprintf("%02d:%02d:%02d", as.integer(et["hour"]), as.integer(et["min"]), as.integer(et["sec"]))
    )
    output <- c(output, yg_name)
  }
  # Leap yoga, if present
  lyg <- yoga_$leap_yoga
  if (!is.null(lyg) && !is.na(lyg) && lyg >= 1 && lyg <= length(yogas)) {
    let <- yoga_$leap_end_time
    lyg_name <- paste0(
      yogas[lyg], " till ",
      sprintf("%02d:%02d:%02d", as.integer(let["hour"]), as.integer(let["min"]), as.integer(let["sec"]))
    )
    output <- c(output, lyg_name)
  }
  return(paste(output, collapse = " & "))
}
# ---------------------------------------------------------------------------- #
