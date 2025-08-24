#' # ---------------------------------------------------------------------------- #
#' #' get_tithi_name
#' #' @description Get name(s) of the Tithi for given Julian day number and place.
#' #'
#' #' @param jd Julian day number
#' #' @param place Vector containing latitude, longitude and timezone
#' #'
#' #' @return Name(s) of the Tithi and its ending time.
#' #'
#' #' @examples
#' #' get_tithi_name(2459778,c(15.34, 75.13, +5.5))
#' #' get_tithi_name(swephR::swe_julday(2022,7,14,0,swephR::SE$GREG_CAL),c(15.34, 75.13, +5.5))
#' get_tithi_name <- function(jd,place){
#'   tithi_ = tithi(jd,place)
#'   size = length(tithi_)
#'   size = size / 4
#'   j <- 1
#'   tithi_name <- ""
#'   for(i in 1:size){
#'     tithi_name <- paste(tithi_name,tithis[tithi_[j]]," till",sep = "")
#'     tithi_name <- paste(tithi_name,paste(tithi_[j+1], tithi_[j+2], tithi_[j+3], sep = ":"))
#'     if(size > 1 && i == 1){
#'       tithi_name <- paste(tithi_name,"& ")
#'     }
#'     j <- 5
#'   }
#'   return (tithi_name)
#' }
#' # ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
#' get_tithi_name
#' @description Get name(s) of the Tithi for given Julian day number and place.
#'
#' @param jd Julian day number
#' @param place Vector containing latitude, longitude and timezone
#'
#' @return Character string: Name(s) of the Tithi with ending time(s).
#'
#' @examples
#' get_tithi_name(2459778, c(15.34, 75.13, +5.5))
#' get_tithi_name(swephR::swe_julday(2022,7,14,0,swephR::SE$GREG_CAL), c(15.34, 75.13, +5.5))
#'
#' @export
get_tithi_name <- function(jd, place) {
  tithi_ <- tithi(jd, place)
  output <- character()
  # Main tithi
  tt <- tithi_$today_tithi
  if (!is.na(tt) && tt >= 1 && tt <= length(tithis)) {
    et <- tithi_$end_time
    t_name <- paste0(
      tithis[tt], " till ",
      sprintf("%02d:%02d:%02d", as.integer(et["hour"]), as.integer(et["min"]), as.integer(et["sec"]))
    )
    output <- c(output, t_name)
  }
  # Leap tithi, if present
  lt <- tithi_$leap_tithi
  if (!is.null(lt) && !is.na(lt) && lt >= 1 && lt <= length(tithis)) {
    let <- tithi_$leap_end_time
    lt_name <- paste0(
      tithis[lt], " till ",
      sprintf("%02d:%02d:%02d", as.integer(let["hour"]), as.integer(let["min"]), as.integer(let["sec"]))
    )
    output <- c(output, lt_name)
  }
  return(paste(output, collapse = " & "))
}
# ---------------------------------------------------------------------------- #
