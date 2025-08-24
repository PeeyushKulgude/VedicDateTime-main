#' # ---------------------------------------------------------------------------- #
#' #' Get Masa Name(s) for Given Julian Day and Place
#' #'
#' #' @description
#' #' Returns the name(s) of the Masa (lunar month) for the given Julian day(s) and place.
#' #'
#' #' @param jd Julian day number (numeric, scalar or vector)
#' #' @param place Numeric vector: (latitude, longitude, timezone)
#' #'
#' #' @return Character vector of Masa names (including "Adhika" if applicable).
#' #'
#' #' @examples
#' #' get_masa_name(2459778, c(15.34, 75.13, 5.5))
#' #' get_masa_name(c(2459778,2459779), c(15.34, 75.13, 5.5))
#' #'
#' #' @export
#' get_masa_name <- function(jd, place) {
#'   if (!is.numeric(jd)) stop("`jd` must be numeric (scalar or vector).")
#'   if (!is.numeric(place) || length(place) != 3) stop("`place` must be a numeric vector of (lat, lon, tz).")
#'
#'   masa_name_vec <- character(length(jd))
#'   for (i in seq_along(jd)) {
#'     masa_ <- masa(jd[i], place)
#'     if (is.null(masa_) || length(masa_) < 2 || is.na(masa_[1]) || masa_[1] < 1 || masa_[1] > length(masas)) {
#'       masa_name_vec[i] <- NA_character_
#'       next
#'     }
#'     masa_name <- ifelse(masa_[2] == 1, "Adhika ", "")
#'     masa_name_vec[i] <- paste0(masa_name, masas[masa_[1]])
#'   }
#'   return(masa_name_vec)
#' }
#' # ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
#' Get Masa Name(s) for Given Julian Day and Place
#'
#' @description
#' Returns the name(s) of the Masa (lunar month) for the given Julian day(s) and place.
#'
#' @param jd Julian day number (numeric, scalar or vector)
#' @param place Numeric vector: (latitude, longitude, timezone)
#'
#' @return Character vector of Masa names (including "Adhika" if applicable).
#'
#' @examples
#' get_masa_name(2459778, c(15.34, 75.13, 5.5))
#' get_masa_name(c(2459778,2459779), c(15.34, 75.13, 5.5))
#'
#' @export
get_masa_name <- function(jd, place) {
  if (!is.numeric(jd)) stop("`jd` must be numeric (scalar or vector).")
  if (!is.numeric(place) || length(place) != 3) stop("`place` must be a numeric vector of (lat, lon, tz).")

  masa_name_vec <- character(length(jd))
  for (i in seq_along(jd)) {
    masa_ <- masa(jd[i], place)
    masa_num <- masa_$masa_num
    is_leap <- masa_$is_leap_month
    if (is.null(masa_num) || is.na(masa_num) || masa_num < 1 || masa_num > length(masas)) {
      masa_name_vec[i] <- NA_character_
      next
    }
    masa_name <- ifelse(is_leap, "Adhika ", "")
    masa_name_vec[i] <- paste0(masa_name, masas[masa_num])
  }
  return(masa_name_vec)
}
# ---------------------------------------------------------------------------- #
