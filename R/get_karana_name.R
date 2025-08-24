#' # ---------------------------------------------------------------------------- #
#' #' Get Karana Name(s) for a Given Date and Place
#' #'
#' #' @description
#' #' Returns the names of the two Karanas for a given Julian day number and place, joined by a separator.
#' #'
#' #' @param jd Julian day number (numeric, scalar or vector)
#' #' @param place Numeric vector: (latitude, longitude, timezone)
#' #' @param sep Character. Separator for joining Karana names (default "-")
#' #'
#' #' @return Character vector of Karana names (joined by `sep`)
#' #'
#' #' @examples
#' #' get_karana_name(2459778, c(15.34, 75.13, 5.5))
#' #' get_karana_name(c(2459778, 2459779), c(15.34, 75.13, 5.5))
#' #'
#' #' @export
#' get_karana_name <- function(jd, place, sep = "-") {
#'   if (!is.numeric(jd)) stop("`jd` must be numeric (scalar or vector).")
#'   if (!is.numeric(place) || length(place) != 3) stop("`place` must be a numeric vector of length 3 (lat, lon, tz).")
#'   # Vectorize if necessary
#'   n <- length(jd)
#'   result <- character(n)
#'   for (i in seq_along(jd)) {
#'     karana_idx <- karana(jd[i], place)
#'     # Check for proper length and valid index
#'     if (length(karana_idx) == 2 &&
#'         all(karana_idx >= 1 & karana_idx <= length(karanas))) {
#'       result[i] <- paste(karanas[karana_idx[1]], karanas[karana_idx[2]], sep = sep)
#'     } else {
#'       result[i] <- NA_character_
#'       warning(sprintf("Invalid Karana index for jd=%s", jd[i]))
#'     }
#'   }
#'   return(result)
#' }
#' # ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
#' Get Karana Name(s) for a Given Date and Place
#'
#' @description
#' Returns the names of the two Karanas for a given Julian day number and place, joined by a separator.
#'
#' @param jd Julian day number (numeric, scalar or vector)
#' @param place Numeric vector: (latitude, longitude, timezone)
#' @param sep Character. Separator for joining Karana names (default "-")
#'
#' @return Character vector of Karana names (joined by `sep`)
#'
#' @examples
#' get_karana_name(2459778, c(15.34, 75.13, 5.5))
#' get_karana_name(c(2459778, 2459779), c(15.34, 75.13, 5.5))
#'
#' @export
get_karana_name <- function(jd, place, sep = "-") {
  if (!is.numeric(jd)) stop("`jd` must be numeric (scalar or vector).")
  if (!is.numeric(place) || length(place) != 3) stop("`place` must be a numeric vector of length 3 (lat, lon, tz).")
  n <- length(jd)
  result <- character(n)
  Nkaranas <- length(karanas)
  for (i in seq_along(jd)) {
    karana_idx <- karana(jd[i], place)
    # Handle wrapping and non-integer edge cases
    karana_idx <- ((as.integer(karana_idx) - 1) %% Nkaranas) + 1
    if (length(karana_idx) == 2 &&
        all(!is.na(karana_idx)) &&
        all(karana_idx >= 1 & karana_idx <= Nkaranas)) {
      result[i] <- paste(karanas[karana_idx[1]], karanas[karana_idx[2]], sep = sep)
    } else {
      result[i] <- NA_character_
      warning(sprintf("Invalid Karana index for jd=%s", jd[i]))
    }
  }
  return(result)
}
# ---------------------------------------------------------------------------- #
