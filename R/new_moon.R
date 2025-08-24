#' # ---------------------------------------------------------------------------- #
#' #' Find Julian Day of New Moon Nearest to a Given Date and Tithi
#' #'
#' #' @description
#' #' Returns the Julian day number representing the new moon day (Amavasya) nearest to a given date and tithi.
#' #'
#' #' @param jd Julian day number (numeric scalar).
#' #' @param tithi_ Number associated with the tithi (numeric scalar).
#' #' @param opt Integer: +1 for next new moon, -1 for previous (default: -1).
#' #'
#' #' @return Julian day number of new moon (NA if not found).
#' #'
#' #' @examples
#' #' new_moon(2459778, 2)
#' #' new_moon(2459778, tithi(2459778, c(15.34, 75.13, 5.5)))
#' #'
#' #' @export
#' new_moon <- function(jd, tithi_, opt = -1) {
#'   if (!is.numeric(jd) || length(jd) != 1) stop("`jd` must be a numeric scalar.")
#'   if (!is.numeric(tithi_) || length(tithi_) != 1) stop("`tithi_` must be a numeric scalar.")
#'   if (!opt %in% c(-1, 1)) stop("`opt` must be +1 (next new moon) or -1 (previous new moon).")
#'
#'   start <- if (opt == -1) {
#'     jd - tithi_
#'   } else {
#'     jd + (30 - tithi_)
#'   }
#'
#'   x <- seq(-2, 2, by = 0.25)
#'   y <- sapply(x, function(dx) lunar_phase(start + dx))
#'   y <- unwrap_angles(y)
#'   y0 <- inverse_lagrange(x, y, 360)
#'   return(start + y0)
#' }
#' # ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
#' Find Julian Day of New Moon Nearest to a Given Date and Tithi
#'
#' @description
#' Returns the Julian day number representing the new moon day (Amavasya) nearest to a given date and tithi.
#'
#' @param jd Julian day number (numeric scalar).
#' @param tithi_ Number associated with the tithi (numeric scalar).
#' @param opt Integer: +1 for next new moon, -1 for previous (default: -1).
#'
#' @return Julian day number of new moon, or NA if not found/converged.
#'
#' @examples
#' new_moon(2459778, 2)
#' new_moon(2459778, tithi(2459778, c(15.34, 75.13, 5.5))$today_tithi)
#'
#' @export
new_moon <- function(jd, tithi_, opt = -1) {
  if (!is.numeric(jd) || length(jd) != 1) stop("`jd` must be a numeric scalar.")
  if (!is.numeric(tithi_) || length(tithi_) != 1) stop("`tithi_` must be a numeric scalar.")
  if (!opt %in% c(-1, 1)) stop("`opt` must be +1 (next new moon) or -1 (previous new moon).")

  start <- if (opt == -1) {
    jd - tithi_
  } else {
    jd + (30 - tithi_)
  }

  x <- seq(-2, 2, by = 0.25)
  y <- sapply(x, function(dx) lunar_phase(start + dx))
  y <- unwrap_angles(y)

  y0 <- tryCatch(
    inverse_lagrange(x, y, 360),
    error = function(e) NA_real_
  )
  result <- start + y0
  if (is.na(result)) warning("New moon could not be found or interpolation failed.")
  return(result)
}
# ---------------------------------------------------------------------------- #
