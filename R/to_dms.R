#' # ---------------------------------------------------------------------------- #
#' #' to_dms
#' #'
#' #' @description Convert decimal degrees to degrees, minutes, and seconds
#' #'
#' #' @param deg Degrees as a decimal number
#' #'
#' #' @return A vector containing degrees, minutes and seconds
#' #'
#' #' @examples
#' #' to_dms(30.263888889)
#' to_dms <- function(deg){
#'   d = as.integer(deg)
#'   mins = (deg - d) * 60
#'   m = as.integer(mins)
#'   s = as.integer(round((mins - m) * 60))
#'   return (c(d, m, s))
#' }
#' # ---------------------------------------------------------------------------- #


# ---------------------------------------------------------------------------- #
#' Convert Decimal Degrees to Degrees, Minutes, Seconds (DMS)
#'
#' @description
#' Converts decimal degrees to degrees, minutes, and seconds.
#'
#' @param deg Degrees as a decimal number (scalar or vector).
#'
#' @return
#' If scalar: named integer vector (deg, min, sec).
#' If vector: data.frame with columns `deg`, `min`, `sec`.
#'
#' @examples
#' to_dms(30.263888889)
#' to_dms(c(30.263888889, -10.75))
#'
#' @export
to_dms <- function(deg) {
  if (!is.numeric(deg)) stop("`deg` must be numeric (scalar or vector).")
  sign_deg <- sign(deg)
  deg_abs <- abs(deg)
  d <- as.integer(deg_abs)
  mins <- (deg_abs - d) * 60
  m <- as.integer(mins)
  s <- as.integer(round((mins - m) * 60))
  # Handle rounding overflow (e.g., 59.999999 rounds to 60)
  over <- s == 60
  if (any(over, na.rm = TRUE)) {
    s[over] <- 0
    m[over] <- m[over] + 1
    over2 <- m == 60
    if (any(over2, na.rm = TRUE)) {
      m[over2] <- 0
      d[over2] <- d[over2] + 1
    }
  }
  # Restore sign to degrees
  d <- d * sign_deg
  if (length(deg) == 1) {
    return(c(deg = d, min = m, sec = s))
  } else {
    return(data.frame(deg = d, min = m, sec = s))
  }
}
# ---------------------------------------------------------------------------- #
