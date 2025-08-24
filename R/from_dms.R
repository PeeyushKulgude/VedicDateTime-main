# ---------------------------------------------------------------------------- #
#' Convert Degrees, Minutes, Seconds to Decimal Degrees
#'
#' @description
#' Converts degrees, minutes, and seconds to decimal degrees.
#'
#' @param degs Degrees (numeric, scalar or vector)
#' @param mins Minutes (numeric, scalar or vector, default = 0)
#' @param secs Seconds (numeric, scalar or vector, default = 0)
#'
#' @return Numeric vector of decimal degrees.
#'
#' @examples
#' from_dms(30, 15, 50)     # 30.26389
#' from_dms(c(30,31), 0, 0) # c(30, 31)
#' from_dms(30, 30)         # 30.5 (seconds default 0)
#' from_dms(-10, 30, 0)     # -9.5
#'
#' @export
from_dms <- function(degs, mins = 0, secs = 0) {
  if (!is.numeric(degs) || !is.numeric(mins) || !is.numeric(secs)) {
    stop("All arguments must be numeric (scalars or vectors of equal length, or will be recycled).")
  }
  # Recycle lengths
  n <- max(length(degs), length(mins), length(secs))
  degs <- rep(degs, length.out = n)
  mins <- rep(mins, length.out = n)
  secs <- rep(secs, length.out = n)
  # Check for negative minutes or seconds (optional)
  if (any(mins < 0 | secs < 0, na.rm = TRUE)) {
    warning("Negative values in 'mins' or 'secs' are not standard in DMS notation.")
  }
  dec <- degs + mins / 60 + secs / 3600
  return(dec)
}
# ---------------------------------------------------------------------------- #
