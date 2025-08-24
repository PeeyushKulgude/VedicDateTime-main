# ---------------------------------------------------------------------------- #
#' Calculate Ahargana (days elapsed since Vedic epoch)
#'
#' Computes the Ahargana, or elapsed days since the traditional Vedic epoch, given a Julian Day number.
#'
#' @param jd Julian day number (numeric vector or scalar).
#'
#' @return Numeric vector of Ahargana values.
#'
#' @examples
#' ahargana(2459778)
#' ahargana(c(2459778, 2459779))
#' \dontrun{ahargana("invalid")}  # Will throw an error
#' ahargana(swephR::swe_julday(2022,7,14,0,swephR::SE$GREG_CAL))
ahargana <- function(jd){
  if (!is.numeric(jd)) {
    stop("Input 'jd' must be numeric (scalar or vector).")
  }
  return(jd - 588465.5)
}
# ---------------------------------------------------------------------------- #
