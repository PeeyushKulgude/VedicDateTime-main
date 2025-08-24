#' # ---------------------------------------------------------------------------- #
#' #' jd_to_gregorian
#' #'
#' #' @description Convert Julian day number to Gregorian date
#' #'
#' #' @param jd Julian day number
#' #'
#' #' @return Gregorian date
#' #'
#' #' @examples
#' #' jd_to_gregorian(2459778)
#' jd_to_gregorian <- function(jd){
#'   return (swephR::swe_revjul(jd, swephR::SE$GREG_CAL))
#' }
#' # ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
#' Convert Julian Day Number(s) to Gregorian Date(s)
#'
#' @description Convert Julian day number(s) to Gregorian calendar date(s).
#'
#' @param jd Julian day number (numeric, scalar or vector)
#'
#' @return For scalar input: named list (year, month, day, hour).
#'         For vector input: data.frame with columns year, month, day, hour.
#'
#' @examples
#' jd_to_gregorian(2459778)
#' jd_to_gregorian(c(2459778, 2459779))
#'
#' @export
jd_to_gregorian <- function(jd) {
  if (!is.numeric(jd)) stop("`jd` must be numeric (scalar or vector).")
  if (length(jd) == 1) {
    return(swephR::swe_revjul(jd, swephR::SE$GREG_CAL))
  } else {
    res <- lapply(jd, function(x) swephR::swe_revjul(x, swephR::SE$GREG_CAL))
    # convert list of lists to data.frame
    df <- as.data.frame(do.call(rbind, lapply(res, function(x)
      unlist(x, use.names = FALSE))))
    names(df) <- c("year", "month", "day", "hour")
    rownames(df) <- NULL
    return(df)
  }
}
# ---------------------------------------------------------------------------- #
