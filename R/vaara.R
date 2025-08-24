#' # ---------------------------------------------------------------------------- #
#' #' vaara
#' #'
#' #' @description Vaara for a given Julian day number
#' #' @param jd Julian day number
#' #'
#' #' @return Vaara as an integer
#' #'
#' #' @examples
#' #' vaara(2459778)
#' vaara <- function(jd){
#'   return (as.integer(ceiling(jd + 1) %% 7) + 1)
#' }
#' # ---------------------------------------------------------------------------- #
#'
# ---------------------------------------------------------------------------- #
#' Get Vaara (Day of Week) for Given Julian Day(s)
#'
#' @description
#' Returns the Vaara (weekday index) for given Julian day number(s).
#'
#' @param jd Julian day number (numeric, scalar or vector)
#'
#' @return Integer vector (1–7): Vaara index (1 = Ravivar/Sunday, 2 = Somvar/Monday, ...)
#'
#' @examples
#' vaara(2459778)
#' vaara(c(2459778, 2459779))
#'
#' @export
vaara <- function(jd) {
  if (!is.numeric(jd)) stop("`jd` must be numeric (scalar or vector).")
  idx <- as.integer((ceiling(jd + 1) %% 7) + 1)
  idx[is.na(jd)] <- NA_integer_
  return(idx)
}
# ---------------------------------------------------------------------------- #
