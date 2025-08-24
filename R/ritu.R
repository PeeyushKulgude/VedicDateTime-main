#' # ---------------------------------------------------------------------------- #
#' #' ritu
#' #'
#' #' @param masa_num Number associated with a Masa
#' #'
#' #' @return Number associated with the Ritu
#' #'
#' #' @examples
#' #' ritu(2)
#' ritu <- function(masa_num){
#'   return (((masa_num[1] - 1) %/% 2) + 1)
#' }
#' # ---------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------- #
#' Ritu (Season Index) for a Given Masa Number
#'
#' @description
#' Returns the index of the Ritu (season) for given Masa number(s).
#'
#' @param masa_num Masa number (integer, scalar or vector, 1–12)
#'
#' @return Integer vector: Ritu index (1–6; NA for invalid)
#'
#' @examples
#' ritu(2)          # 1 = Vasanta
#' ritu(7:12)       # 4 = Sharad, 5 = Hemanta, 6 = Sishira
#'
#' @export
ritu <- function(masa_num) {
  masa_num <- as.integer(masa_num)
  out <- rep(NA_integer_, length(masa_num))
  mask <- !is.na(masa_num) & masa_num >= 1 & masa_num <= 12
  out[mask] <- ((masa_num[mask] - 1) %/% 2) + 1
  return(out)
}
# ---------------------------------------------------------------------------- #
