# ---------------------------------------------------------------------------- #
#' Get Ritu (Season) Name(s) for Given Masa Number(s)
#'
#' @description
#' Returns the name(s) of the Ritu (season) for the given Masa number(s).
#'
#' @param masa_num Numeric scalar or vector: Masa index (typically 1–12).
#'
#' @return Character vector of Ritu names (NA for invalid/NA inputs).
#'
#' @examples
#' get_ritu_name(2)
#' get_ritu_name(1:12)
#'
#' @export
get_ritu_name <- function(masa_num) {
  if (!is.numeric(masa_num)) stop("`masa_num` must be numeric (scalar or vector).")
  idx <- ritu(masa_num)
  name_vec <- ifelse(
    !is.na(idx) & idx >= 1 & idx <= length(ritus),
    ritus[idx],
    NA_character_
  )
  return(name_vec)
}
# ---------------------------------------------------------------------------- #
