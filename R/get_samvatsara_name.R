# ---------------------------------------------------------------------------- #
#' Get Shaka Samvatsara Name(s) for Given Julian Day and Maasa Number
#'
#' @description
#' Returns the name(s) of the Shaka Samvatsar (year name) for the given Julian day number(s) and maasa number(s).
#'
#' @param jd Julian day number (numeric, scalar or vector)
#' @param maasa_num Maasa number (numeric, scalar or vector)
#'
#' @return Character vector of Samvatsara names (NA for invalid/NA inputs).
#'
#' @examples
#' get_samvatsara_name(2459778, 2)
#' get_samvatsara_name(c(2459778, 2459779), 2)
#' get_samvatsara_name(2459778, 1:3)
#'
#' @export
get_samvatsara_name <- function(jd, maasa_num) {
  if (!is.numeric(jd) || !is.numeric(maasa_num)) {
    stop("`jd` and `maasa_num` must be numeric (scalars or vectors).")
  }
  n <- max(length(jd), length(maasa_num))
  jd <- rep(jd, length.out = n)
  maasa_num <- rep(maasa_num, length.out = n)
  idx <- samvatsara(jd, maasa_num)
  name_vec <- ifelse(
    !is.na(idx) & idx >= 1 & idx <= length(samvatsars),
    samvatsars[idx],
    NA_character_
  )
  return(name_vec)
}
# ---------------------------------------------------------------------------- #
