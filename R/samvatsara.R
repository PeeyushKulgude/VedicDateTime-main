# ---------------------------------------------------------------------------- #
#' Get Shaka Samvatsara Index for Given Julian Day and Maasa Number
#'
#' @description
#' Returns the number (1–60) associated with the Shaka Samvatsar (year) for given Julian day and maasa number.
#'
#' @param jd Julian day number (numeric, scalar or vector)
#' @param maasa_num Maasa number (numeric, scalar or vector)
#'
#' @return Integer: Samvatsara index (1–60; NA if invalid input).
#'
#' @examples
#' samvatsara(2459778, 2)
#'
#' @export
samvatsara <- function(jd, maasa_num) {
  if (!is.numeric(jd) || !is.numeric(maasa_num))
    stop("`jd` and `maasa_num` must be numeric.")
  n <- max(length(jd), length(maasa_num))
  jd <- rep(jd, length.out = n)
  maasa_num <- rep(maasa_num, length.out = n)
  kali <- vapply(seq_len(n), function(i) elapsed_year(jd[i], maasa_num[i])$kali, numeric(1L))
  # Apply formula only for kali >= 4009 (per tradition)
  ifelse(kali >= 4009,
         {k <- (kali - 14) %% 60},
         {k <- kali})
  samvat <- (k + 27 + as.integer((k * 211 - 108) / 18000)) %% 60
  # R uses 0-indexing with modulo; adjust to 1:60 for human convention
  samvat <- samvat %% 60
  samvat[samvat == 0] <- 60
  return(samvat)
}
# ---------------------------------------------------------------------------- #
