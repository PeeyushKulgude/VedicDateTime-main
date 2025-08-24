# ---------------------------------------------------------------------------- #
#' Calculate Elapsed Years in Vedic Calendars
#'
#' @description
#' Computes the elapsed years for Kali, Saka, and Vikram Samvat for a given Julian Day and month (maasa).
#'
#' @param jd Julian day number (numeric scalar or vector)
#' @param maasa_num Numeric vector (or scalar): index (1–12) for lunar month (maasa).
#'
#' @return
#' If scalar input: a named list with entries `kali`, `saka`, `vikrama`.
#' If vectorized: data.frame with columns `jd`, `maasa_num`, `kali`, `saka`, `vikrama`.
#'
#' @examples
#' elapsed_year(2459778, 2)
#' elapsed_year(c(2459778,2459779), 2)
#' elapsed_year(2459778, 1:3)
#'
#' @export
elapsed_year <- function(jd, maasa_num) {
  sidereal_year <- 365.25636

  if (!is.numeric(jd)) stop("`jd` must be numeric (scalar or vector).")
  if (!is.numeric(maasa_num)) stop("`maasa_num` must be numeric (scalar or vector).")
  if (any(maasa_num < 1 | maasa_num > 12, na.rm = TRUE)) warning("`maasa_num` should be in 1:12 for Vedic lunar months.")

  n <- max(length(jd), length(maasa_num))
  jd <- rep(jd, length.out = n)
  maasa_num <- rep(maasa_num, length.out = n)

  kali <- saka <- vikrama <- numeric(n)
  for (i in seq_len(n)) {
    ahar <- ahargana(jd[i])
    kali[i] <- as.integer((ahar + (4 - maasa_num[i]) * 30) / sidereal_year)
    saka[i] <- kali[i] - 3179
    vikrama[i] <- saka[i] + 135
  }
  if (n == 1) {
    return(list(kali = kali[1], saka = saka[1], vikrama = vikrama[1]))
  } else {
    return(data.frame(jd = jd, maasa_num = maasa_num, kali = kali, saka = saka, vikrama = vikrama))
  }
}
# ---------------------------------------------------------------------------- #
