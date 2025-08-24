#' # ---------------------------------------------------------------------------- #
#' #' Get Nakshatra Name(s) and Ending Time(s) for a Given Julian Day and Place
#' #'
#' #' @description
#' #' Returns the name(s) of Nakshatra(s) and their ending times for the given Julian day and place.
#' #'
#' #' @param jd Julian day number (numeric, scalar or vector)
#' #' @param place Numeric vector: (latitude, longitude, timezone)
#' #'
#' #' @return A tibble/data.frame with columns: `nakshatra`, `end_hour`, `end_min`, `end_sec`
#' #'         If `jd` is a vector, returns a list of data.frames (one per `jd`).
#' #'
#' #' @examples
#' #' get_nakshatra_name(2459778, c(15.34, 75.13, 5.5))
#' #' get_nakshatra_name(c(2459778, 2459779), c(15.34, 75.13, 5.5))
#' #'
#' #' @export
#' get_nakshatra_name <- function(jd, place) {
#'   if (!is.numeric(jd)) stop("`jd` must be numeric (scalar or vector).")
#'   if (!is.numeric(place) || length(place) != 3) stop("`place` must be a numeric vector of length 3 (lat, lon, tz).")
#'
#'   parse_nakshatra_block <- function(nblock) {
#'     out <- list()
#'     size <- length(nblock) / 4
#'     for (i in seq_len(size)) {
#'       idx <- (i - 1) * 4 + 1
#'       n_idx <- nblock[idx]
#'       hour <- nblock[idx + 1]
#'       min <- nblock[idx + 2]
#'       sec <- nblock[idx + 3]
#'       name <- if (!is.na(n_idx) && n_idx >= 1 && n_idx <= length(nakshatras)) nakshatras[n_idx] else NA_character_
#'       out[[i]] <- data.frame(
#'         nakshatra = name,
#'         end_hour = hour,
#'         end_min = min,
#'         end_sec = sec,
#'         stringsAsFactors = FALSE
#'       )
#'     }
#'     dplyr::bind_rows(out)
#'   }
#'
#'   res_list <- lapply(jd, function(jd1) {
#'     nblock <- nakshatra(jd1, place)
#'     if (is.null(nblock) || length(nblock) == 0) return(NULL)
#'     parse_nakshatra_block(nblock)
#'   })
#'
#'   # If only one JD, return data frame, else list
#'   if (length(jd) == 1) {
#'     return(res_list[[1]])
#'   } else {
#'     names(res_list) <- as.character(jd)
#'     return(res_list)
#'   }
#' }
#' # ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
#' Get Nakshatra Name(s) and Ending Time(s) for a Given Julian Day and Place
#'
#' @description
#' Returns the name(s) of Nakshatra(s) and their ending times for the given Julian day and place.
#'
#' @param jd Julian day number (numeric, scalar or vector)
#' @param place Numeric vector: (latitude, longitude, timezone)
#'
#' @return A data.frame with columns: `nakshatra`, `end_hour`, `end_min`, `end_sec`
#'         If `jd` is a vector, returns a named list of data.frames (one per `jd`).
#'
#' @examples
#' get_nakshatra_name(2459778, c(15.34, 75.13, 5.5))
#' get_nakshatra_name(c(2459778, 2459779), c(15.34, 75.13, 5.5))
#'
#' @export
get_nakshatra_name <- function(jd, place) {
  if (!is.numeric(jd)) stop("`jd` must be numeric (scalar or vector).")
  if (!is.numeric(place) || length(place) != 3) stop("`place` must be a numeric vector of length 3 (lat, lon, tz).")

  parse_nakshatra_block <- function(nblock) {
    # nblock is the named list returned by modern nakshatra()
    out <- list()
    # Today's nakshatra
    n_idx <- nblock$today_nakshatra
    end_time <- nblock$end_time
    name <- if (!is.na(n_idx) && n_idx >= 1 && n_idx <= length(nakshatras)) nakshatras[n_idx] else NA_character_
    out[[1]] <- data.frame(
      nakshatra = name,
      end_hour = end_time["hour"],
      end_min = end_time["min"],
      end_sec = end_time["sec"],
      stringsAsFactors = FALSE
    )
    # Leap nakshatra, if present
    leap_idx <- nblock$leap_nakshatra
    if (!is.null(leap_idx) && !is.na(leap_idx) && leap_idx >= 1 && leap_idx <= length(nakshatras)) {
      leap_time <- nblock$leap_end_time
      leap_name <- nakshatras[leap_idx]
      out[[2]] <- data.frame(
        nakshatra = leap_name,
        end_hour = leap_time["hour"],
        end_min = leap_time["min"],
        end_sec = leap_time["sec"],
        stringsAsFactors = FALSE
      )
    }
    dplyr::bind_rows(out)
  }

  res_list <- lapply(jd, function(jd1) {
    nblock <- nakshatra(jd1, place)
    if (is.null(nblock) || length(nblock) == 0) return(NULL)
    parse_nakshatra_block(nblock)
  })

  # If only one JD, return data frame, else named list
  if (length(jd) == 1) {
    return(res_list[[1]])
  } else {
    names(res_list) <- as.character(jd)
    return(res_list)
  }
}
# ---------------------------------------------------------------------------- #
