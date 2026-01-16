#' Convert INSPIRE IDs between long and short formats
#'
#' @description
#' This function converts a vector of INSPIRE-compliant grid IDs from their long
#' format to the short format, or vice-versa. It automatically detects the
#' input format and is fully vectorized to handle large inputs efficiently.
#'
#' The long format is `CRS{epsg}RES{cellsize}mN{y}E{x}`. The short format can be
#' either `{cellsize}N{y}E{x}` or `{cellsize}E{x}N{y}`.
#'
#' @param ids A character vector of INSPIRE IDs. All IDs in the vector must be
#'   of the same format (either all long or all short).
#' @param crs An integer representing the EPSG code. This parameter is **only
#'   used when converting from the short format to the long format**. It
#'   defaults to `3035` (ETRS89-LAEA).
#' @param axis_order A character string specifying the coordinate order for the
#'   output. This parameter is **only used when converting from the long format
#'   to the short format**. It can be one of:
#'   \itemize{
#'     \item `"NE"` (the default) to produce the format `{cellsize}N{y}E{x}`.
#'     \item `"EN"` to produce the format `{cellsize}E{x}N{y}`.
#'   }
#'
#' @return A character vector of the converted INSPIRE IDs.
#' @export
#'
#' @examples
#' long_ids <- c("CRS3035RES1000mN2684000E4334000", "CRS3035RES10000mN2700000E4400000")
#' short_ids_ne <- c("1kmN2684E4334", "10kmN270E440")
#' short_ids_en <- c("1kmE4334N2684", "10kmE440N270")
#'
#' # --- Long to Short ---
#'
#' # Convert long to short with default "NE" order
#' inspire_id_format(long_ids)
#'
#' # Convert long to short with specified "EN" order
#' inspire_id_format(long_ids, axis_order = "EN")
#'
#' # --- Short to Long ---
#'
#' # Convert short ("NE" format) to long with default CRS (3035)
#' inspire_id_format(short_ids_ne)
#'
#' # The function also correctly parses the "EN" format
#' inspire_id_format(short_ids_en)
#'
#' # Override the CRS when converting short to long
#' inspire_id_format(short_ids_ne, crs = 3857)
#'
inspire_id_format <- function(ids, crs = 3035, axis_order = "NE") {
  # 1. Handle empty or invalid input
  if (length(ids) == 0) {
    return(character(0))
  }
  if (!is.character(ids)) {
    stop("'ids' must be a character vector.", call. = FALSE)
  }

  # 2. Check for mixed formats and determine input type
  is_long <- startsWith(ids, "CRS")
  if (!all(is_long) && any(is_long)) {
    stop(
      "Input contains a mix of long and short INSPIRE ID formats.",
      call. = FALSE
    )
  }
  is_long_format <- is_long[1]

  # 3. Perform conversion based on format
  if (is_long_format) {
    # --- CONVERT LONG TO SHORT (Vectorized) ---
    axis_order <- match.arg(axis_order, choices = c("NE", "EN"))

    parsed <- utils::strcapture(
      "^CRS[0-9]+RES([0-9]+)mN([0-9]+)E([0-9]+)$",
      ids,
      proto = list(cellsize = numeric(), y = numeric(), x = numeric())
    )

    res_str <- ifelse(
      parsed$cellsize >= 1000,
      paste0(parsed$cellsize / 1000, "km"),
      paste0(parsed$cellsize, "m")
    )

    # Use robust divisor logic (10^tz_count) consistent with grid generation
    divisors <- vapply(parsed$cellsize, function(cs) {
      10^.tz_count(cs)
    }, FUN.VALUE = numeric(1))

    y_short <- parsed$y / divisors
    x_short <- parsed$x / divisors

    out <- if (axis_order == "NE") {
      sprintf("%sN%.0fE%.0f", res_str, y_short, x_short)
    } else {
      # "EN"
      sprintf("%sE%.0fN%.0f", res_str, x_short, y_short)
    }
    out[is.na(parsed$cellsize)] <- NA_character_
    out
  } else {
    # --- CONVERT SHORT TO LONG (Vectorized) ---
    # The 'axis_order' argument is ignored; we detect both NE and EN formats.

    # First pass: try to parse the "NE" format
    parsed <- utils::strcapture(
      "^([0-9.]+k?m)N([0-9.]+)E([0-9.]+)$",
      ids,
      proto = list(res_str = character(), y = numeric(), x = numeric())
    )

    # Second pass: for IDs that failed, try parsing the "EN" format
    failed_idx <- which(is.na(parsed$res_str))
    if (length(failed_idx) > 0) {
      # Note the swapped order of x and y in the prototype list
      parsed_en <- utils::strcapture(
        "^([0-9.]+k?m)E([0-9.]+)N([0-9.]+)$",
        ids[failed_idx],
        proto = list(res_str = character(), x = numeric(), y = numeric())
      )
      # Fill in the failed slots from the first pass with results from the second
      if (nrow(parsed_en) > 0) {
        parsed[failed_idx, ] <- parsed_en[, names(parsed)]
      }
    }

    # Strict parsing of units to avoid errors like "100mm" -> 100
    is_km <- endsWith(parsed$res_str, "km")
    is_m <- endsWith(parsed$res_str, "m")
    
    numeric_res <- rep(NA_real_, length(parsed$res_str))
    
    # Handle 'km' suffix
    km_idx <- which(is_km)
    if (length(km_idx) > 0) {
      # Remove last 2 chars ("km")
      val_str <- substr(parsed$res_str[km_idx], 1, nchar(parsed$res_str[km_idx]) - 2)
      numeric_res[km_idx] <- as.numeric(val_str) * 1000
    }
    
    # Handle 'm' suffix (excluding those that are 'km')
    m_idx <- which(is_m & !is_km)
    if (length(m_idx) > 0) {
      # Remove last 1 char ("m")
      val_str <- substr(parsed$res_str[m_idx], 1, nchar(parsed$res_str[m_idx]) - 1)
      numeric_res[m_idx] <- as.numeric(val_str)
    }
    
    cellsize_m <- numeric_res

    # Use robust multiplier logic
    multipliers <- vapply(cellsize_m, function(cs) {
      10^.tz_count(cs)
    }, FUN.VALUE = numeric(1))

    y_long <- parsed$y * multipliers
    x_long <- parsed$x * multipliers

    out <- sprintf("CRS%.0fRES%.0fmN%.0fE%.0f", crs, cellsize_m, y_long, x_long)
    out[is.na(cellsize_m)] <- NA_character_
    out
  }
}
