#' Convert INSPIRE IDs between long and short formats
#'
#' @description
#' This function converts a vector of INSPIRE-compliant grid IDs from their long
#' format to the short format, or vice-versa. It automatically detects the
#' input format and is fully vectorized to handle large inputs efficiently.
#'
#' The long format is structured as `CRS{epsg}RES{cellsize}mN{y}E{x}`, while the
#' short format is `{cellsize}N{y}E{x}`.
#'
#' @param ids A character vector of INSPIRE IDs. All IDs in the vector must be
#'   of the same format (either all long or all short).
#' @param crs An integer representing the EPSG code of the coordinate reference
#'   system. This is only used when converting from the short format to the long
#'   format, as the CRS is not contained in the short ID. Defaults to `3035`
#'   (ETRS89-LAEA).
#'
#' @return A character vector of the converted INSPIRE IDs.
#' @export
#' @name inspire
#'
#' @examples
#' long_ids <- c("CRS3035RES1000mN2684000E4334000", "CRS3035RES10000mN2700000E4400000")
#' short_ids <- c("1kmN2684E4334", "10kmN270E440")
#'
#' # Convert long to short
#' inspire_convert(long_ids)
#'
#' # Convert short to long
#' inspire_convert(short_ids)
#'
#' # Convert short to long with a different CRS
#' inspire_convert(short_ids, crs = 3857)
inspire_convert <- function(ids, crs = 3035) {
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
    parsed <- utils::strcapture(
      "^CRS[0-9]+RES([0-9]+)mN([0-9]+)E([0-9]+)$",
      ids,
      proto = list(cellsize = numeric(), y = numeric(), x = numeric())
    )

    # Vectorized conversion of cellsize (m) to resolution string (km or m)
    res_str <- ifelse(
      parsed$cellsize >= 1000,
      paste0(parsed$cellsize / 1000, "km"),
      paste0(parsed$cellsize, "m")
    )

    y_short <- parsed$y / parsed$cellsize
    x_short <- parsed$x / parsed$cellsize

    sprintf("%sN%.0fE%.0f", res_str, y_short, x_short)
  } else {
    # --- CONVERT SHORT TO LONG (Vectorized) ---
    parsed <- utils::strcapture(
      "^([0-9]+(k?m))N([0-9]+)E([0-9]+)$",
      ids,
      proto = list(
        res_str = character(),
        cellsize_str = character(),
        y = numeric(),
        x = numeric()
      )
    )

    # Vectorized conversion of resolution string to cellsize in meters
    is_km <- endsWith(parsed$res_str, "km")
    numeric_res <- as.numeric(gsub("k?m", "", parsed$res_str))
    cellsize_m <- ifelse(is_km, numeric_res * 1000, numeric_res)

    y_long <- parsed$y * cellsize_m
    x_long <- parsed$x * cellsize_m

    sprintf("CRS%.0fRES%.0fmN%.0fE%.0f", crs, cellsize_m, y_long, x_long)
  }
}
