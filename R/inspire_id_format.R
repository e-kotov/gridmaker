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

  # 3. Perform conversion based on format
  # Delegate to C++ implementation for maximum performance (20x faster than R, takes ~50ms for 100k IDs, vs ~1s in R)
  axis_order <- match.arg(axis_order, choices = c("NE", "EN"))
  convert_inspire_ids_rcpp(ids, crs, axis_order)
}
