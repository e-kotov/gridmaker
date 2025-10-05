#' Generate INSPIRE IDs
#' @description
#'   Given pairs of coordinates, generates their INSPIRE grid representation.
#'   Given INSPIRE identifiers, can also extract the X and Y coordinates.
#'
#'   An INSPIRE ID contains information about the CRS, cell size and the
#'   ETRS89-LAEA coordinates of the lower-left corner of the grid cell in its
#'   format.
#'
#'   \preformatted{CRS3035{cellsize}mN{y}E{x} # long format
#'     {cellsize}N{y}E{x}         # short format (NE order)
#'     {cellsize}E{x}N{y}         # short format (EN order)}
#'
#'   The long format always uses meters while the short format aggregates
#'   cell sizes greater or equal to 1000m to km.
#'
#' @param coords A list, matrix, or dataframe where the X and Y coordinates are
#'   either in the columns \code{"x"} and \code{"y"} or in the first and second
#'   column position, respectively. Column names are converted to lowercase.
#'
#'   Can also be a \code{sf}/\code{sfc} object in which case the coordinates are
#'   extracted using \code{\link[sf]{st_coordinates}}.
#' @param short If \code{TRUE}, generates short INSPIRE ID. Defaults to
#'   \code{FALSE}.
#' @param axis_order A character string specifying the coordinate order for the
#'   output. This parameter is **only used when `short = TRUE`**. It can be one of:
#'   \itemize{
#'     \item `"NE"` (the default) to produce the format `{cellsize}N{y}E{x}`.
#'     \item `"EN"` to produce the format `{cellsize}E{x}N{y}`.
#'   }
#' @param llc Do the coordinates in \code{coords} represent the lower-left
#'   corners of their cells? If \code{FALSE}, subtracts each coordinate by
#'   half of \code{cellsize_m}. If \code{TRUE}, leaves them as-is. Defaults
#'   to \code{FALSE}, i.e., treat coordinates as cell centroids.
#' @param tolerance If \code{res} is \code{NULL}, controls the maximum
#'   acceptable difference between calculated cell spacings to consider them
#'   uniform. Defaults to \code{1e-6}.
#' @param sample If \code{res} is \code{NULL}, specifies the number of points
#'   to guess a resolution from. Defaults to 2000 to keep performance high.
#'   Increase this value if you are uncertain about the quality of your data.
#' @inheritParams create_grid
#'
#' @returns \code{inspire_generate} returns a character vector containing
#'   the INSPIRE identifiers.
#'
#'   \code{inspire_extract} returns a dataframe or
#'   \code{\link[sf:st_sf]{sf}} dataframe (if \code{as_sf = TRUE}) containing
#'   the points extracted from the INSPIRE identifiers and information about
#'   the CRS and cell sizes. Note that the returned coordinates are always
#'   the centers of the grid cells as opposed to the lower-left corners.
#' @export
#'
#' @name inspire
#'
#' @examples
#' # Generate IDs from a dataframe
#' coords <- data.frame(x = c(4334100, 4334200), y = 2684000)
#' gen <- inspire_generate(coords, llc = TRUE, cellsize_m = 100)
#' ext <- inspire_extract(gen)[c("x", "y")]
#' # Note: inspire_extract gives cell centers, so this won't be identical if llc=TRUE
#'
#' # Generate long format IDs
#' inspire_generate(coords, llc = TRUE, cellsize_m = 100)
#'
#' # Generate short format IDs with default "NE" axis order
#' inspire_generate(coords, llc = TRUE, cellsize_m = 1000, short = TRUE)
#'
#' # Generate short format IDs with "EN" axis order
#' inspire_generate(coords, llc = TRUE, cellsize_m = 1000, short = TRUE, axis_order = "EN")
#'
#' # Extract coordinates from short ID strings
#' inspire_extract("100mN34000E44000", crs = 3035)
#'
#' # Generate IDs from an sf dataframe
#' if (requireNamespace("sf", quietly = TRUE)) {
#'   coords_df <- data.frame(x = c(4334100, 4334200), y = 2684000)
#'   coords_sf <- sf::st_as_sf(coords_df, coords = c("x", "y"), crs = 3035)
#'   inspire_generate(coords_sf, cellsize_m = 1000)
#' }
inspire_generate <- function(
  coords,
  cellsize_m = NULL,
  short = FALSE,
  axis_order = "NE",
  llc = FALSE,
  tolerance = 1e-6,
  sample = 2000
) {
  if (inherits(coords, c("sf", "sfc"))) {
    if (nrow(coords) == 0) {
      stop("Input 'coords' cannot be empty (0 rows).", call. = FALSE)
    }
    coords <- sf::st_transform(coords, 3035)
    coords <- sf::st_coordinates(coords)
  }

  if (is.matrix(coords)) {
    if (nrow(coords) == 0) {
      stop("Input 'coords' cannot be empty (0 rows).", call. = FALSE)
    }
    coords <- as.data.frame(coords)
  }

  if (nrow(coords) == 0) {
    stop("Input 'coords' cannot be empty (0 rows).", call. = FALSE)
  }

  colnames(coords) <- tolower(colnames(coords))
  x <- coords[["x"]]
  y <- coords[["y"]]

  if (is.null(x) || is.null(y)) {
    x <- coords[[1]]
    y <- coords[[2]]
  }

  if (any(is.na(x)) || any(is.na(y))) {
    stop("Input 'coords' contains NA values in the coordinates.", call. = FALSE)
  }

  if (is.null(cellsize_m)) {
    cellsize_m <- guess_resolution(x, y, tolerance = tolerance, sample = sample)
  }

  if (!llc) {
    x <- x - cellsize_m / 2
    y <- y - cellsize_m / 2
  }

  if (short) {
    # Validate the new argument
    axis_order <- match.arg(axis_order, choices = c("NE", "EN"))

    x_short <- trunc(x / cellsize_m)
    y_short <- trunc(y / cellsize_m)
    res <- m_to_res(cellsize_m)

    # Conditionally format the output string based on axis_order
    if (axis_order == "NE") {
      sprintf("%sN%.0fE%.0f", res, y_short, x_short)
    } else {
      # "EN"
      sprintf("%sE%.0fN%.0f", res, x_short, y_short)
    }
  } else {
    sprintf("CRS3035RES%.0fmN%.0fE%.0f", cellsize_m, y, x)
  }
}

guess_resolution <- function(x, y, sample = 2000, tolerance = 1e-6) {
  # Guard against sampling more elements than exist
  n_total <- length(x)
  if (n_total < sample) {
    sample <- n_total
  }

  # Sample from indices to maintain x-y correspondence if ever needed
  sample_indices <- seq(1, n_total, length.out = sample)
  x <- x[sample_indices]
  y <- y[sample_indices]

  sx <- sort(unique(x))
  sy <- sort(unique(y))
  diff_x <- diff(sx)
  diff_y <- diff(sy)

  if (length(diff_x) == 0 && length(diff_y) == 0) {
    # This can happen if there's only one unique coordinate pair
    stop(
      "Cannot determine resolution: not enough unique coordinates in the sample to form a grid.",
      call. = FALSE
    )
  }

  # Use the median of the differences to be more robust to outliers
  res_x <- if (length(diff_x) > 0) stats::median(diff_x) else NA
  res_y <- if (length(diff_y) > 0) stats::median(diff_y) else NA

  # Determine final resolution, preferring a valid value if one exists
  final_res <- if (!is.na(res_x)) res_x else res_y
  if (is.na(final_res)) {
    stop(
      "Cannot determine resolution from the provided coordinates.",
      call. = FALSE
    )
  }

  # Check for uniform spacing and anisotropy based on the determined resolution
  if (!is.na(res_x) && any(abs(diff_x - res_x) > tolerance)) {
    stop(
      "Coordinates have non-uniform spacing in the X dimension.",
      call. = FALSE
    )
  }
  if (!is.na(res_y) && any(abs(diff_y - res_y) > tolerance)) {
    stop(
      "Coordinates have non-uniform spacing in the Y dimension.",
      call. = FALSE
    )
  }
  if (!is.na(res_x) && !is.na(res_y) && abs(res_x - res_y) > tolerance) {
    stop(
      "Coordinates form an anisotropic grid. X and Y coordinates have a different resolution.",
      call. = FALSE
    )
  }

  final_res
}
