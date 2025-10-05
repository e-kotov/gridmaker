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
#' {cellsize}N{y}E{x}         # short format}
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
#' @returns \code{z22_inspire_generate} returns a character vector containing
#'   the INSPIRE identifiers.
#'
#'   \code{z22_inspire_extract} returns a dataframe or
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
#' gen <- inspire_generate(coords, llc = TRUE)
#' ext <- inspire_extract(gen)[c("x", "y")]
#' identical(ext, coords)
#'
#' # Extract coordinates from short ID strings
#' inspire_extract("100mN34000E44000")
#'
#' # Generate IDs from an sf dataframe
#' if (requireNamespace("sf", quietly = TRUE)) {
#'   coords <- sf::st_as_sf(coords, coords = c("x", "y"))
#'   inspire_generate(coords)
#' }
inspire_generate <- function(
  coords,
  cellsize_m = NULL,
  short = FALSE,
  llc = FALSE,
  tolerance = 1e-6,
  sample = 2000
) {
  if (inherits(coords, c("sf", "sfc"))) {
    coords <- sf::st_transform(coords, 3035)
    coords <- sf::st_coordinates(coords)
  }

  if (is.matrix(coords)) {
    coords <- as.data.frame(coords)
  }

  colnames(coords) <- tolower(colnames(coords))
  x <- coords[["x"]]
  y <- coords[["y"]]

  if (is.null(x) || is.null(y)) {
    x <- coords[[1]]
    y <- coords[[2]]
  }

  if (is.null(cellsize_m)) {
    cellsize_m <- guess_resolution(x, y, tolerance = tolerance, sample = sample)
  }

  if (!llc) {
    x <- x - cellsize_m / 2
    y <- y - cellsize_m / 2
  }

  if (short) {
    x <- trunc(x / cellsize_m)
    y <- trunc(y / cellsize_m)
    res <- m_to_res(cellsize_m)
    sprintf("%sN%.0fE%.0f", res, y, x)
  } else {
    sprintf("CRS3035RES%.0fmN%.0fE%.0f", cellsize_m, y, x)
  }
}


guess_resolution <- function(x, y, sample = 2000, tolerance = 1e-6) {
  x <- x[seq(1, sample)] # take a sample
  y <- y[seq(1, sample)]
  sx <- sort(unique(x))
  sy <- sort(unique(y))
  diff_x <- diff(sx)
  diff_y <- diff(sy)
  res_x <- diff_x[1]
  res_y <- diff_y[1]

  if (length(sx) < 2 && length(sy) < 2) {
    stop("Not enough coordinates to form a grid.")
  }

  un_x <- all(abs(diff_x - diff_x[1]) < tolerance)
  un_y <- all(abs(diff_y - diff_y[1]) < tolerance)

  if (!un_x || !un_y) {
    stop("Coordinates have non-uniform spacing in X and/or Y dimensions.")
  }

  if (!is.na(res_x) && !is.na(res_y) && !res_x == res_y) {
    stop("Coordinates form an anisotropic grid. X and Y coordinates have a different resolution.")
  }

  res_x
}
