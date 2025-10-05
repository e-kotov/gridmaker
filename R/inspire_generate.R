#' Generate INSPIRE IDs
#' @description
#' Given pairs of coordinates, generates their INSPIRE grid representation.
#' Given INSPIRE identifiers, can also extract the X and Y coordinates.
#'
#' An INSPIRE ID contains information about the CRS, cell size and the
#' ETRS89-LAEA coordinates of the south-west corner of the grid cell in its
#' format. Only the relevant first digits are used in place of the full
#' coordinates. In case of \code{res = "100km"}, these are the first two
#' digits, for \code{res = "100m"} the first five digits.
#'
#' \preformatted{CRS3035{cellsize}mN{y}E{x} # long format
#' {cellsize}N{y}E{x}         # short format}
#'
#' The long format always uses meters while the short format aggregates
#' cell sizes greater or equal to 1000m to km.
#'
#' @param coords A list, matrix, or dataframe where the X and Y coordinates are
#' either in the columns \code{"x"} and \code{"y"} or in the first and second
#' column position, respectively. Column names are converted to lowercase.
#'
#' Can also be a \code{sf}/\code{sfc} object in which case the coordinates are
#' extracted using \code{\link[sf]{st_coordinates}}.
#' @param res Resolution of the grid. Can be \code{"100m"}, \code{"250m"},
#' \code{"1km"}, \code{"5km"}, \code{"10km"}, or \code{"100km"}. If
#' \code{NULL}, tries to guess the resolution from the provided coordinates.
#' @param short If \code{TRUE}, generates short INSPIRE ID. Defaults to
#' \code{FALSE}.
#' @returns \code{z22_inspire_generate} returns a character vector containing
#' the INSPIRE identifiers. \code{z22_inspire_extract} returns a dataframe
#' or \code{\link[sf:st_sfc]{sfc}} object containing the points extracted from
#' the INSPIRE identifiers. Note that the returned coordinates are always
#' the centers of the grid cells as opposed to the south-west corners.
#' @export
#'
#' @details
#' To remain fast even for huge grid datasets, the function is just a very
#' simple \code{\link{sprintf}} wrapper that performs no input checks. To
#' produce valid INSPIRE identifiers, make sure to transform your data to
#' ETRS89-LAEA (e.g. using
#' \code{\link[sf:st_transform]{st_transform}(..., 3035)}). You should also
#' make sure that the coordinates are the south-west corner of existing
#' INSPIRE grid cells.
#'
#' @name inspire
#'
#' @examples
#' # Generate IDs from a dataframe
#' coords <- data.frame(x = c(4334150, 4334250), y = c(2684050, 2684050))
#' identical(inspire_extract(inspire_generate(coords))[c("x", "y")], coords)
#'
#' # Extract coordinates from short ID strings
#' inspire_extract("100mN34000E44000")
#'
#' # Generate IDs from an sf dataframe
#' if (requireNamespace("sf", quietly = TRUE)) {
#'   coords <- sf::st_as_sf(coords, coords = c("x", "y"))
#'   inspire_generate(coords)
#' }
inspire_generate <- function(coords, res = NULL, short = FALSE) {
  if (inherits(coords, c("sf", "sfc"))) {
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

  if (is.null(res)) {
    res_m <- guess_resolution(x, y)
  } else {
    res_m <- res_to_m(res)
  }

  # Check if the numeric resolution is valid. This also catches NAs from "abc".
  valid_resolutions_m <- res_to_m(grid_reses)
  if (any(!res_m %in% valid_resolutions_m, na.rm = TRUE) || anyNA(res_m)) {
    stop(
      "'res' must be one of the standard INSPIRE resolutions: ",
      paste(grid_reses, collapse = ", ")
    )
  }

  if (short) {
    x_trunc <- trunc(x / res_m)
    y_trunc <- trunc(y / res_m)
    res_str <- m_to_res(res_m)
    generated_ids <- sprintf("%sN%sE%s", res_str, y_trunc, x_trunc)
  } else {
    res_str <- format(res_m, scientific = FALSE)
    generated_ids <- sprintf("CRS3035RES%smN%.0fE%.0f", res_str, y, x)
  }

  generated_ids[is.na(x) | is.na(y)] <- NA_character_

  generated_ids
}
