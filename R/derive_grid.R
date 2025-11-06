#' Convert INSPIRE IDs into a spatial grid
#'
#' @description{
#' This function takes a vector of INSPIRE-compliant IDs and derives a
#' regular spatial grid from it. For generating a spatial grid from a spatial
#' extent, see \code{\link{create_grid}}.}
#' @param ids A vector containing character strings of INSPIRE-compliant IDs.
#'   Can be either short or long INSPIRE IDs.
#' @param point_type A character string, used only when `output_type = "sf_points"`.
#'   Determines the location of the points: `"centroid"` for the center
#'   of the cell, or `"llc"` (default) for the lower-left corner.
#'
#' @inherit create_grid
#'
#' @export
#'
#' @examples
#' library(sf)
#'
#' inspire <- c(
#'   "CRS3035RES100000mN26E43", "CRS3035RES100000mN26E44",
#'   "CRS3035RES100000mN27E41", "CRS3035RES100000mN27E42",
#'   "CRS3035RES100000mN27E43", "CRS3035RES100000mN27E44"
#' )
#'
#' grid <- derive_grid(inspire)
#' plot(grid$geometry)
derive_grid <- function(
    ids,
    point_type = c("llc", "centroid"),
    output_type = c("sf_polygons", "sf_points", "dataframe"),
    include_llc = TRUE,
    parallel = FALSE,
    quiet = FALSE
) {
  if (!isFALSE(parallel)) {
    warning("Parallel processing is not yet supported for `derive_grid()`.")
  }

  if (!is.logical(quiet) || length(quiet) != 1) {
    stop(
      "'quiet' must be a single logical value (TRUE or FALSE).",
      call. = FALSE
    )
  }

  backend_args <- list(point_type = point_type, output_type = output_type)
  do.call(derive_grid_internal, c(list(ids = ids), backend_args))
}
