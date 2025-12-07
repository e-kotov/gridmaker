#' @rdname inspire_grid
#' @description
#' This function takes a vector of INSPIRE-compliant IDs and derives a
#' regular spatial grid from it. For generating a spatial grid from a spatial
#' extent, see \code{\link{inspire_grid_from_extent}}.
#'
#' @return An `sf` object or `data.frame` representing the grid derived from
#'   the INSPIRE IDs.
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
#' grid <- inspire_grid_from_ids(inspire)
#' plot(grid$geometry)
#'
#' # Or using the S3 generic
#' grid <- inspire_grid(inspire)
#' plot(grid$geometry)
inspire_grid_from_ids <- function(
  ids,
  point_type = c("llc", "centroid"),
  output_type = c("sf_polygons", "sf_points", "dataframe"),
  include_llc = TRUE,
  quiet = FALSE,
  ...
) {
  if (!is.logical(quiet) || length(quiet) != 1) {
    stop(
      "'quiet' must be a single logical value (TRUE or FALSE).",
      call. = FALSE
    )
  }

  backend_args <- list(point_type = point_type, output_type = output_type)
  do.call(inspire_grid_from_ids_internal, c(list(ids = ids), backend_args))
}
