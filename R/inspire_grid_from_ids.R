#' @rdname inspire_grid
#' @description
#' This function takes a vector of INSPIRE-compliant IDs and derives a
#' regular spatial grid from it. For generating a spatial grid from a spatial
#' extent, see \code{\link{inspire_grid_from_extent}}.
#'
#' @param ids A character vector of INSPIRE-compliant grid cell IDs
#'   (e.g., `"CRS3035RES100000mN26E43"`).
#' @param point_type A character string determining the location of the points
#'   when `output_type = "sf_points"`: `"centroid"` for the center of the cell,
#'   or `"llc"` for the lower-left corner. Default is `"llc"`.
#' @param output_type The class of the output object: `"sf_polygons"` (default),
#'   `"sf_points"`, or `"dataframe"`.
#' @param include_llc A logical value. If `TRUE` (default), columns for the
#'   lower-left corner coordinates (`X_LLC`, `Y_LLC`) of each cell are included
#'   in the output.
#' @param id_format A character string specifying which grid cell IDs to include.
#'   Options are `"both"` (default), `"long"`, `"short"`, or `"none"`.
#' @param axis_order A character string specifying the coordinate order for output
#'   Short INSPIRE IDs (only used when `id_format` is `"short"` or `"both"`):
#'   `"NE"` (default) for `{cellsize}N{y}E{x}` format, or `"EN"` for
#'   `{cellsize}E{x}N{y}` format.
#' @param quiet Logical value. If `TRUE`, all progress messages are suppressed. Defaults to `FALSE`.
#' @param dsn The destination for the output grid. If provided, the grid is written
#'   to this file path instead of being returned as an object.
#' @param layer The name of the grid layer when writing to file (e.g., for GeoPackage).
#'   If not specified and `dsn` is a file path, defaults to the file's base name.
#' @param ... Additional arguments passed to backend handlers or to \code{\link[sf]{st_write}}
#'   when writing to file.
#'
#' @return An `sf` object or `data.frame` representing the grid derived from
#'   the INSPIRE IDs. If \code{dsn} is specified, returns \code{invisible(dsn)}.
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
  id_format = c("both", "long", "short"),
  axis_order = c("NE", "EN"),
  quiet = FALSE,
  dsn = NULL,
  layer = NULL,
  ...
) {
  if (!is.logical(quiet) || length(quiet) != 1) {
    stop(
      "'quiet' must be a single logical value (TRUE or FALSE).",
      call. = FALSE
    )
  }

  # Capture dots and merge
  dots <- list(...)
  backend_args <- c(
    list(
      point_type = point_type,
      output_type = output_type,
      include_llc = include_llc,
      id_format = id_format,
      axis_order = axis_order,
      dsn = dsn,
      layer = layer,
      quiet = quiet
    ),
    dots
  )

  do.call(inspire_grid_from_ids_internal, c(list(ids = ids), backend_args))
}
