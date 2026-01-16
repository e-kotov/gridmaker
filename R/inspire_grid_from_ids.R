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
#' @param output_type The class of the output object: `"sf_polygons"` (default) creates
#'   a spatial object with polygon geometries, `"sf_points"` creates an `sf`
#'   object with point geometries, `"dataframe"` creates a data frame with
#'   grid cell centroid coordinates (`X_centroid`, `Y_centroid`), and
#'   `"spatraster"` creates a `terra::SpatRaster` object with grid cell IDs
#'   stored as factor levels (Raster Attribute Table).
#'   **Note:** `"spatraster"` is only supported by `inspire_grid_from_extent()`,
#'   not by `inspire_grid_from_ids()`.
#' @inheritParams inspire_grid_params
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
  quiet = getOption("gridmaker.quiet", FALSE),
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
