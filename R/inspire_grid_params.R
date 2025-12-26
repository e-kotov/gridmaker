#' Common parameters for INSPIRE grid functions
#'
#' @description
#' This is a documentation template containing common parameter definitions
#' shared across inspire_grid functions. This documentation is not exported
#' and exists solely to provide @inheritParams targets.
#'
#' @param crs The coordinate reference system (CRS) for the output grid.
#'   Accepts various formats handled by `sf::st_crs()`: an integer or numeric
#'   EPSG code (e.g., `3035`), a string representation like `"epsg:3035"`, or
#'   a `crs` object. If `NULL` (default), the CRS is inherited from
#'   the spatial input. If the input also lacks a CRS, the function will stop
#'   with an error.
#' @param id_format A character string specifying which grid cell IDs to generate.
#'   Options are `"both"` (default), `"long"`, `"short"`, or `"none"`.
#' @param axis_order A character string specifying the coordinate order for the
#'   output Short INSPIRE IDs. This parameter is **only used when `id_format` is
#'   `"short"` or `"both"`**. It can be one of:
#'   \itemize{
#'     \item `"NE"` (the default) to produce the format `{cellsize}N{y}E{x}`.
#'     \item `"EN"` to produce the format `{cellsize}E{x}N{y}` (e.g. this format is used in [Danish national grid](https://www.dst.dk/en/TilSalg/produkter/geodata/kvadratnet)).
#'   }
#' @param include_llc A logical value. If `TRUE` (default), columns for the
#'   lower-left corner coordinates (`X_LLC`, `Y_LLC`) of each cell are included
#'   in the output.
#' @param quiet Logical value. If `TRUE`, all progress messages and progress bars are suppressed. Defaults to `FALSE`.
#' @param dsn The destination for the output grid. For sf objects, this is passed to
#'   `sf::st_write`. For `spatraster` output, this uses `terra::writeRaster`.
#'   This can be a file path (e.g., `"path/to/grid.gpkg"` for vector data or
#'   `"path/to/grid.tif"` for raster data) or a database connection string.
#'   If \code{dsn} is provided, the grid is written to the specified location
#'   instead of being returned as an object.
#' @param layer The name of the grid layer, passed directly to `sf::st_write`.
#'   Its interpretation depends on the destination driver. For a GeoPackage
#'   file, this will be the layer name. If \code{dsn} is a file path and `layer` is
#'   not specified, it defaults to the file's base name.
#' @param ... Additional arguments passed to backend handlers.
#'   When writing to text files (e.g., .csv, .tsv) via \code{dsn}, these arguments are passed to \code{\link[readr]{write_delim}} (e.g., \code{na = "NA"}, \code{quote = "all"}).
#'   When writing to spatial files via \code{dsn}, these are passed to \code{\link[sf]{st_write}}.
#'   For \code{output_type = "spatraster"} writing, these are passed to \code{\link[terra]{writeRaster}}.
#'   For streaming backends (`mirai` or sequential), this can include \code{max_cells_per_chunk} to control memory usage.
#'
#' @name inspire_grid_params
#' @keywords internal
NULL
