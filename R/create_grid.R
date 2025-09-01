#' Create a high-performance, standard-compliant spatial grid
#'
#' @description{
#' This function generates a regular spatial grid aligned to the CRS origin.
#' It combines high performance for large areas (using `sfheaders`) with a
#' flexible and robust set of features for input handling and output formatting,
#' including INSPIRE-compliant grid IDs and automatic parallel processing with `mirai` and `future` backends.}
#'
#' @param grid_extent A spatial object to define the grid's extent. Can be an
#'   `sf` or `sfc` object, a 2x2 `bbox` matrix, or a numeric vector of
#'   `c(xmin, ymin, xmax, ymax)`.
#' @param cellsize_m A single integer representing the grid cell size in metres
#'   (e.g., 1000 for a 1 km grid).
#' @param crs The coordinate reference system (CRS) for the output grid.
#'   Accepts various formats handled by `sf::st_crs()`: an integer or numeric
#'   EPSG code (e.g., `3035`), a string representation like `"epsg:3035"`, or
#'   a `crs` object. If `NULL` (default), the CRS is inherited from
#'   `grid_extent`. If `grid_extent` also lacks a CRS, the function will stop
#'   with an error.
#' @param output_type The class of the output object: `"sf_polygons"` (default) creates
#'   a spatial object with polygon geometries, `"sf_points"` creates an `sf`
#'   object with point geometries, and `"dataframe"` creates a data frame with
#'   grid cell centroid coordinates (`X_centroid`, `Y_centroid`).
#' @param clip_to_input A logical value. If `TRUE`, the grid is filtered to
#'   include only cells that intersect the `grid_extent`. This does not cut
#'   cell geometries.
#' @param use_convex_hull A logical value. If `TRUE` and `clip_to_input` is
#'   `TRUE`, the grid is clipped to the convex hull of the input geometry,
#'   which can be faster and simpler than using a complex polygon.
#' @param buffer_m A numeric value. If `clip_to_input` is `TRUE`, this specifies
#'   a buffer distance in metres to apply to the `grid_extent` geometry before
#'   clipping. Defaults to `0` (no buffer).
#' @param id_format A character string specifying which grid cell IDs to generate.
#'   Options are `"both"` (default), `"long"`, `"short"`, or `"none"`.
#' @param include_llc A logical value. If `TRUE` (default), columns for the
#'   lower-left corner coordinates (`X_LLC`, `Y_LLC`) of each cell are included
#'   in the output.
#' @param point_type A character string, used only when `output_type = "sf_points"`.
#'   Determines the location of the points: `"centroid"` (default) for the center of the cell, or `"llc"` for the lower-left corner.
#' @param parallel Controls parallel execution. Options are:
#'   \itemize{
#'     \item **`'auto'` (default):** Automatically detects and uses a configured
#'       `mirai` or `future` backend if one is available. If both are set, it
#'       prefers the one with more available workers and issues a warning. If
#'       neither is configured, it runs sequentially.
#'     \item **`TRUE`:** Forces the function to attempt parallel execution. It
#'       will raise an error if a valid parallel backend (with >1 worker) is
#'       not configured.
#'     \item **`FALSE`:** Forces the function to run in single-threaded
#'       sequential mode.
#'   }
#'   For parallelism, you must configure a backend *before* calling this
#'   function, for example: `mirai::daemons(4)` or `future::plan("multisession")`.
#'
#' @return An `sf` object (with polygon or point geometries) or a `data.frame`
#'   representing the grid, with optional columns for coordinates and
#'   INSPIRE-compliant grid IDs.
#' @export
#'
#' @examples
#' library(sf)
#' # Load the sample data from the sf package
#' nc_raw <- st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
#'
#' # Define target projected CRS and cell size
#' target_crs <- 5070 # NAD83 / Conus Albers
#' cellsize_m <- 10000 # 10 km
#'
#' # Project the data
#' nc <- st_transform(nc_raw, target_crs)
#'
#' # Create a grid covering the data
#' nc_grid <- create_grid(
#'   grid_extent = nc,
#'   cellsize_m = cellsize_m,
#'   output_type = "sf_polygons",
#'   clip_to_input = TRUE
#' )
#'
#' head(nc_grid, 3)
create_grid <- function(
  grid_extent,
  cellsize_m,
  crs = NULL,
  output_type = "sf_polygons",
  clip_to_input = FALSE,
  use_convex_hull = FALSE,
  buffer_m = 0,
  id_format = "both",
  include_llc = TRUE,
  point_type = "centroid",
  parallel = "auto"
) {
  # --- 1. Validate Arguments ---
  if (!is.character(parallel) && !is.logical(parallel)) {
    stop("'parallel' must be 'auto', TRUE, or FALSE.", call. = FALSE)
  }
  if (is.character(parallel) && parallel != "auto") {
    stop("If 'parallel' is a string, it must be 'auto'.", call. = FALSE)
  }

  backend_args <- list(
    output_type = output_type,
    clip_to_input = clip_to_input,
    use_convex_hull = use_convex_hull,
    buffer_m = buffer_m,
    id_format = id_format,
    include_llc = include_llc,
    point_type = point_type
  )

  # --- 2. Check Backend Status ---
  mirai_ready <- requireNamespace("mirai", quietly = TRUE) &&
    mirai::daemons_set()
  n_mirai <- if (mirai_ready) mirai::status()$connections else 0

  future_ready <- requireNamespace("future", quietly = TRUE) &&
    !inherits(future::plan(), "sequential")
  n_future <- if (future_ready) future::nbrOfWorkers() else 1

  use_mirai <- FALSE
  use_future <- FALSE

  # --- 3. Dispatcher Logic ---
  run_mode <- "sequential"

  if (parallel == "auto" || parallel == TRUE) {
    if (mirai_ready && future_ready && n_mirai > 0 && n_future > 1) {
      # Conflict: both are configured
      warning(
        "Both `mirai` and `future` backends are configured. ",
        "Choosing the one with more workers. It is recommended to only configure one backend.",
        call. = FALSE
      )
      if (n_mirai >= n_future) {
        use_mirai <- TRUE
      } else {
        use_future <- TRUE
      }
    } else if (mirai_ready && n_mirai > 0) {
      use_mirai <- TRUE
    } else if (future_ready && n_future > 1) {
      use_future <- TRUE
    }

    if (use_mirai || use_future) {
      run_mode <- "parallel"
    }
  }

  # --- 4. Execute Based on Mode ---
  # A. Forced Parallel Execution
  if (parallel == TRUE) {
    if (run_mode != "parallel") {
      stop(
        "parallel = TRUE, but no valid parallel backend with >1 worker was found.\n",
        "Please install the required packages ('mirai', 'future', 'furrr', etc.) and\n",
        "configure a backend, e.g., `mirai::daemons(n)` or `future::plan('multisession')`.",
        call. = FALSE
      )
    }
    # (Execution continues to section C)
  }

  # B. Forced or Fallback Sequential Execution
  if (run_mode == "sequential") {
    if (parallel == "auto") {
      # This is not a warning because 'auto' implies it's okay to fall back
      message("No parallel backend detected. Running in sequential mode.")
    } else {
      message("Running in sequential mode.")
    }

    all_args <- c(
      list(grid_extent = grid_extent, cellsize_m = cellsize_m, crs = crs),
      backend_args
    )
    return(do.call(create_grid_internal, all_args))
  }

  # C. Parallel Execution (from 'auto' or 'TRUE')
  if (run_mode == "parallel") {
    if (use_mirai) {
      message("`mirai` backend detected. Running in parallel.")
      return(run_parallel_mirai(grid_extent, cellsize_m, crs, backend_args))
    } else if (use_future) {
      message("`future` backend detected. Running in parallel.")
      return(run_parallel_future(grid_extent, cellsize_m, crs, backend_args))
    }
  }
}
