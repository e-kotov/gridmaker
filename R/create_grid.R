#' Create a standard-compliant spatial grid with INSPIRE IDs
#'
#' @description{
#' This function generates a regular spatial grid aligned to the CRS origin.
#' It combines high performance for large areas (using `sfheaders`) with a
#' flexible and robust set of features for input handling and output formatting,
#' including INSPIRE-compliant grid IDs and automatic parallel processing with
#' `mirai` and `future` backends. When `dsn` is provided, the grid is written
#' directly to a file and the function returns `NULL` invisibly.}
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
#' @param dsn The destination for the output grid, passed directly to
#'   `sf::st_write`. This can be a file path (e.g., `"path/to/grid.gpkg"`)
#'   or a database connection string. If `dsn` is provided, the grid is
#'   written to the specified location instead of being returned as an object.
#' @param layer The name of the grid layer, passed directly to `sf::st_write`.
#'   Its interpretation depends on the destination driver. For a GeoPackage
#'   file, this will be the layer name. If `dsn` is a file path and `layer` is
#'   not specified, it defaults to the file's base name.
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
#' @param quiet logical value. If ‘TRUE’, all progress messages and progress bars are suppressed. Defaults to ‘FALSE’.
#' @param ... Additional arguments passed to specific backend handlers. For
#'   streaming backends (`mirai` or sequential), this can include
#'   `max_cells_per_chunk` to control memory usage.
#'
#' @return If `dsn` is `NULL` (the default), an `sf` object or `data.frame`
#'   representing the grid. If `dsn` is specified, the function writes the grid
#'   to a file and returns `invisible(NULL)`.
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
  parallel = "auto",
  quiet = FALSE,
  dsn = NULL,
  layer = NULL,
  ...
) {
  # --- 1. Validate Arguments ---
  if (!is.character(parallel) && !is.logical(parallel)) {
    stop("'parallel' must be 'auto', TRUE, or FALSE.", call. = FALSE)
  }
  if (is.character(parallel) && parallel != "auto") {
    stop("If 'parallel' is a string, it must be 'auto'.", call. = FALSE)
  }
  if (!is.logical(quiet) || length(quiet) != 1) {
    stop(
      "'quiet' must be a single logical value (TRUE or FALSE).",
      call. = FALSE
    )
  }

  # If dsn is provided but layer is not, derive from dsn
  if (!is.null(dsn) && is.null(layer)) {
    layer <- tools::file_path_sans_ext(basename(dsn))
    if (!quiet) {
      message("`layer` not specified, defaulting to '", layer, "'.")
    }
  }

  # Only check if generating an in-memory object (dsn is NULL)
  if (is.null(dsn)) {
    available_gb <- .get_ram_gb("available")

    # Proceed only if we could get available RAM
    if (!is.null(available_gb)) {
      estimated_gb <- .estimate_grid_memory_gb(
        grid_extent = grid_extent,
        cellsize_m = cellsize_m,
        crs = crs,
        output_type = output_type,
        id_format = id_format,
        include_llc = include_llc,
        point_type = point_type
      )

      if (estimated_gb > available_gb) {
        warning(
          "Estimated grid size is ~",
          round(estimated_gb, 1),
          " GB, ",
          "which may exceed your available system memory of ~",
          available_gb,
          " GB.\n",
          "  Consider writing the grid directly to disk by providing the 'dsn' and 'layer' arguments.",
          call. = FALSE
        )
      }
    }
  }

  backend_args <- list(
    output_type = output_type,
    clip_to_input = clip_to_input,
    use_convex_hull = use_convex_hull,
    buffer_m = buffer_m,
    id_format = id_format,
    include_llc = include_llc,
    point_type = point_type,
    ...
  )
  parallel_backend_args <- c(backend_args, list(quiet = quiet))

  # --- 2. Check Backend Status ---
  mirai_ready <- requireNamespace("mirai", quietly = TRUE) &&
    mirai::daemons_set()
  n_mirai <- if (mirai_ready) mirai::status()$connections else 0

  future_ready <- requireNamespace("future", quietly = TRUE) &&
    !inherits(future::plan(), "sequential")
  n_future <- if (future_ready) future::nbrOfWorkers() else 1

  use_mirai <- FALSE
  use_future <- FALSE
  run_mode <- "sequential"

  if (parallel == "auto" || parallel == TRUE) {
    if (mirai_ready && future_ready && n_mirai > 0 && n_future > 1) {
      warning(
        "Both `mirai` and `future` backends are configured...",
        call. = FALSE
      )
      if (n_mirai >= n_future) use_mirai <- TRUE else use_future <- TRUE
    } else if (mirai_ready && n_mirai > 0) {
      use_mirai <- TRUE
    } else if (future_ready && n_future > 1) {
      use_future <- TRUE
    }
    if (use_mirai || use_future) run_mode <- "parallel"
  }

  if (parallel == TRUE && run_mode != "parallel") {
    stop(
      "parallel = TRUE, but no valid parallel backend was found...",
      call. = FALSE
    )
  }

  # --- 3. DISPATCHER LOGIC ---

  # --- A. WRITING TO DISK ---
  if (!is.null(dsn)) {
    # Use the highly efficient mirai stream if available
    if (use_mirai) {
      if (!quiet) {
        message(
          "`mirai` backend detected. Running in parallel (streaming to disk)."
        )
      }
      return(async_stream_to_disk_with_mirai(
        grid_extent = grid_extent,
        cellsize_m = cellsize_m,
        crs = crs,
        dsn = dsn,
        layer = layer,
        dot_args = backend_args,
        quiet = quiet
      ))
    } else {
      # Otherwise, use the safe, sequential streaming method
      if (!quiet) {
        message(
          "No parallel backend detected. Running in sequential mode (streaming to disk)."
        )
      }
      return(stream_to_disk_sequential(
        grid_extent = grid_extent,
        cellsize_m = cellsize_m,
        crs = crs,
        dsn = dsn,
        layer = layer,
        dot_args = backend_args,
        quiet = quiet
      ))
    }
  }

  # --- B. GENERATING IN-MEMORY ---
  if (run_mode == "parallel") {
    if (use_mirai) {
      if (!quiet) {
        message("`mirai` backend detected. Running in parallel (in-memory).")
      }
      return(run_parallel_mirai(
        grid_extent,
        cellsize_m,
        crs,
        parallel_backend_args
      ))
    } else if (use_future) {
      if (!quiet) {
        message("`future` backend detected. Running in parallel (in-memory).")
      }
      return(run_parallel_future(
        grid_extent,
        cellsize_m,
        crs,
        parallel_backend_args
      ))
    }
  }

  # Fallback to sequential in-memory generation
  if (!quiet) {
    if (parallel == "auto") {
      message(
        "No parallel backend detected. Running in sequential mode. See ?create_grid for details how to enable parallel processing to speed up large jobs."
      )
    } else {
      message("Running in sequential mode.")
    }
  }
  all_args <- c(
    list(grid_extent = grid_extent, cellsize_m = cellsize_m, crs = crs),
    backend_args
  )
  return(do.call(create_grid_internal, all_args))
}
