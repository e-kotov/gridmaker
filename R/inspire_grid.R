#' Create or Reconstruct an INSPIRE Grid
#'
#' @description
#' Generates a standard-compliant spatial grid aligned to the CRS origin.
#' This function acts as a unified interface:
#' \itemize{
#'   \item If \code{x} is a spatial object (extent), it creates a new grid (calling \code{\link{inspire_grid_from_extent}}).
#'   \item If \code{x} is a character vector (INSPIRE IDs), it reconstructs the grid (calling \code{\link{inspire_grid_from_ids}}).
#' }
#'
#' It combines high performance for large areas (using `sfheaders`) with a
#' flexible and robust set of features for input handling and output formatting,
#' including INSPIRE-compliant grid IDs and automatic parallel processing with
#' `mirai` and `future` backends.
#'
#' @param x The main input object: either a spatial object (extent) or a character vector (INSPIRE IDs).
#' @param cellsize_m A single integer representing the grid cell size in metres
#'   (e.g., 1000 for a 1 km grid). Required for spatial inputs.
#' @param output_type The class of the output object: `"sf_polygons"` (default) creates
#'   a spatial object with polygon geometries, `"sf_points"` creates an `sf`
#'   object with point geometries, `"dataframe"` creates a data frame with
#'   grid cell centroid coordinates (`X_centroid`, `Y_centroid`), and
#'   `"spatraster"` creates a `terra::SpatRaster` object with grid cell IDs
#'   stored as factor levels (Raster Attribute Table).
#' @param clip_to_input A logical value. If `TRUE`, the grid is filtered to
#'   include only cells that intersect the spatial input. This does not cut
#'   cell geometries.
#' @param use_convex_hull A logical value. If `TRUE` and `clip_to_input` is
#'   `TRUE`, the grid is clipped to the convex hull of the input geometry,
#'   which can be faster and simpler than using a complex polygon.
#' @param buffer_m A numeric value. If `clip_to_input` is `TRUE`, this specifies
#'   a buffer distance in metres to apply to the spatial input before clipping.
#'   Defaults to `0` (no buffer).
#' @param point_type A character string, used only when `output_type = "sf_points"`.
#'   Determines the location of the points: `"centroid"` for the center of the cell, or \code{"llc"} for the lower-left corner. Default is \code{"llc"} for \code{inspire_grid_from_ids()}, and \code{"centroid"} for \code{inspire_grid.character()}.
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
#'   function, for example: `mirai::daemons(8)` or `future::plan("multisession", workers = 8)`.
#'   **Performance tip:** Benchmarks show 8 workers provide optimal performance for most
#'   grid sizes. Using >32 workers typically decreases performance due to overhead.
#'   The function automatically limits active workers for small grids to minimize overhead:
#'   <50k cells use max 4 workers, <500k cells use max 8 workers, <2M cells use max 16 workers.
#'   This automatic limiting can be overridden by setting `options(gridmaker.tile_multiplier)`.
#'   **Note:** Parallel processing is not supported when `output_type = "spatraster"`.
#'   Raster output will always run sequentially.
#' @param max_memory_gb A numeric value. Maximum memory in gigabytes to use for grid creation. Default is `NULL`, in which case there is an automatic limit based on **available free system memory** (not total system RAM). Using this argument allows manual override, which is recommended on certain HPC (High Performance Computing) systems where jobs are allocated a fixed amount of memory that is less than the total free memory of the allocated node.
#' @inheritParams inspire_grid_params
#'
#' @return If \code{dsn} is \code{NULL} (the default), an \code{sf} object, \code{data.frame},
#'   or \code{SpatRaster} representing the grid. If \code{dsn} is specified, the function writes
#'   the grid to a file and returns \code{invisible(dsn)}.
#'
#' @export
inspire_grid <- function(
  x,
  cellsize_m = NULL,
  crs = NULL,
  output_type = "sf_polygons",
  clip_to_input = FALSE,
  use_convex_hull = FALSE,
  buffer_m = 0,
  id_format = "both",
  axis_order = "NE",
  include_llc = TRUE,
  point_type = "centroid",
  parallel = "auto",
  quiet = FALSE,
  dsn = NULL,
  layer = NULL,
  max_memory_gb = NULL,
  include_rat = FALSE,
  ...
) {
  # Validate output_type early
  valid_output_types <- c("sf_polygons", "sf_points", "dataframe", "spatraster")
  if (!output_type %in% valid_output_types) {
    stop(
      sprintf(
        "Invalid output_type '%s'. Must be one of: %s",
        output_type,
        paste(valid_output_types, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  UseMethod("inspire_grid")
}

#' @export
#' @rdname inspire_grid
inspire_grid.sf <- function(
  x,
  cellsize_m = NULL,
  crs = NULL,
  output_type = "sf_polygons",
  clip_to_input = FALSE,
  use_convex_hull = FALSE,
  buffer_m = 0,
  id_format = "both",
  axis_order = "NE",
  include_llc = TRUE,
  point_type = "centroid",
  parallel = "auto",
  quiet = FALSE,
  dsn = NULL,
  layer = NULL,
  max_memory_gb = NULL,
  include_rat = FALSE,
  ...
) {
  inspire_grid_from_extent(
    grid_extent = x,
    cellsize_m = cellsize_m,
    crs = crs,
    output_type = output_type,
    clip_to_input = clip_to_input,
    use_convex_hull = use_convex_hull,
    buffer_m = buffer_m,
    id_format = id_format,
    axis_order = axis_order,
    include_llc = include_llc,
    point_type = point_type,
    parallel = parallel,
    quiet = quiet,
    dsn = dsn,
    layer = layer,
    max_memory_gb = max_memory_gb,
    include_rat = include_rat,
    ...
  )
}

#' @export
#' @rdname inspire_grid
inspire_grid.sfc <- function(
  x,
  cellsize_m = NULL,
  crs = NULL,
  output_type = "sf_polygons",
  clip_to_input = FALSE,
  use_convex_hull = FALSE,
  buffer_m = 0,
  id_format = "both",
  axis_order = "NE",
  include_llc = TRUE,
  point_type = "centroid",
  parallel = "auto",
  quiet = FALSE,
  dsn = NULL,
  layer = NULL,
  max_memory_gb = NULL,
  include_rat = FALSE,
  ...
) {
  inspire_grid_from_extent(
    grid_extent = x,
    cellsize_m = cellsize_m,
    crs = crs,
    output_type = output_type,
    clip_to_input = clip_to_input,
    use_convex_hull = use_convex_hull,
    buffer_m = buffer_m,
    id_format = id_format,
    axis_order = axis_order,
    include_llc = include_llc,
    point_type = point_type,
    parallel = parallel,
    quiet = quiet,
    dsn = dsn,
    layer = layer,
    max_memory_gb = max_memory_gb,
    include_rat = include_rat,
    ...
  )
}

#' @export
#' @rdname inspire_grid
inspire_grid.bbox <- function(
  x,
  cellsize_m = NULL,
  crs = NULL,
  output_type = "sf_polygons",
  clip_to_input = FALSE,
  use_convex_hull = FALSE,
  buffer_m = 0,
  id_format = "both",
  axis_order = "NE",
  include_llc = TRUE,
  point_type = "centroid",
  parallel = "auto",
  quiet = FALSE,
  dsn = NULL,
  layer = NULL,
  max_memory_gb = NULL,
  include_rat = FALSE,
  ...
) {
  inspire_grid_from_extent(
    grid_extent = x,
    cellsize_m = cellsize_m,
    crs = crs,
    output_type = output_type,
    clip_to_input = clip_to_input,
    use_convex_hull = use_convex_hull,
    buffer_m = buffer_m,
    id_format = id_format,
    axis_order = axis_order,
    include_llc = include_llc,
    point_type = point_type,
    parallel = parallel,
    quiet = quiet,
    dsn = dsn,
    layer = layer,
    max_memory_gb = max_memory_gb,
    include_rat = include_rat,
    ...
  )
}

#' @export
#' @rdname inspire_grid
inspire_grid.numeric <- function(
  x,
  cellsize_m = NULL,
  crs = NULL,
  output_type = "sf_polygons",
  clip_to_input = FALSE,
  use_convex_hull = FALSE,
  buffer_m = 0,
  id_format = "both",
  axis_order = "NE",
  include_llc = TRUE,
  point_type = "centroid",
  parallel = "auto",
  quiet = FALSE,
  dsn = NULL,
  layer = NULL,
  max_memory_gb = NULL,
  include_rat = FALSE,
  ...
) {
  inspire_grid_from_extent(
    grid_extent = x,
    cellsize_m = cellsize_m,
    crs = crs,
    output_type = output_type,
    clip_to_input = clip_to_input,
    use_convex_hull = use_convex_hull,
    buffer_m = buffer_m,
    id_format = id_format,
    axis_order = axis_order,
    include_llc = include_llc,
    point_type = point_type,
    parallel = parallel,
    quiet = quiet,
    dsn = dsn,
    layer = layer,
    max_memory_gb = max_memory_gb,
    include_rat = include_rat,
    ...
  )
}

#' @export
#' @rdname inspire_grid
inspire_grid.matrix <- function(
  x,
  cellsize_m = NULL,
  crs = NULL,
  output_type = "sf_polygons",
  clip_to_input = FALSE,
  use_convex_hull = FALSE,
  buffer_m = 0,
  id_format = "both",
  axis_order = "NE",
  include_llc = TRUE,
  point_type = "centroid",
  parallel = "auto",
  quiet = FALSE,
  dsn = NULL,
  layer = NULL,
  max_memory_gb = NULL,
  include_rat = FALSE,
  ...
) {
  inspire_grid_from_extent(
    grid_extent = x,
    cellsize_m = cellsize_m,
    crs = crs,
    output_type = output_type,
    clip_to_input = clip_to_input,
    use_convex_hull = use_convex_hull,
    buffer_m = buffer_m,
    id_format = id_format,
    axis_order = axis_order,
    include_llc = include_llc,
    point_type = point_type,
    parallel = parallel,
    quiet = quiet,
    dsn = dsn,
    layer = layer,
    max_memory_gb = max_memory_gb,
    include_rat = include_rat,
    ...
  )
}

#' @export
#' @rdname inspire_grid
inspire_grid.character <- function(
  x,
  # --- Must match Generic Order Exactly ---
  cellsize_m = NULL, # Ignored (Sink)
  crs = NULL, # Used
  output_type = "sf_polygons", # Used
  clip_to_input = FALSE, # Ignored (Sink)
  use_convex_hull = FALSE, # Ignored (Sink)
  buffer_m = 0, # Ignored (Sink)
  id_format = "both", # Used
  axis_order = "NE", # Used
  include_llc = TRUE, # Used
  point_type = "llc", # Used
  parallel = "auto", # Ignored (Sink)
  quiet = FALSE, # Used
  dsn = NULL, # Used
  layer = NULL, # Used
  max_memory_gb = NULL, # Ignored (Sink)
  include_rat = FALSE, # Ignored (Sink)
  ...
) {
  # 1. Guardrails: Warn if specific ignored arguments are provided
  # Note: We check if they are explicitly non-NULL/TRUE, distinct from defaults
  if (
    !is.null(cellsize_m) ||
      isTRUE(clip_to_input) ||
      isTRUE(use_convex_hull) ||
      buffer_m != 0 ||
      parallel != "auto" ||
      !is.null(max_memory_gb) ||
      isTRUE(include_rat)
  ) {
    warning(
      "Arguments 'cellsize_m', 'clip_to_input', 'use_convex_hull', 'buffer_m', ",
      "'parallel', 'max_memory_gb', and 'include_rat' are ignored for INSPIRE ID reconstruction.",
      call. = FALSE
    )
  }

  # 2. Call the internal function
  # We explicitly pass the *relevant* arguments found above.
  # We do NOT pass the *ignored* arguments (like cellsize_m).
  inspire_grid_from_ids(
    ids = x,
    crs = crs,
    point_type = point_type,
    output_type = output_type,
    include_llc = include_llc,
    id_format = id_format,
    axis_order = axis_order,
    quiet = quiet,
    dsn = dsn,
    layer = layer,
    ...
  )
}

#' @rdname inspire_grid
#' @export
#'
#' @param grid_extent The spatial object defining the extent. Can be an \code{sf} object,
#'   \code{sfc} geometry collection, \code{bbox}, \code{numeric} vector (as c(xmin, ymin, xmax, ymax)),
#'   or \code{matrix}.
#' @inheritParams inspire_grid
#'
#' @details
#' This function creates a spatial grid aligned to the CRS origin, with support for
#' clipping to input geometries, parallel processing, and multiple output formats.
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
#' nc_grid <- inspire_grid_from_extent(
#'   grid_extent = nc,
#'   cellsize_m = cellsize_m,
#'   output_type = "sf_polygons",
#'   clip_to_input = TRUE
#' )
#'
#' # Or using the S3 generic
#' nc_grid <- inspire_grid(nc, cellsize_m = cellsize_m, clip_to_input = TRUE)
#'
#' head(nc_grid, 3)
inspire_grid_from_extent <- function(
  grid_extent,
  cellsize_m,
  crs = NULL,
  output_type = "sf_polygons",
  clip_to_input = FALSE,
  use_convex_hull = FALSE,
  buffer_m = 0,
  id_format = "both",
  axis_order = "NE",
  include_llc = TRUE,
  point_type = "centroid",
  parallel = "auto",
  quiet = FALSE,
  dsn = NULL,
  layer = NULL,
  max_memory_gb = NULL,
  include_rat = FALSE,
  ...
) {
  # --- 1. Validate Arguments ---
  if (is.null(cellsize_m)) {
    stop("Argument 'cellsize_m' is missing, with no default", call. = FALSE)
  }
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
    available_gb <- .get_ram_gb("avail")

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
    axis_order = axis_order,
    include_llc = include_llc,
    point_type = point_type,
    include_rat = include_rat,
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

  # --- 3. RASTER PATH (SEQUENTIAL ONLY) ---
  if (output_type == "spatraster") {
    if (!requireNamespace("terra", quietly = TRUE)) {
      stop(
        "Package 'terra' is required for 'spatraster' output.",
        call. = FALSE
      )
    }

    if (isTRUE(parallel) || (is.character(parallel) && parallel == "auto")) {
      if (!quiet) {
        message(
          "Note: 'spatraster' output is processed sequentially by terra (streaming)."
        )
      }
    }

    # Case A: Streaming to Disk (File-backed)
    if (!is.null(dsn)) {
      # Validate extension
      validate_disk_compatibility(output_type, dsn)

      return(stream_grid_raster_terra(
        grid_extent = grid_extent,
        cellsize_m = cellsize_m,
        crs = crs,
        dsn = dsn,
        layer = layer,
        dot_args = backend_args,
        quiet = quiet,
        max_memory_gb = max_memory_gb
      ))
    }

    # Case B: In-Memory Generation (Legacy/Small grids)
    # Collect arguments
    all_args <- c(
      list(grid_extent = grid_extent, cellsize_m = cellsize_m, crs = crs),
      backend_args
    )

    # Generate in-memory
    r <- do.call(inspire_grid_from_extent_internal, all_args)
    return(r)
  }

  # --- 4. DISPATCHER LOGIC ---

  # --- A. WRITING TO DISK ---
  if (!is.null(dsn)) {
    # Validate extension vs output_type
    validate_disk_compatibility(output_type, dsn)

    # Use the highly efficient mirai stream if available
    if (use_mirai) {
      if (!quiet) {
        message(
          "`mirai` backend detected. Running in parallel (streaming to disk)."
        )
      }
      return(stream_grid_mirai(
        grid_extent = grid_extent,
        cellsize_m = cellsize_m,
        crs = crs,
        dsn = dsn,
        layer = layer,
        dot_args = backend_args,
        quiet = quiet,
        max_memory_gb = max_memory_gb
      ))
    } else {
      # Otherwise, use the safe, sequential streaming method
      if (!quiet) {
        message(
          "No parallel backend detected. Running in sequential mode (streaming to disk)."
        )
      }
      return(stream_grid_sequential(
        grid_extent = grid_extent,
        cellsize_m = cellsize_m,
        crs = crs,
        dsn = dsn,
        layer = layer,
        dot_args = backend_args,
        quiet = quiet,
        max_memory_gb = max_memory_gb
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
        "No parallel backend detected. Running in sequential mode. See ?inspire_grid for details how to enable parallel processing to speed up large jobs."
      )
    } else {
      message("Running in sequential mode.")
    }
  }
  all_args <- c(
    list(grid_extent = grid_extent, cellsize_m = cellsize_m, crs = crs),
    backend_args
  )
  return(do.call(inspire_grid_from_extent_internal, all_args))
}
