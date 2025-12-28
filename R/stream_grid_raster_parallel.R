#' Parallel Raster Grid Streaming
#'
#' Internal functions for parallel raster grid generation with mirai and future
#' backends. These functions implement a parallel-compute, sequential-write
#' pattern where chunk computations are distributed across workers but disk
#' writes remain sequential for file integrity.
#'
#' @section Performance Benchmarks:
#' Benchmarks on ~500k cell grids show significant speedups over sequential:
#'
#' \strong{mirai backend} (best overall):
#' \itemize{
#'   \item TIF: ~40-65x faster with 2-4 workers
#'   \item NC: ~15-25x faster with 2-4 workers
#'   \item IMG: ~40-45x faster with 2-4 workers
#' }
#'
#' \strong{future backend}:
#' \itemize{
#'   \item TIF: ~25-40x faster with 2 workers
#'   \item NC: ~10-15x faster with 2 workers
#'   \item IMG: ~15-22x faster with 2 workers
#' }
#'
#' @section Backend Selection:
#' \itemize{
#'   \item \strong{mirai} is recommended for all formats due to lower overhead
#'     from persistent daemons (compute time ~10ms vs ~60ms for future)
#'   \item 2-4 workers provide optimal performance; 8+ workers show diminishing
#'     returns due to I/O bottleneck
#'   \item For small grids (<100k cells), sequential may be faster due to
#'     parallel startup overhead
#' }
#'
#' @name stream_grid_raster_parallel
#' @keywords internal
#' @noRd
NULL


#' Compute grid cell IDs for a chunk
#'
#' Pure computation function that generates cell IDs without terra objects.
#' Suitable for parallel execution across workers.
#'
#' @param start_row Starting row (1-indexed)
#' @param nrows Number of rows in chunk
#' @param ncols Total columns in raster
#' @return List with values, start, and nrows
#' @keywords internal
#' @noRd
.compute_raster_chunk <- function(start_row, nrows, ncols) {
  # Generate row/col indices for the chunk
  rows <- rep(start_row:(start_row + nrows - 1), each = ncols)
  cols <- rep(1:ncols, times = nrows)

  # Compute linear cell IDs (row-major order)
  cell_ids <- (rows - 1) * ncols + cols

  list(values = as.integer(cell_ids), start = start_row, nrows = nrows)
}


#' Stream raster grid using mirai parallel backend
#'
#' Parallel compute with mirai daemons, sequential write to disk.
#' Achieves ~40-65x speedup over sequential for TIF format.
#'
#' @inheritParams stream_grid_raster_terra
#' @param n_workers Number of mirai daemons to use (default: auto-detect)
#' @return Invisible path to output file
#' @keywords internal
#' @noRd
stream_raster_parallel_mirai <- function(
  grid_extent,
  cellsize_m,
  crs,
  dsn,
  layer,
  dot_args,
  quiet = FALSE,
  max_memory_gb = NULL,
  n_workers = NULL
) {
  if (!requireNamespace("mirai", quietly = TRUE)) {
    stop(
      "Package 'mirai' is required for parallel raster streaming.",
      call. = FALSE
    )
  }

  # --- 1. SETUP ---
  old_opts <- terra::terraOptions()
  on.exit(suppressWarnings(do.call(terra::terraOptions, old_opts)), add = TRUE)

  if (!is.null(max_memory_gb)) {
    terra::terraOptions(memmax = max_memory_gb, todisk = TRUE)
  } else {
    terra::terraOptions(todisk = TRUE)
    if (is.null(old_opts$memfrac) || old_opts$memfrac > 0.6) {
      terra::terraOptions(memfrac = 0.5)
    }
  }

  # RAT deprecation warning
  if (isTRUE(dot_args$include_rat)) {
    warning(
      "The 'include_rat' parameter is deprecated and has no effect. ",
      "To generate a Raster Attribute Table, create the grid in-memory ",
      "and use terra::levels() to attach the RAT before saving.",
      call. = FALSE
    )
  }

  # --- 2. PREPARE GRID GEOMETRY ---
  grid_crs <- sf::st_crs(crs %||% sf::st_crs(grid_extent))
  if (is.na(grid_crs) || sf::st_is_longlat(grid_crs)) {
    stop("A projected CRS is required.")
  }

  bbox <- .get_bbox_from_grid_extent(grid_extent, grid_crs)
  xmin <- floor(as.numeric(bbox["xmin"]) / cellsize_m) * cellsize_m
  ymin <- floor(as.numeric(bbox["ymin"]) / cellsize_m) * cellsize_m
  xmax <- ceiling(as.numeric(bbox["xmax"]) / cellsize_m) * cellsize_m
  ymax <- ceiling(as.numeric(bbox["ymax"]) / cellsize_m) * cellsize_m

  if (xmax <= xmin) {
    xmax <- xmin + cellsize_m
  }
  if (ymax <= ymin) {
    ymax <- ymin + cellsize_m
  }

  nrows <- as.integer(round((ymax - ymin) / cellsize_m))
  ncols <- as.integer(round((xmax - xmin) / cellsize_m))
  crs_string <- if (!is.na(grid_crs)) grid_crs$wkt else ""

  # --- 3. DETECT WORKERS ---
  if (is.null(n_workers)) {
    if (mirai::daemons_set()) {
      n_workers <- mirai::status()$connections
    } else {
      stop(
        "No mirai daemons configured. ",
        "Start daemons with mirai::daemons(n) before calling this function.",
        call. = FALSE
      )
    }
  }

  # Limit workers for small grids (overhead exceeds benefit)
  total_cells <- nrows * ncols
  if (total_cells < 50000) {
    n_workers <- min(n_workers, 2)
  } else if (total_cells < 200000) {
    n_workers <- min(n_workers, 4)
  }

  if (!quiet) {
    message(sprintf(
      "Streaming raster to disk using mirai (%d workers, %d chunks)...",
      n_workers,
      n_workers
    ))
  }

  # --- 4. CREATE CHUNK DEFINITIONS ---
  rows_per_chunk <- ceiling(nrows / n_workers)
  chunk_defs <- lapply(seq_len(n_workers), function(i) {
    start <- (i - 1) * rows_per_chunk + 1
    end <- min(i * rows_per_chunk, nrows)
    if (start > nrows) {
      return(NULL)
    }
    list(start_row = start, nrows = end - start + 1, ncols = ncols)
  })
  chunk_defs <- Filter(Negate(is.null), chunk_defs)

  # --- 5. PARALLEL COMPUTE WITH MIRAI ---
  tasks <- lapply(chunk_defs, function(chunk_def) {
    mirai::mirai(
      {
        rows <- rep(start_row:(start_row + nrows - 1), each = ncols)
        cols <- rep(1:ncols, times = nrows)
        cell_ids <- (rows - 1) * ncols + cols
        list(values = as.integer(cell_ids), start = start_row, nrows = nrows)
      },
      start_row = chunk_def$start_row,
      nrows = chunk_def$nrows,
      ncols = chunk_def$ncols
    )
  })

  # Collect results
  computed_chunks <- lapply(tasks, function(t) t[])

  # --- 6. SEQUENTIAL WRITE ---
  r_template <- terra::rast(
    nrows = nrows,
    ncols = ncols,
    xmin = xmin,
    xmax = xmax,
    ymin = ymin,
    ymax = ymax,
    crs = crs_string
  )

  gdal_opts <- c("COMPRESS=DEFLATE", "TILED=YES", "BIGTIFF=IF_NEEDED")
  datatype <- dot_args$datatype %||% "INT4S"

  b <- terra::writeStart(
    r_template,
    filename = dsn,
    overwrite = TRUE,
    wopt = list(datatype = datatype, gdal = gdal_opts)
  )

  for (chunk in computed_chunks) {
    terra::writeValues(
      r_template,
      chunk$values,
      start = chunk$start,
      nrows = chunk$nrows
    )
  }

  r_out <- terra::writeStop(r_template)

  # --- 7. POST-PROCESSING (CLIPPING) ---
  if (isTRUE(dot_args$clip_to_input)) {
    .apply_raster_mask(dsn, grid_extent, dot_args, gdal_opts, quiet)
  }

  return(invisible(dsn))
}


#' Stream raster grid using future parallel backend
#'
#' Parallel compute with future, sequential write to disk.
#' Achieves ~15-40x speedup over sequential depending on format.
#'
#' @inheritParams stream_grid_raster_terra
#' @return Invisible path to output file
#' @keywords internal
#' @noRd
stream_raster_parallel_future <- function(
  grid_extent,
  cellsize_m,
  crs,
  dsn,
  layer,
  dot_args,
  quiet = FALSE,
  max_memory_gb = NULL
) {
  if (
    !requireNamespace("future", quietly = TRUE) ||
      !requireNamespace("future.apply", quietly = TRUE)
  ) {
    stop(
      "Packages 'future' and 'future.apply' are required for parallel raster streaming.",
      call. = FALSE
    )
  }

  # --- 1. SETUP ---
  old_opts <- terra::terraOptions()
  on.exit(suppressWarnings(do.call(terra::terraOptions, old_opts)), add = TRUE)

  if (!is.null(max_memory_gb)) {
    terra::terraOptions(memmax = max_memory_gb, todisk = TRUE)
  } else {
    terra::terraOptions(todisk = TRUE)
    if (is.null(old_opts$memfrac) || old_opts$memfrac > 0.6) {
      terra::terraOptions(memfrac = 0.5)
    }
  }

  # RAT deprecation warning
  if (isTRUE(dot_args$include_rat)) {
    warning(
      "The 'include_rat' parameter is deprecated and has no effect. ",
      "To generate a Raster Attribute Table, create the grid in-memory ",
      "and use terra::levels() to attach the RAT before saving.",
      call. = FALSE
    )
  }

  # --- 2. PREPARE GRID GEOMETRY ---
  grid_crs <- sf::st_crs(crs %||% sf::st_crs(grid_extent))
  if (is.na(grid_crs) || sf::st_is_longlat(grid_crs)) {
    stop("A projected CRS is required.")
  }

  bbox <- .get_bbox_from_grid_extent(grid_extent, grid_crs)
  xmin <- floor(as.numeric(bbox["xmin"]) / cellsize_m) * cellsize_m
  ymin <- floor(as.numeric(bbox["ymin"]) / cellsize_m) * cellsize_m
  xmax <- ceiling(as.numeric(bbox["xmax"]) / cellsize_m) * cellsize_m
  ymax <- ceiling(as.numeric(bbox["ymax"]) / cellsize_m) * cellsize_m

  if (xmax <= xmin) {
    xmax <- xmin + cellsize_m
  }
  if (ymax <= ymin) {
    ymax <- ymin + cellsize_m
  }

  nrows <- as.integer(round((ymax - ymin) / cellsize_m))
  ncols <- as.integer(round((xmax - xmin) / cellsize_m))
  crs_string <- if (!is.na(grid_crs)) grid_crs$wkt else ""

  # --- 3. DETECT WORKERS ---
  n_workers <- future::nbrOfWorkers()
  if (n_workers <= 1) {
    stop(
      "No future workers configured. ",
      "Configure a parallel plan with future::plan() before calling this function.",
      call. = FALSE
    )
  }

  # Limit workers for small grids
  total_cells <- nrows * ncols
  if (total_cells < 50000) {
    n_workers <- min(n_workers, 2)
  } else if (total_cells < 200000) {
    n_workers <- min(n_workers, 4)
  }

  if (!quiet) {
    message(sprintf(
      "Streaming raster to disk using future (%d workers, %d chunks)...",
      n_workers,
      n_workers
    ))
  }

  # --- 4. CREATE CHUNK DEFINITIONS ---
  rows_per_chunk <- ceiling(nrows / n_workers)
  chunk_defs <- lapply(seq_len(n_workers), function(i) {
    start <- (i - 1) * rows_per_chunk + 1
    end <- min(i * rows_per_chunk, nrows)
    if (start > nrows) {
      return(NULL)
    }
    list(start_row = start, nrows = end - start + 1, ncols = ncols)
  })
  chunk_defs <- Filter(Negate(is.null), chunk_defs)

  # --- 5. PARALLEL COMPUTE WITH FUTURE ---
  computed_chunks <- future.apply::future_lapply(
    chunk_defs,
    function(chunk_def) {
      .compute_raster_chunk(
        chunk_def$start_row,
        chunk_def$nrows,
        chunk_def$ncols
      )
    },
    future.seed = NULL
  )

  # --- 6. SEQUENTIAL WRITE ---
  r_template <- terra::rast(
    nrows = nrows,
    ncols = ncols,
    xmin = xmin,
    xmax = xmax,
    ymin = ymin,
    ymax = ymax,
    crs = crs_string
  )

  gdal_opts <- c("COMPRESS=DEFLATE", "TILED=YES", "BIGTIFF=IF_NEEDED")
  datatype <- dot_args$datatype %||% "INT4S"

  b <- terra::writeStart(
    r_template,
    filename = dsn,
    overwrite = TRUE,
    wopt = list(datatype = datatype, gdal = gdal_opts)
  )

  for (chunk in computed_chunks) {
    terra::writeValues(
      r_template,
      chunk$values,
      start = chunk$start,
      nrows = chunk$nrows
    )
  }

  r_out <- terra::writeStop(r_template)

  # --- 7. POST-PROCESSING (CLIPPING) ---
  if (isTRUE(dot_args$clip_to_input)) {
    .apply_raster_mask(dsn, grid_extent, dot_args, gdal_opts, quiet)
  }

  return(invisible(dsn))
}


#' Apply mask to raster (clipping)
#'
#' Helper function to apply mask after raster generation.
#'
#' @param dsn Path to raster file
#' @param grid_extent Original grid extent
#' @param dot_args Additional arguments
#' @param gdal_opts GDAL write options
#' @param quiet Suppress messages
#' @keywords internal
#' @noRd
.apply_raster_mask <- function(dsn, grid_extent, dot_args, gdal_opts, quiet) {
  clipping_target <- NULL
  if (inherits(grid_extent, c("sf", "sfc"))) {
    if (isTRUE(dot_args$use_convex_hull)) {
      clipping_target <- sf::st_convex_hull(sf::st_union(grid_extent))
    } else {
      clipping_target <- grid_extent
    }
    if (!is.null(dot_args$buffer_m) && dot_args$buffer_m > 0) {
      clipping_target <- sf::st_buffer(
        clipping_target,
        dist = dot_args$buffer_m
      )
    }
  }

  if (!is.null(clipping_target)) {
    if (!quiet) {
      message("Applying mask (clipping to input)...")
    }

    output_ext <- tools::file_ext(dsn)
    temp_masked <- tempfile(
      fileext = if (nzchar(output_ext)) paste0(".", output_ext) else ""
    )

    r_raw <- terra::rast(dsn)
    v_target <- terra::vect(clipping_target)

    terra::mask(
      r_raw,
      v_target,
      filename = temp_masked,
      overwrite = TRUE,
      wopt = list(gdal = gdal_opts)
    )

    file.copy(temp_masked, dsn, overwrite = TRUE)
    unlink(temp_masked)
  }
}
