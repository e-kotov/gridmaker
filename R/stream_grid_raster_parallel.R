#' Parallel Raster Grid Streaming
#'
#' Internal functions for parallel raster grid generation with mirai backend.
#' These functions implement an async streaming pattern where chunk computations
#' are distributed across workers and written to disk as they complete.
#'
#' @section Performance Benchmarks:
#' Benchmarks on ~10M cell grids show meaningful speedups over sequential:
#'
#' \strong{mirai backend}:
#' \itemize{
#'   \item TIF: ~1.7-2.3x faster
#'   \item NC: ~1.9-2.9x faster
#'   \item IMG: ~1.8-2.5x faster
#' }
#'
#' @section Backend Selection:
#' \itemize{
#'   \item \strong{mirai} is the only supported parallel backend for raster output.
#'   \item Speedups generally plateau around 2-3x regardless of worker count.
#'   \item Optimal worker count is typically 4-8; adding more workers yields diminishing returns.
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
  # Compute first cell ID for this linear chunk
  # Cell IDs are 1-based, row-major
  first_cell <- (start_row - 1) * ncols + 1

  # Create sequence directly
  # Since we are processing full rows, the cell IDs are contiguous
  values <- seq.int(from = first_cell, by = 1L, length.out = nrows * ncols)

  list(values = values, start = start_row, nrows = nrows)
}


#' Stream raster grid using mirai parallel backend
#'
#' Parallel compute with mirai daemons, sequential write to disk.
#' Achieves ~2-3x speedup over sequential for TIF format.
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
  if (
    !all(sapply(
      c("mirai", "promises", "later"),
      requireNamespace,
      quietly = TRUE
    ))
  ) {
    stop(
      "Packages 'mirai', 'promises', and 'later' are required for async raster streaming.",
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
  num_chunks <- length(chunk_defs)

  if (!quiet) {
    message(sprintf(
      "Streaming raster to disk using mirai (%d workers, %d chunks)...",
      n_workers,
      num_chunks
    ))
  }

  # --- 5. OPEN RASTER FILE FOR WRITING ---
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

  terra::writeStart(
    r_template,
    filename = dsn,
    overwrite = TRUE,
    wopt = list(datatype = datatype, gdal = gdal_opts)
  )

  # --- 6. ASYNC PIPELINE: COMPUTE AND WRITE AS CHUNKS COMPLETE ---
  # We need to ensure chunks are written in row order, so we use a pending

  # writes buffer that holds out-of-order chunks until their turn.

  # Track state
  chunks_completed <- 0
  next_chunk_to_write <- 1
  pending_writes <- list() # Buffer for out-of-order chunks
  start_time <- Sys.time()
  pipeline_finished <- FALSE
  pipeline_error <- NULL

  # Helper function to format time
  format_time_eta <- function(seconds) {
    if (is.na(seconds) || seconds < 0 || !is.finite(seconds)) {
      return("...")
    }
    if (seconds < 60) {
      return(sprintf("%.0f sec", seconds))
    }
    if (seconds < 3600) {
      return(sprintf("%.1f min", seconds / 60))
    }
    return(sprintf("%.1f hr", seconds / 3600))
  }

  # Function to write pending chunks in order
  write_pending_chunks <- function() {
    while (!is.null(pending_writes[[as.character(next_chunk_to_write)]])) {
      chunk <- pending_writes[[as.character(next_chunk_to_write)]]
      pending_writes[[as.character(next_chunk_to_write)]] <<- NULL

      terra::writeValues(
        r_template,
        chunk$values,
        start = chunk$start,
        nrows = chunk$nrows
      )

      next_chunk_to_write <<- next_chunk_to_write + 1
    }
  }

  # Launch async computation with promise callbacks
  all_tasks_promise <- promises::as.promise(
    mirai::mirai_map(
      .x = seq_along(chunk_defs),
      .f = function(chunk_idx) {
        chunk_def <- chunk_defs[[chunk_idx]]
        start_row <- chunk_def$start_row
        nrows_chunk <- chunk_def$nrows
        ncols_chunk <- chunk_def$ncols

        # Generate continuous sequence of cell IDs directly (row-major order)
        # Optimization: Avoid allocating large row/col vectors
        start_id <- (start_row - 1) * ncols_chunk + 1
        end_id <- start_id + (nrows_chunk * ncols_chunk) - 1
        cell_ids <- start_id:end_id

        list(
          chunk_idx = chunk_idx,
          values = as.integer(cell_ids),
          start = start_row,
          nrows = nrows_chunk
        )
      },
      chunk_defs = chunk_defs,
      .promise = function(result) {
        # Store this chunk in pending writes buffer
        pending_writes[[as.character(result$chunk_idx)]] <<- result

        # Try to write any chunks that are ready (in order)
        write_pending_chunks()

        # Update progress
        chunks_completed <<- chunks_completed + 1
        if (!quiet) {
          time_elapsed_s <- as.numeric(difftime(
            Sys.time(),
            start_time,
            units = "secs"
          ))
          eta_str <- if (chunks_completed > 1 && time_elapsed_s > 0) {
            chunks_per_second <- chunks_completed / time_elapsed_s
            chunks_remaining <- num_chunks - chunks_completed
            eta_seconds <- chunks_remaining / chunks_per_second
            format_time_eta(eta_seconds)
          } else {
            "..."
          }

          percent <- floor((chunks_completed / num_chunks) * 100)
          bar_width <- 30
          done_width <- round(percent / 100 * bar_width)
          bar <- paste0(
            "[",
            paste(rep("=", done_width), collapse = ""),
            paste(rep(" ", bar_width - done_width), collapse = ""),
            "]"
          )

          progress_string <- sprintf(
            "\rWriting chunks: %s %3d%% (%d/%d) | ETA: %-8s",
            bar,
            percent,
            chunks_completed,
            num_chunks,
            eta_str
          )
          cat(progress_string)
        }
      }
    )
  )

  # Handle completion
  promises::then(
    all_tasks_promise,
    onFulfilled = function(value) {
      # Flush any remaining pending writes
      write_pending_chunks()
      pipeline_finished <<- TRUE
    },
    onRejected = function(err) {
      pipeline_error <<- err
      pipeline_finished <<- TRUE
    }
  )

  # --- 7. EVENT LOOP: Wait for pipeline to complete ---
  if (!quiet) {
    message("Starting async raster streaming pipeline...")
  }

  while (!pipeline_finished) {
    later::run_now()
    Sys.sleep(0.05)
  }

  # Handle errors
  if (!is.null(pipeline_error)) {
    terra::writeStop(r_template)
    stop("Raster streaming failed: ", pipeline_error$message, call. = FALSE)
  }

  # --- 8. FINALIZE ---
  terra::writeStop(r_template)

  if (!quiet) {
    cat("\nRaster streaming complete.\n")
  }

  # --- 9. POST-PROCESSING (CLIPPING) ---
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
