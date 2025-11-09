#' Internal function to create a grid and stream it to a file using mirai.
#' @note This is the memory-efficient streaming implementation for the mirai backend.
#' @keywords internal
#' @noRd
async_stream_to_disk_with_mirai <- function(
  grid_extent,
  cellsize_m,
  crs,
  dsn,
  layer,
  dot_args,
  quiet = FALSE,
  max_memory_gb = NULL
) {
  # --- 1. Validate Dependencies ---
  if (
    !all(sapply(
      c("mirai", "promises", "later"),
      requireNamespace,
      quietly = TRUE
    ))
  ) {
    stop(
      "Packages 'mirai', 'promises', and 'later' are required for streaming.",
      call. = FALSE
    )
  }
  if (!mirai::daemons_set()) {
    stop(
      "`mirai` daemons are not running. Please configure them first.",
      call. = FALSE
    )
  }
  if (file.exists(dsn)) {
    if (!quiet) {
      message(paste(
        "Output file '",
        dsn,
        "' exists and will be overwritten.",
        sep = ""
      ))
    }
    unlink(dsn, recursive = TRUE)
  }

  # --- 2. Prepare Geometries and CRS ---
  grid_crs <- `%||%`(crs, sf::st_crs(grid_extent))
  if (is.na(grid_crs) || sf::st_is_longlat(grid_crs)) {
    stop("A projected CRS is required.")
  }

  # Combine backend arguments from dot_args
  backend_args <- c(
    list(cellsize_m = cellsize_m, crs = grid_crs),
    dot_args
  )

  clipping_target <- NULL
  if (isTRUE(backend_args$clip_to_input)) {
    if (sf::st_crs(grid_extent) != grid_crs) {
      grid_extent <- sf::st_transform(grid_extent, grid_crs)
    }
    target <- if (isTRUE(backend_args$use_convex_hull)) {
      sf::st_convex_hull(sf::st_union(grid_extent))
    } else {
      grid_extent
    }
    if (!is.null(backend_args$buffer_m) && backend_args$buffer_m > 0) {
      target <- sf::st_buffer(target, dist = backend_args$buffer_m)
    }
    clipping_target <- target
  }

  # --- 3. CREATE TILES (BALANCING MEMORY AND PARALLELISM) ---
  full_bbox <- .get_bbox_from_grid_extent(grid_extent, grid_crs)
  bbox_values <- as.numeric(full_bbox)
  xmin <- floor(bbox_values[1] / cellsize_m) * cellsize_m
  ymin <- floor(bbox_values[2] / cellsize_m) * cellsize_m
  xmax <- ceiling(bbox_values[3] / cellsize_m) * cellsize_m
  ymax <- ceiling(bbox_values[4] / cellsize_m) * cellsize_m

  # Calculate the maximum rows per chunk based on memory constraints
  max_rows_per_chunk <- .calculate_rows_per_chunk(
    grid_extent,
    cellsize_m,
    grid_crs,
    dot_args,
    max_memory_gb
  )

  # Calculate total rows in the grid
  total_rows <- as.integer(round((ymax - ymin) / cellsize_m))

  # Determine optimal number of chunks for parallelism
  num_daemons <- mirai::status()$connections
  tile_multiplier <- getOption("gridmaker.tile_multiplier", default = 2)
  desired_tiles <- as.integer(round(num_daemons * tile_multiplier))

  # Calculate rows per chunk needed to achieve desired parallelism
  desired_rows_per_chunk <- ceiling(total_rows / desired_tiles)

  # Use the MINIMUM of memory-constrained and parallelism-optimal values
  # This ensures we never exceed memory while maximizing parallelism
  actual_rows_per_chunk <- min(max_rows_per_chunk, desired_rows_per_chunk)

  # Ensure at least 1 row per chunk
  if (actual_rows_per_chunk < 1) {
    actual_rows_per_chunk <- 1
  }

  # Calculate actual number of chunks we'll create
  num_tiles <- ceiling(total_rows / actual_rows_per_chunk)

  # Create evenly-sized chunks based on row counts
  row_breaks <- floor(seq.int(0, total_rows, length.out = num_tiles + 1))
  y_breaks <- ymin + (row_breaks * cellsize_m)
  y_breaks <- unique(y_breaks)

  num_tiles <- length(y_breaks) - 1

  if (!quiet) {
    message(paste(
      "Creating",
      num_tiles,
      "chunks (",
      actual_rows_per_chunk,
      "rows/chunk) for",
      num_daemons,
      "daemons."
    ))
    if (actual_rows_per_chunk < desired_rows_per_chunk) {
      message(paste(
        "  Note: Memory constraints limited chunk size",
        "(max",
        max_rows_per_chunk,
        "rows/chunk)"
      ))
    }
  }

  tile_bboxes <- lapply(1:(length(y_breaks) - 1), function(i) {
    sf::st_bbox(
      c(xmin = xmin, ymin = y_breaks[i], xmax = xmax, ymax = y_breaks[i + 1]),
      crs = grid_crs
    )
  })

  # Helper function to format seconds into a readable string
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

  # --- 4. THE ASYNCHRONOUS PIPELINE with mirai ---
  writer_promise_chain <- promises::promise_resolve(TRUE)
  is_first_chunk <- TRUE
  chunks_completed <- 0
  start_time <- Sys.time()

  all_tasks_queued_promise <- promises::as.promise(
    mirai::mirai_map(
      .x = tile_bboxes,
      .f = function(.x) {
        args_for_tile <- c(list(grid_extent = .x), backend_args)
        args_for_tile$clip_to_input <- FALSE
        chunk <- do.call(create_grid_internal, args_for_tile)
        if (nrow(chunk) > 0 && !is.null(clipping_target)) {
          intersects_indices <- sf::st_intersects(chunk, clipping_target)
          chunk <- chunk[lengths(intersects_indices) > 0, ]
        }
        if (nrow(chunk) == 0) NULL else chunk
      },
      backend_args = backend_args,
      clipping_target = clipping_target,
      .promise = function(chunk) {
        writer_promise_chain <<- promises::then(
          writer_promise_chain,
          function(value) {
            if (!is.null(chunk)) {
              if (!quiet) {
                chunks_completed <<- chunks_completed + 1

                time_elapsed_s <- as.numeric(difftime(
                  Sys.time(),
                  start_time,
                  units = "secs"
                ))
                eta_str <- if (chunks_completed > 1 && time_elapsed_s > 0) {
                  chunks_per_second <- chunks_completed / time_elapsed_s
                  chunks_remaining <- num_tiles - chunks_completed
                  eta_seconds <- chunks_remaining / chunks_per_second
                  format_time_eta(eta_seconds)
                } else {
                  "..."
                }

                percent <- floor((chunks_completed / num_tiles) * 100)
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
                  num_tiles,
                  eta_str
                )
                cat(progress_string)
              }

              sf::st_write(
                chunk,
                dsn = dsn,
                layer = layer,
                append = !is_first_chunk,
                quiet = TRUE
              )
              if (is_first_chunk) is_first_chunk <<- FALSE
            }
          }
        )
      }
    )
  )

  final_promise <- promises::`%...>%`(
    all_tasks_queued_promise,
    (function(...) {
      writer_promise_chain
    })
  )

  pipeline_finished <- FALSE
  promises::then(
    final_promise,
    onFulfilled = function(value) {
      if (!quiet) {
        cat("\nGrid streaming complete.\n")
      }
      pipeline_finished <<- TRUE
    },
    onRejected = function(err) {
      if (!quiet) {
        cat("\n")
      }
      pipeline_finished <<- TRUE
      stop("Grid streaming failed: ", err$message, call. = FALSE)
    }
  )

  # --- 5. Execute the promise chain ---
  if (!quiet) {
    message("Starting asynchronous generation and writing pipeline...")
  }
  while (!pipeline_finished) {
    later::run_now()
    Sys.sleep(0.1)
  }

  return(invisible(dsn))
}

#' Internal function to create a grid and stream it sequentially to a file.
#' @note This provides a memory-safe fallback for writing to disk without a
#'   parallel backend.
#' @keywords internal
#' @noRd
stream_to_disk_sequential <- function(
  grid_extent,
  cellsize_m,
  crs,
  dsn,
  layer,
  dot_args,
  quiet = FALSE,
  max_memory_gb = NULL
) {
  # --- 1. VALIDATE AND PREPARE ---
  if (file.exists(dsn)) {
    if (!quiet) {
      message(paste(
        "Output file '",
        dsn,
        "' exists and will be overwritten.",
        sep = ""
      ))
    }
    unlink(dsn, recursive = TRUE)
  }

  grid_crs <- `%||%`(crs, sf::st_crs(grid_extent))
  if (is.na(grid_crs) || sf::st_is_longlat(grid_crs)) {
    stop("A projected CRS is required.")
  }

  backend_args <- c(
    list(cellsize_m = cellsize_m, crs = grid_crs),
    dot_args
  )

  clipping_target <- NULL
  if (isTRUE(backend_args$clip_to_input)) {
    if (sf::st_crs(grid_extent) != grid_crs) {
      grid_extent <- sf::st_transform(grid_extent, grid_crs)
    }
    target <- if (isTRUE(backend_args$use_convex_hull)) {
      sf::st_convex_hull(sf::st_union(grid_extent))
    } else {
      grid_extent
    }
    if (!is.null(backend_args$buffer_m) && backend_args$buffer_m > 0) {
      target <- sf::st_buffer(target, dist = backend_args$buffer_m)
    }
    clipping_target <- target
  }

  # --- 2. CREATE TILES (BALANCING MEMORY AND PARALLELISM) ---
  full_bbox <- .get_bbox_from_grid_extent(grid_extent, grid_crs)
  bbox_values <- as.numeric(full_bbox)
  xmin <- floor(bbox_values[1] / cellsize_m) * cellsize_m
  ymin <- floor(bbox_values[2] / cellsize_m) * cellsize_m
  xmax <- ceiling(bbox_values[3] / cellsize_m) * cellsize_m
  ymax <- ceiling(bbox_values[4] / cellsize_m) * cellsize_m

  # Calculate the maximum rows per chunk based on memory constraints
  max_rows_per_chunk <- .calculate_rows_per_chunk(
    grid_extent,
    cellsize_m,
    grid_crs,
    backend_args,
    max_memory_gb
  )

  # Calculate total rows in the grid
  total_rows <- as.integer(round((ymax - ymin) / cellsize_m))

  # For sequential mode, we want fewer, memory-safe chunks
  # Use a modest multiplier to balance chunk count with memory
  desired_tiles <- max(4, total_rows %/% max_rows_per_chunk)

  # Calculate rows per chunk needed to achieve desired parallelism
  desired_rows_per_chunk <- ceiling(total_rows / desired_tiles)

  # Use the MINIMUM of memory-constrained and parallelism-optimal values
  actual_rows_per_chunk <- min(max_rows_per_chunk, desired_rows_per_chunk)

  # Ensure at least 1 row per chunk
  if (actual_rows_per_chunk < 1) {
    actual_rows_per_chunk <- 1
  }

  # Calculate actual number of chunks we'll create
  num_tiles <- ceiling(total_rows / actual_rows_per_chunk)

  # Create evenly-sized chunks based on row counts
  row_breaks <- floor(seq.int(0, total_rows, length.out = num_tiles + 1))
  y_breaks <- ymin + (row_breaks * cellsize_m)
  y_breaks <- unique(y_breaks)

  num_tiles <- length(y_breaks) - 1

  if (!quiet) {
    message(paste(
      "Sequentially generating and writing",
      num_tiles,
      "chunks (",
      actual_rows_per_chunk,
      "rows/chunk)..."
    ))
    if (actual_rows_per_chunk < desired_rows_per_chunk) {
      message(paste(
        "  Note: Memory constraints limited chunk size",
        "(max",
        max_rows_per_chunk,
        "rows/chunk)"
      ))
    }
  }

  tile_bboxes <- lapply(1:(length(y_breaks) - 1), function(i) {
    sf::st_bbox(
      c(xmin = xmin, ymin = y_breaks[i], xmax = xmax, ymax = y_breaks[i + 1]),
      crs = grid_crs
    )
  })

  # --- 3. SEQUENTIAL GENERATION AND WRITING LOOP ---
  is_first_chunk <- TRUE
  if (!quiet) {
    pb <- utils::txtProgressBar(min = 0, max = num_tiles, style = 3)
  }

  for (i in seq_along(tile_bboxes)) {
    tile_bbox <- tile_bboxes[[i]]

    # A. Generate one chunk
    args_for_tile <- c(list(grid_extent = tile_bbox), backend_args)
    args_for_tile$clip_to_input <- FALSE
    chunk <- do.call(create_grid_internal, args_for_tile)

    # B. Clip if necessary
    if (nrow(chunk) > 0 && !is.null(clipping_target)) {
      intersects_indices <- sf::st_intersects(chunk, clipping_target)
      chunk <- chunk[lengths(intersects_indices) > 0, ]
    }

    # C. Write to disk
    if (nrow(chunk) > 0) {
      sf::st_write(
        chunk,
        dsn = dsn,
        layer = layer,
        append = !is_first_chunk,
        quiet = TRUE
      )
      if (is_first_chunk) is_first_chunk <- FALSE
    }

    # D. Update progress
    if (!quiet) {
      utils::setTxtProgressBar(pb, i)
    }
  }

  if (!quiet) {
    close(pb)
    cat("\nGrid streaming complete.\n")
  }

  return(invisible(dsn))
}
