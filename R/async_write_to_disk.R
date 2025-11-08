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
  quiet = FALSE
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

  # --- 2. Prepare Geometries and Tiles ---
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

  # --- 2. CREATE TILES ---
  full_bbox <- sf::st_bbox(grid_extent)
  xmin <- floor(as.numeric(full_bbox["xmin"]) / cellsize_m) * cellsize_m
  ymin <- floor(as.numeric(full_bbox["ymin"]) / cellsize_m) * cellsize_m
  xmax <- ceiling(as.numeric(full_bbox["xmax"]) / cellsize_m) * cellsize_m
  ymax <- ceiling(as.numeric(full_bbox["ymax"]) / cellsize_m) * cellsize_m

  num_daemons <- mirai::status()$connections
  tile_multiplier <- getOption("gridmaker.tile_multiplier", default = 2)
  num_tiles <- as.integer(round(num_daemons * tile_multiplier))

  if (!quiet) {
    message(paste(
      "Tiling strategy: Creating",
      num_tiles,
      "geographic slices for",
      num_daemons,
      "daemons."
    ))
  }

  # Calculate total rows and divide them among tiles
  total_rows <- as.integer(round((ymax - ymin) / cellsize_m))
  if (total_rows < num_tiles) {
    num_tiles <- total_rows
  }

  # Create breaks based on integer row counts, then convert to coordinates
  row_breaks <- floor(seq.int(0, total_rows, length.out = num_tiles + 1))
  y_breaks <- ymin + (row_breaks * cellsize_m)
  y_breaks <- unique(y_breaks) # Ensure no zero-height tiles

  num_tiles <- length(y_breaks) - 1
  if (!quiet) {
    message(paste("Dispatching", num_tiles, "generation jobs..."))
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

  # --- 3. THE ASYNCHRONOUS PIPELINE with mirai ---
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

  # --- 4. Execute the promise chain ---
  if (!quiet) {
    message("Starting asynchronous generation and writing pipeline...")
  }
  while (!pipeline_finished) {
    later::run_now()
    Sys.sleep(0.1)
  }

  return(invisible(NULL))
}
