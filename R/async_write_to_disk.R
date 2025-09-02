#' Create a standard-compliant spatial grid and stream it to a file using mirai
#'
#' @description
#' This function generates a regular spatial grid and writes it directly to a file
#' using a memory-efficient asynchronous pipeline with the `mirai` backend.
#' Chunks are generated in parallel and the `.promise` argument of `mirai_map`
#' is used to write chunks to disk sequentially as they become available.
#'
#' @note
#' This function requires that `mirai` daemons are already configured and running
#' (e.g., via `mirai::daemons(4)`). It also requires the `promises` and `later`
#' packages.
#'
#' @inheritParams create_grid
#' @param dsn A character string for the data source name (e.g., file path).
#' @param layer A character string for the layer name.
#' @param max_cells_per_chunk An optional integer. If provided, the function will
#'   create processing chunks that contain approximately this many grid cells at most.
#'   This provides more granular control over memory usage than the default tiling
#'   strategy. If `NULL` (the default), tiling is based on the number of workers.
#' @export
create_grid_and_stream_mirai <- function(
  grid_extent,
  cellsize_m,
  crs = NULL,
  dsn,
  layer,
  output_type = "sf_polygons",
  clip_to_input = FALSE,
  use_convex_hull = FALSE,
  buffer_m = 0,
  id_format = "both",
  include_llc = TRUE,
  point_type = "centroid",
  quiet = FALSE,
  max_cells_per_chunk = NULL
) {
  # --- 1. Validate Dependencies and Arguments ---
  if (
    !all(sapply(
      c("mirai", "promises", "later"),
      requireNamespace,
      quietly = TRUE
    ))
  ) {
    stop(
      "Packages 'mirai', 'promises', and 'later' are required for this function.",
      call. = FALSE
    )
  }
  if (!mirai::daemons_set()) {
    stop(
      "`mirai` daemons are not running. Please configure them first (e.g., `mirai::daemons(4)`).",
      call. = FALSE
    )
  }
  if (missing(dsn)) {
    stop("'dsn' must be provided.", call. = FALSE)
  }
  if (missing(layer)) {
    stop("'layer' must be provided.", call. = FALSE)
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

  # All arguments needed by the internal function must be in this list
  backend_args <- list(
    cellsize_m = cellsize_m,
    crs = grid_crs,
    output_type = output_type,
    clip_to_input = clip_to_input,
    use_convex_hull = use_convex_hull,
    buffer_m = buffer_m,
    id_format = id_format,
    include_llc = include_llc,
    point_type = point_type
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
      target <- sf::st_buffer(target, dist = buffer_m)
    }
    clipping_target <- target
  }

  full_bbox <- sf::st_bbox(grid_extent)
  xmin <- floor(as.numeric(full_bbox["xmin"]) / cellsize_m) * cellsize_m
  ymin <- floor(as.numeric(full_bbox["ymin"]) / cellsize_m) * cellsize_m
  xmax <- ceiling(as.numeric(full_bbox["xmax"]) / cellsize_m) * cellsize_m
  ymax <- ceiling(as.numeric(full_bbox["ymax"]) / cellsize_m) * cellsize_m

  # New chunking logic
  if (!is.null(max_cells_per_chunk)) {
    # Strategy 1: Chunk by cell count
    if (!quiet) {
      message(paste(
        "Tiling strategy: Aiming for max",
        format(max_cells_per_chunk, big.mark = ","),
        "cells per chunk."
      ))
    }
    n_cols <- ceiling((xmax - xmin) / cellsize_m)
    rows_per_chunk <- floor(max_cells_per_chunk / n_cols)
    if (rows_per_chunk == 0) {
      rows_per_chunk <- 1
      if (!quiet) {
        warning(
          "max_cells_per_chunk is smaller than the number of cells in a single row. Chunks will exceed the target size.",
          call. = FALSE
        )
      }
    }
    chunk_height_m <- rows_per_chunk * cellsize_m
    y_breaks <- seq(from = ymin, to = ymax, by = chunk_height_m)
    if (tail(y_breaks, 1) < ymax) {
      y_breaks <- c(y_breaks, ymax)
    }
  } else {
    # Strategy 2: Default chunking by number of workers
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
    y_breaks <- round(seq(ymin, ymax, length.out = num_tiles + 1))
  }

  y_breaks <- unique(y_breaks)
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

  # --- 3. THE ASYNCHRONOUS PIPELINE with mirai ---
  writer_promise_chain <- promises::promise_resolve(TRUE)
  is_first_chunk <- TRUE

  all_tasks_queued_promise <- promises::as.promise(
    mirai::mirai_map(
      .x = tile_bboxes,
      .f = function(.x) {
        args_for_tile <- c(list(grid_extent = .x), backend_args)
        args_for_tile$clip_to_input <- FALSE # Internal clipping is handled below
        chunk <- do.call(gridmaker:::create_grid_internal, args_for_tile)
        if (nrow(chunk) > 0 && !is.null(clipping_target)) {
          intersects_indices <- sf::st_intersects(chunk, clipping_target)
          chunk <- chunk[lengths(intersects_indices) > 0, ]
        }
        if (nrow(chunk) == 0) NULL else chunk
      },
      backend_args = backend_args,
      clipping_target = clipping_target,
      .promise = function(chunk) {
        # Use promises::then() for robustness instead of the '%...>%' operator
        writer_promise_chain <<- promises::then(
          writer_promise_chain,
          function(value) {
            if (!is.null(chunk)) {
              if (!quiet) {
                cat(".")
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

  # The final promise waits for all queuing, THEN all writing
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
