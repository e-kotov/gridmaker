#' Internal function to run grid creation using the `future` framework.
#' @keywords internal
#' @noRd
run_parallel_future <- function(grid_extent, cellsize_m, crs, dot_args) {
  if (!requireNamespace("furrr", quietly = TRUE)) {
    stop(
      "Package 'furrr' is required for future-based parallelism.",
      call. = FALSE
    )
  }

  quiet <- isTRUE(dot_args$quiet)
  dot_args$quiet <- NULL

  # --- 1. PREPARE GEOMETRIES ---
  grid_crs <- `%||%`(crs, sf::st_crs(grid_extent))
  if (is.na(grid_crs) || sf::st_is_longlat(grid_crs)) {
    stop("A projected CRS is required.")
  }
  all_args <- c(list(cellsize_m = cellsize_m, crs = grid_crs), dot_args)
  clipping_target <- NULL
  if (isTRUE(all_args$clip_to_input)) {
    if (sf::st_crs(grid_extent) != grid_crs) {
      grid_extent <- sf::st_transform(grid_extent, grid_crs)
    }
    target <- if (isTRUE(all_args$use_convex_hull)) {
      sf::st_convex_hull(sf::st_union(grid_extent))
    } else {
      grid_extent
    }
    if (!is.null(all_args$buffer_m) && all_args$buffer_m > 0) {
      target <- sf::st_buffer(target, dist = all_args$buffer_m)
    }
    clipping_target <- target
  }

  # --- 2. CREATE TILES (DEFINITIVE CORRECT LOGIC) ---
  full_bbox <- sf::st_bbox(grid_extent)
  xmin <- floor(as.numeric(full_bbox["xmin"]) / cellsize_m) * cellsize_m
  ymin <- floor(as.numeric(full_bbox["ymin"]) / cellsize_m) * cellsize_m
  xmax <- ceiling(as.numeric(full_bbox["xmax"]) / cellsize_m) * cellsize_m
  ymax <- ceiling(as.numeric(full_bbox["ymax"]) / cellsize_m) * cellsize_m

  num_workers <- future::nbrOfWorkers()
  # Scale down multiplier for high core counts to avoid overhead
  default_multiplier <- if (num_workers > 16) 1 else 2
  tile_multiplier <- getOption(
    "gridmaker.tile_multiplier",
    default = default_multiplier
  )
  num_tiles <- as.integer(round(num_workers * tile_multiplier))

  if (!quiet) {
    message(paste(
      "Processing",
      num_tiles,
      "tiles using",
      num_workers,
      "future workers..."
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

  tile_bboxes <- lapply(1:(length(y_breaks) - 1), function(i) {
    sf::st_bbox(
      c(xmin = xmin, ymin = y_breaks[i], xmax = xmax, ymax = y_breaks[i + 1]),
      crs = grid_crs
    )
  })

  # --- 3. PROCESS AND CLIP TILES IN PARALLEL ---
  grid_chunks <- furrr::future_map(
    .x = tile_bboxes,
    .f = ~ {
      args_for_tile <- c(list(grid_extent = .x), all_args)
      args_for_tile$clip_to_input <- FALSE
      chunk <- do.call(create_grid_internal, args_for_tile)
      if (nrow(chunk) > 0 && !is.null(clipping_target)) {
        intersects_indices <- sf::st_intersects(chunk, clipping_target)
        chunk <- chunk[lengths(intersects_indices) > 0, ]
      }
      if (nrow(chunk) == 0) NULL else chunk
    },
    .progress = !quiet,
    .options = furrr::furrr_options(seed = TRUE, packages = "gridmaker")
  )

  # --- 4. COMBINE (No de-duplication needed) ---
  if (!quiet) {
    message("Combining results...")
  }
  final_grid <- do.call(rbind, purrr::compact(grid_chunks))
  return(final_grid)
}
