# Internal function to run grid creation using the `future` framework.
run_parallel_future <- function(grid_extent, cellsize_m, crs, dot_args) {
  if (!requireNamespace("furrr", quietly = TRUE)) {
    stop(
      "Package 'furrr' is required for future-based parallelism.",
      call. = FALSE
    )
  }

  # --- 1. PREPARE GEOMETRIES AND TILES ---
  grid_crs <- crs %||% sf::st_crs(grid_extent)
  if (is.na(grid_crs) || sf::st_is_longlat(grid_crs)) {
    stop("A projected CRS is required.")
  }

  # The full set of arguments for the internal function
  all_args <- c(list(cellsize_m = cellsize_m, crs = grid_crs), dot_args)

  # Prepare clipping target once in the main session
  clipping_target <- NULL
  if (isTRUE(all_args$clip_to_input)) {
    # Transform extent if necessary
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

  # --- 2. CREATE TILES ---
  full_bbox <- sf::st_bbox(grid_extent)
  xmin <- floor(as.numeric(full_bbox["xmin"]) / cellsize_m) * cellsize_m
  ymin <- floor(as.numeric(full_bbox["ymin"]) / cellsize_m) * cellsize_m
  xmax <- ceiling(as.numeric(full_bbox["xmax"]) / cellsize_m) * cellsize_m
  ymax <- ceiling(as.numeric(full_bbox["ymax"]) / cellsize_m) * cellsize_m

  num_workers <- future::nbrOfWorkers()
  num_tiles <- num_workers * 2
  message(paste(
    "Processing",
    num_tiles,
    "tiles using",
    num_workers,
    "future workers..."
  ))
  y_breaks <- round(seq(ymin, ymax, length.out = num_tiles + 1))
  tile_bboxes <- lapply(1:num_tiles, function(i) {
    sf::st_bbox(
      c(xmin = xmin, ymin = y_breaks[i], xmax = xmax, ymax = y_breaks[i + 1]),
      crs = grid_crs
    )
  })

  # --- 3. PROCESS TILES IN PARALLEL ---
  grid_chunks <- furrr::future_map(
    .x = tile_bboxes,
    .f = ~ {
      # Set clip_to_input to FALSE because clipping happens on the full geometry
      # in the main session, not per-tile.
      args_for_tile <- c(list(grid_extent = .x), all_args)
      args_for_tile$clip_to_input <- FALSE
      do.call(create_grid_internal, args_for_tile)
    },
    .progress = TRUE,
    .options = furrr::furrr_options(seed = TRUE)
  )

  # --- 4. COMBINE, CLIP, AND DE-DUPLICATE ---
  message("Combining and finalizing results...")
  final_grid <- do.call(rbind, grid_chunks)
  if (nrow(final_grid) == 0) {
    return(final_grid)
  }

  # Manual clipping of the combined grid
  if (!is.null(clipping_target)) {
    intersects_indices <- sf::st_intersects(final_grid, clipping_target)
    final_grid <- final_grid[lengths(intersects_indices) > 0, ]
  }
  if (nrow(final_grid) == 0) {
    return(final_grid)
  }

  # De-duplicate
  id_col_name <- grep("ID", names(final_grid), value = TRUE)[1]
  if (!is.na(id_col_name)) {
    num_before <- nrow(final_grid)
    final_grid <- final_grid[!duplicated(final_grid[[id_col_name]]), ]
    num_removed <- num_before - nrow(final_grid)
    if (num_removed > 0) {
      message(paste(
        "Removed",
        num_removed,
        "duplicate cells from tile boundaries."
      ))
    }
  }

  return(final_grid)
}


# Internal function to run grid creation using the `mirai` framework.
run_parallel_mirai <- function(grid_extent, cellsize_m, crs, dot_args) {
  if (
    !all(sapply(
      c("purrr", "mirai", "carrier"),
      requireNamespace,
      quietly = TRUE
    ))
  ) {
    stop(
      "Packages 'purrr', 'mirai', and 'carrier' are required.",
      call. = FALSE
    )
  }

  # --- 1. PREPARE GEOMETRIES AND TILES ---
  grid_crs <- crs %||% sf::st_crs(grid_extent)
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

  # --- 2. CREATE TILES ---
  full_bbox <- sf::st_bbox(grid_extent)
  xmin <- floor(as.numeric(full_bbox["xmin"]) / cellsize_m) * cellsize_m
  ymin <- floor(as.numeric(full_bbox["ymin"]) / cellsize_m) * cellsize_m
  xmax <- ceiling(as.numeric(full_bbox["xmax"]) / cellsize_m) * cellsize_m
  ymax <- ceiling(as.numeric(full_bbox["ymax"]) / cellsize_m) * cellsize_m

  num_daemons <- mirai::status()$connections
  num_tiles <- if (num_daemons > 0) num_daemons * 2 else 2
  message(paste(
    "Processing",
    num_tiles,
    "tiles using",
    num_daemons,
    "mirai daemons..."
  ))
  y_breaks <- round(seq(ymin, ymax, length.out = num_tiles + 1))
  tile_bboxes <- lapply(1:num_tiles, function(i) {
    sf::st_bbox(
      c(xmin = xmin, ymin = y_breaks[i], xmax = xmax, ymax = y_breaks[i + 1]),
      crs = grid_crs
    )
  })

  # --- 3. PROCESS TILES IN PARALLEL ---
  # Create a "crate" that contains the workhorse function and its arguments
  # This is sent to each worker.
  parallel_worker <- carrier::crate(
    function(tile_bbox) {
      args_for_tile <- c(list(grid_extent = tile_bbox), all_args)
      args_for_tile$clip_to_input <- FALSE
      do.call(create_grid_internal, args_for_tile)
    },
    create_grid_internal = create_grid_internal,
    all_args = all_args
  )

  grid_chunks <- purrr::map(tile_bboxes, parallel_worker, .progress = TRUE)

  # --- 4. COMBINE, CLIP, AND DE-DUPLICATE ---
  message("Combining and finalizing results...")
  final_grid <- do.call(rbind, purrr::compact(grid_chunks))
  if (nrow(final_grid) == 0) {
    return(final_grid)
  }

  if (!is.null(clipping_target)) {
    intersects_indices <- sf::st_intersects(final_grid, clipping_target)
    final_grid <- final_grid[lengths(intersects_indices) > 0, ]
  }
  if (nrow(final_grid) == 0) {
    return(final_grid)
  }

  id_col_name <- grep("ID", names(final_grid), value = TRUE)[1]
  if (!is.na(id_col_name)) {
    num_before <- nrow(final_grid)
    final_grid <- final_grid[!duplicated(final_grid[[id_col_name]]), ]
    num_removed <- num_before - nrow(final_grid)
    if (num_removed > 0) {
      message(paste(
        "Removed",
        num_removed,
        "duplicate cells from tile boundaries."
      ))
    }
  }

  return(final_grid)
}
