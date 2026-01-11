#' Internal function to run grid creation using the `mirai` framework.
#' @keywords internal
#' @noRd
run_parallel_mirai <- function(grid_extent, cellsize_m, crs, dot_args) {
  if (
    !all(sapply(
      c("purrr", "mirai", "carrier"),
      requireNamespace,
      quietly = TRUE
    ))
  ) {
    stop("Packages 'purrr', 'mirai', and 'carrier' are required.")
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
  full_bbox <- .get_bbox_from_grid_extent(grid_extent, grid_crs)
  xmin <- floor(as.numeric(full_bbox["xmin"]) / cellsize_m) * cellsize_m
  ymin <- floor(as.numeric(full_bbox["ymin"]) / cellsize_m) * cellsize_m
  xmax <- ceiling(as.numeric(full_bbox["xmax"]) / cellsize_m) * cellsize_m
  ymax <- ceiling(as.numeric(full_bbox["ymax"]) / cellsize_m) * cellsize_m

  # Estimate total cells to optimize worker count
  width_m <- xmax - xmin
  height_m <- ymax - ymin
  total_cells_est <- (width_m / cellsize_m) * (height_m / cellsize_m)

  num_daemons <- mirai::status()$connections

  # Heuristic: Limit active workers for small grids to avoid overhead
  # Based on multi-resolution benchmarks (50m-1000m cell sizes):
  # < 50k cells: max 4 workers (parallel overhead ~50% of runtime)
  # < 500k cells: max 8 workers (optimal for most scenarios)
  # < 2M cells: max 16 workers
  # >= 2M cells: use all workers, but warn if >32
  effective_workers <- num_daemons
  if (total_cells_est < 50000) {
    effective_workers <- min(num_daemons, 4)
  } else if (total_cells_est < 500000) {
    effective_workers <- min(num_daemons, 8)
  } else if (total_cells_est < 2000000) {
    effective_workers <- min(num_daemons, 16)
  }

  # Default multiplier is now 1 based on benchmarks
  # tile_mult=1 consistently performs best; tile_mult=2 hurts performance
  default_multiplier <- 1
  tile_multiplier <- getOption(
    "gridmaker.tile_multiplier",
    default = default_multiplier
  )

  # Calculate num_tiles using effective_workers
  # If user explicitly set multiplier, we use all daemons (assuming they know what they are doing)
  # Otherwise we use the effective worker count to limit overhead
  user_set_multiplier <- !is.null(getOption("gridmaker.tile_multiplier"))

  base_workers <- if (user_set_multiplier) num_daemons else effective_workers

  num_tiles <- if (base_workers > 0) {
    as.integer(round(base_workers * tile_multiplier))
  } else {
    2
  }

  # Warning for suboptimal configurations based on benchmark data
  if (!quiet) {
    if (num_daemons > 32) {
      message(
        "Note: You have ",
        num_daemons,
        " workers configured. Benchmarks show that >32 workers ",
        "often decrease performance due to overhead. Consider using 8-16 workers."
      )
    }

    if (user_set_multiplier && tile_multiplier > 1) {
      message(
        "Note: You set gridmaker.tile_multiplier = ",
        tile_multiplier,
        ". Benchmarks show tile_multiplier = 1 performs best in most cases."
      )
    }
  }

  if (!quiet) {
    msg <- paste(
      "Processing",
      num_tiles,
      "tiles using",
      num_daemons,
      "mirai daemons"
    )
    if (effective_workers < num_daemons && !user_set_multiplier) {
      msg <- paste0(
        msg,
        " (limited to ",
        effective_workers,
        " active to reduce overhead)"
      )
    }
    message(paste0(msg, "..."))
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
  parallel_worker <- carrier::crate(
    function(tile_bbox) {
      args_for_tile <- c(list(grid_extent = tile_bbox), all_args)
      args_for_tile$clip_to_input <- FALSE
      chunk <- do.call(inspire_grid_from_extent_internal, args_for_tile)
      if (nrow(chunk) > 0 && !is.null(clipping_target)) {
        intersects_indices <- sf::st_intersects(chunk, clipping_target)
        chunk <- chunk[lengths(intersects_indices) > 0, ]
      }
      if (nrow(chunk) == 0) NULL else chunk
    },
    all_args = all_args,
    clipping_target = clipping_target,
    inspire_grid_from_extent_internal = inspire_grid_from_extent_internal,
    as_inspire_grid = as_inspire_grid,
    as_inspire_grid_polygons = as_inspire_grid_polygons,
    as_inspire_grid_points = as_inspire_grid_points,
    as_inspire_grid_coordinates = as_inspire_grid_coordinates
  )

  grid_chunks_promises <- mirai::mirai_map(tile_bboxes, parallel_worker)
  grid_chunks <- grid_chunks_promises[]

  # --- 4. COMBINE (No de-duplication needed) ---
  if (!quiet) {
    message("Combining results...")
  }
  final_grid <- do.call(rbind, purrr::compact(grid_chunks))
  return(final_grid)
}
