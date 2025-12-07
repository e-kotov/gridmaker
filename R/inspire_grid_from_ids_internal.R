inspire_grid_from_ids_internal <- function(
  ids,
  point_type = c("llc", "centroid"),
  output_type = c("sf_polygons", "sf_points", "dataframe"),
  include_llc = TRUE,
  quiet = FALSE,
  dsn = NULL,
  layer = NULL,
  ...
) {
  output_type <- match.arg(output_type)
  point_type <- match.arg(point_type)

  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("The 'sf' package is required. Please install it.", call. = FALSE)
  }

  if (
    output_type == "sf_polygons" &&
      !requireNamespace("sfheaders", quietly = TRUE)
  ) {
    stop(
      "Package 'sfheaders' is required for 'sf' output. Please install it.",
      call. = FALSE
    )
  }

  # Parse IDs to get coords and validate consistency across all IDs
  grid_df <- inspire_id_to_coords(ids, as_sf = FALSE)
  names(grid_df) <- c("crs", "cellsize", "Y_LLC", "X_LLC")

  if (length(unique(grid_df$crs)) > 1) {
    stop(
      "Invalid CRS: Multiple coordinate reference systems found. Please ensure that all INSPIRE IDs have the same CRS.",
      call. = FALSE
    )
  }

  if (length(unique(grid_df$cellsize)) > 1) {
    stop(
      "Invalid cell size: Multiple different cell sizes found. Please ensure that all INSPIRE IDs refer to the same cell size.",
      call. = FALSE
    )
  }

  grid_crs <- sf::st_crs(grid_df$crs[[1]])
  if (is.na(grid_crs) || sf::st_is_longlat(grid_crs)) {
    stop(
      "Invalid CRS: The coordinate reference system must be a projected system (e.g., EPSG:3035) and not a geographic one (like WGS84, EPSG:4326).",
      call. = FALSE
    )
  }

  cellsize <- grid_df$cellsize[[1]]

  # --- 1. In-Memory Generation (dsn is NULL) ---
  if (is.null(dsn)) {
    out_obj <- as_inspire_grid(
      grid_df,
      cellsize = cellsize,
      crs = grid_crs,
      output_type = output_type,
      point_type = point_type
    )

    # Add ID (Specific to this function)
    out_obj$id <- ids

    # Cleanup and reorder using helper
    return(clean_and_order_grid(
      out_obj,
      output_type = output_type,
      point_type = point_type,
      include_llc = include_llc
    ))
  }

  # --- 2. Write to Disk (Streaming/Chunking) ---
  # 1. Validate extension vs output_type
  validate_disk_compatibility(output_type, dsn)

  if (is.null(layer)) {
    layer <- tools::file_path_sans_ext(basename(dsn))
    if (!quiet) message("`layer` not specified, defaulting to '", layer, "'.")
  }

  if (file.exists(dsn)) {
    if (!quiet) message("Output file '", dsn, "' exists and will be overwritten.")
    unlink(dsn, recursive = TRUE)
  }

  n_total <- nrow(grid_df)
  # 50,000 IDs per chunk is a conservative balance for memory vs IO
  chunk_size <- 50000
  n_chunks <- ceiling(n_total / chunk_size)

  if (!quiet) {
    message("Writing ", n_total, " grid cells to '", dsn, "'...")
  }

  for (i in seq_len(n_chunks)) {
    start_idx <- (i - 1) * chunk_size + 1
    end_idx <- min(i * chunk_size, n_total)
    idx <- start_idx:end_idx

    chunk_df <- grid_df[idx, , drop = FALSE]
    chunk_ids <- ids[idx]

    chunk_obj <- as_inspire_grid(
      chunk_df,
      cellsize = cellsize,
      crs = grid_crs,
      output_type = output_type,
      point_type = point_type
    )

    chunk_obj$id <- chunk_ids

    # Use helper for consistency
    chunk_obj <- clean_and_order_grid(
      chunk_obj,
      output_type = output_type,
      point_type = point_type,
      include_llc = include_llc
    )

    write_grid_chunk(
      chunk = chunk_obj,
      dsn = dsn,
      layer = layer,
      append = (i > 1),
      quiet = TRUE,
      ...
    )
  }

  if (!quiet) message("Done.")

  invisible(dsn)
}
