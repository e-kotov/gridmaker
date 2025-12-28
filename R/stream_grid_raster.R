#' Stream a raster grid to disk using terra
#'
#' Internal function to create a grid and stream it to a file using terra's
#' native disk-based processing.
#'
#' @note This leverages terra's `todisk=TRUE` option and `app()` function to
#'   process grid generation in chunks, minimizing memory usage.
#' @keywords internal
#' @noRd
stream_grid_raster_terra <- function(
  grid_extent,
  cellsize_m,
  crs,
  dsn,
  layer,
  dot_args,
  quiet = FALSE,
  max_memory_gb = NULL
) {
  # --- 1. SETUP ---
  # Configure terra options for this session
  # We use on.exit to restore original options
  old_opts <- terra::terraOptions()
  on.exit(suppressWarnings(do.call(terra::terraOptions, old_opts)), add = TRUE)

  if (!is.null(max_memory_gb)) {
    terra::terraOptions(memmax = max_memory_gb, todisk = TRUE)
  } else {
    terra::terraOptions(todisk = TRUE)
    # Use a conservative memory fraction if no explicit limit is set
    # to ensure we leave room for system overhead
    if (is.null(old_opts$memfrac) || old_opts$memfrac > 0.6) {
      terra::terraOptions(memfrac = 0.5)
    }
  }

  if (!quiet) {
    message("Streaming raster to disk using terra (chunked processing)...")
  }

  # --- 2. PREPARE GRID GEOMETRY ---
  grid_crs <- sf::st_crs(crs %||% sf::st_crs(grid_extent))
  if (is.na(grid_crs) || sf::st_is_longlat(grid_crs)) {
    stop("A projected CRS is required.")
  }

  include_rat <- isTRUE(dot_args$include_rat)
  ext <- tolower(tools::file_ext(dsn))

  # Early validation for include_rat formats
  if (include_rat && !is.null(dsn)) {
    if (ext %in% c("hdf", "hdf5", "h5")) {
      stop(
        "Raster Attribute Table (RAT) is not supported for HDF5 format. ",
        "Use 'include_rat = FALSE' or choose a different format (e.g., .nc, .kea, .tif).",
        call. = FALSE
      )
    }
  }

  # Calculate aligned extent
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

  # Determine CRS string for terra
  # terra prefers WKT or PROJ string
  crs_string <- if (!is.na(grid_crs)) grid_crs$wkt else ""

  # --- 3. CREATE VIRTUAL COORDINATE RASTERS ---
  # Create a template raster (virtual, no values)
  r_template <- terra::rast(
    nrows = nrows,
    ncols = ncols,
    xmin = xmin,
    xmax = xmax,
    ymin = ymin,
    ymax = ymax,
    crs = crs_string
  )

  # Create virtual coordinate rasters
  # These generate coordinates on-the-fly when accessed
  r_x <- terra::init(r_template, "x")
  r_y <- terra::init(r_template, "y")

  # Stack them for app() processing
  coords_stack <- c(r_x, r_y)

  # --- 4. PREPARE GENERATOR FUNCTION ---

  # Extract parameters needed inside the function
  # Note: Functions passed to app() must be self-contained or explicitly import vars

  # Define the function that converts coords to grid values
  # We return standard 1-based cell indices/IDs unless a specific format requires otherwise
  # Note: Generating complex character strings (INSPIRE IDs) in streaming raster
  # is memory-prohibitive for the RAT (Raster Attribute Table).
  # We stick to numeric cell indices/IDs which are efficient.

  # If clipping is requested, we need to handle it.
  # However, passing large vector objects to worker chunks in app() might be slow.
  # For streaming raster, masking *after* generation or during generation?
  # terra::mask() is also disk-based and chunked.

  # Generation function
  # Note: Captures xmin, ymax, cellsize_m, nrows, ncols from outer scope
  generator_fun <- function(xy) {
    # xy is a matrix/vector of coordinates [x, y]
    # For large chunks, it's a matrix
    if (is.null(dim(xy))) {
      x_vals <- xy[1]
      y_vals <- xy[2]
    } else {
      x_vals <- xy[, 1]
      y_vals <- xy[, 2]
    }

    # Calculate row/col indices (1-based)
    # Column: floor((x - xmin) / cellsize) + 1
    # Row: floor((ymax - y) / cellsize) + 1  (Note Y axis direction)

    col_idx <- floor((x_vals - xmin) / cellsize_m) + 1
    row_idx <- floor((ymax - y_vals) / cellsize_m) + 1

    # Calculate linear cell index (row-major order)
    # (row - 1) * ncols + col
    cell_id <- (row_idx - 1) * ncols + col_idx

    return(cell_id)
  }

  # --- 5. EXECUTE STREAMING ---

  # Prepare GDAL options (compression is key for large files)
  gdal_opts <- c(
    "COMPRESS=DEFLATE",
    "TILED=YES",
    "BIGTIFF=IF_NEEDED" # Important for >4GB files
  )

  # Execute app()
  # This runs in chunks, writing directly to disk
  terra::app(
    coords_stack,
    fun = generator_fun,
    filename = dsn,
    overwrite = TRUE,
    wopt = list(
      datatype = "INT4S", # 4-byte signed integer (supports up to ~2 billion cells)
      gdal = gdal_opts
    )
  )

  # --- 6. POST-PROCESSING (CLIPPING) ---
  if (isTRUE(dot_args$clip_to_input)) {
    # If clipping is needed, we apply a mask
    # This reads the just-created file and writes a new one (or updates it?)
    # terra::mask creates a NEW file.

    # If we want to mask in place, it's tricky.
    # Better to mask during generation?
    # But checking point-in-polygon for every cell is EXPENSIVE.
    # Vectorized masking via terra::mask is highly optimized (rasterize vector -> mask).

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

      # We need a temp file for the masked output if we want to overwrite 'dsn'
      temp_masked <- tempfile(fileext = ".tif")

      # Load the raster we just created
      r_raw <- terra::rast(dsn)

      # Convert sf target to terra::vect
      v_target <- terra::vect(clipping_target)

      # Apply mask
      terra::mask(
        r_raw,
        v_target,
        filename = temp_masked,
        overwrite = TRUE,
        wopt = list(gdal = gdal_opts)
      )

      # Replace original with masked
      file.copy(temp_masked, dsn, overwrite = TRUE)
      unlink(temp_masked)
    }
  }

  # --- 7. RAT GENERATION (OPTIONAL) ---
  if (include_rat) {
    n_cells <- nrows * ncols

    if (!quiet) {
      message("Generating Raster Attribute Table (RAT)...")
    }

    # Generate ID strings for all cells
    r_final <- terra::rast(dsn)
    coords <- terra::xyFromCell(r_final, seq_len(n_cells))
    x_llc <- coords[, 1] - (cellsize_m / 2)
    y_llc <- coords[, 2] - (cellsize_m / 2)

    # ID generation logic
    nzeros <- .tz_count(cellsize_m)
    div <- as.integer(10^nzeros)
    size_lbl <- if (cellsize_m >= 1000) {
      paste0(cellsize_m / 1000, "km")
    } else {
      paste0(cellsize_m, "m")
    }
    epsg <- grid_crs$epsg %||% 3035
    axis_order <- dot_args$axis_order %||% "NE"
    id_format <- dot_args$id_format %||% "both"

    id_long <- sprintf("CRS%sRES%smN%.0fE%.0f", epsg, cellsize_m, y_llc, x_llc)
    id_short <- if (axis_order == "NE") {
      sprintf("%sN%.0fE%.0f", size_lbl, y_llc / div, x_llc / div)
    } else {
      sprintf("%sE%.0fN%.0f", size_lbl, x_llc / div, y_llc / div)
    }

    # Create RAT dataframe
    if (id_format == "both") {
      rat_df <- data.frame(
        Value = seq_len(n_cells),
        GRD_ID_SHORT = id_short,
        GRD_ID_LONG = id_long,
        stringsAsFactors = FALSE
      )
    } else if (id_format == "short") {
      rat_df <- data.frame(Value = seq_len(n_cells), GRD_ID = id_short)
    } else {
      rat_df <- data.frame(Value = seq_len(n_cells), GRD_ID = id_long)
    }

    # Filter RAT to exclude entries for NA cells (after clipping)
    # This ensures the RAT only contains entries for valid raster cells
    raster_values <- terra::values(r_final)
    valid_values <- unique(raster_values[!is.na(raster_values)])
    if (length(valid_values) < n_cells) {
      rat_df <- rat_df[rat_df$Value %in% valid_values, ]
      if (!quiet) {
        message(
          "  Filtered RAT: ",
          nrow(rat_df),
          " entries ",
          "(removed ",
          n_cells - nrow(rat_df),
          " entries for masked cells)"
        )
      }
    }

    # Format-specific persistence
    if (ext %in% c("nc", "kea")) {
      # NetCDF/KEA: Native support via levels() + writeRaster
      if (!quiet) {
        message("Writing RAT natively (", toupper(ext), ")...")
      }
      levels(r_final) <- rat_df
      names(r_final) <- names(rat_df)[2]

      # Write to temp then replace (can't overwrite source)
      temp_out <- tempfile(fileext = paste0(".", ext))
      terra::writeRaster(r_final, temp_out, overwrite = TRUE)
      file.copy(temp_out, dsn, overwrite = TRUE)
      unlink(temp_out)
    } else if (ext %in% c("tif", "tiff")) {
      # GeoTIFF: Double-write required
      if (!quiet) {
        message(
          "Writing RAT for GeoTIFF (double-write required)...\n",
          "Note: For large grids, consider NetCDF (.nc) or KEA (.kea) formats."
        )
      }

      levels(r_final) <- rat_df
      names(r_final) <- names(rat_df)[2]

      # Write to temp then replace
      temp_out <- tempfile(fileext = ".tif")
      terra::writeRaster(
        r_final,
        temp_out,
        overwrite = TRUE,
        wopt = list(datatype = "INT4S", gdal = gdal_opts)
      )
      file.copy(temp_out, dsn, overwrite = TRUE)
      unlink(temp_out)

      # Also copy .aux.xml if created
      aux_temp <- paste0(temp_out, ".aux.xml")
      aux_final <- paste0(dsn, ".aux.xml")
      if (file.exists(aux_temp)) {
        file.copy(aux_temp, aux_final, overwrite = TRUE)
        unlink(aux_temp)
      }
    } else {
      # Other formats: Try native levels()
      if (!quiet) {
        message("Attempting RAT for format: ", ext)
      }
      levels(r_final) <- rat_df
      names(r_final) <- names(rat_df)[2]

      temp_out <- tempfile(fileext = paste0(".", ext))
      terra::writeRaster(r_final, temp_out, overwrite = TRUE)
      file.copy(temp_out, dsn, overwrite = TRUE)
      unlink(temp_out)
    }
  }

  return(invisible(dsn))
}
