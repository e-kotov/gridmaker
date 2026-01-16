#' Stream a raster grid to disk using terra
#'
#' Internal function to create a grid and stream it to a file using terra's
#' writeStart/writeValues/writeStop API for efficient chunked processing.
#'
#' @note This uses terra's explicit write API with `terra::blocks()` for
#'   optimal memory management during grid generation.
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

  # --- RAT SUPPORT DISABLED ---
  # See RASTER_REFACTORING_PLAN.md for rationale
  include_rat <- isTRUE(dot_args$include_rat)
  if (include_rat) {
    warning(
      "The 'include_rat' parameter is deprecated and has no effect. ",
      "To generate a Raster Attribute Table, create the grid in-memory ",
      "and use terra::levels() to attach the RAT before saving.",
      call. = FALSE
    )
  }

  if (!quiet) {
    message("Streaming raster to disk using terra (chunked processing)...")
  }

  # --- 2. PREPARE GRID GEOMETRY ---
  grid_crs <- sf::st_crs(crs %||% sf::st_crs(grid_extent))
  if (is.na(grid_crs) || sf::st_is_longlat(grid_crs)) {
    stop("A projected CRS is required.")
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

  # Determine CRS string for terra (prefer WKT)
  crs_string <- if (!is.na(grid_crs)) grid_crs$wkt else ""

  # --- 3. CREATE TEMPLATE RASTER ---
  r_template <- terra::rast(
    nrows = nrows,
    ncols = ncols,
    xmin = xmin,
    xmax = xmax,
    ymin = ymin,
    ymax = ymax,
    crs = crs_string
  )

  # --- 4. PREPARE GDAL OPTIONS ---
  gdal_opts <- c(
    "COMPRESS=DEFLATE",
    "TILED=YES",
    "BIGTIFF=IF_NEEDED"
  )

  datatype <- dot_args$datatype
  if (is.null(datatype)) {
    datatype <- "INT4S"
  }

  # --- 5. STREAM USING writeStart/writeValues/writeStop ---
  # This is the terra-recommended pattern for large raster generation

  b <- terra::writeStart(
    r_template,
    filename = dsn,
    overwrite = TRUE,
    wopt = list(
      datatype = datatype,
      gdal = gdal_opts
    )
  )

  if (!quiet) {
    message(sprintf("  Processing %d chunks...", b$n))
  }

  nc <- ncols # number of columns

  for (i in 1:b$n) {
    # Get cell range for this chunk
    # Pattern from terra/R/interpolate.R:
    #   xyFromCell(out, cellFromRowCol(out, b$row[i], 1):cellFromRowCol(out, b$row[i]+b$nrows[i]-1, nc))
    first_cell <- terra::cellFromRowCol(r_template, b$row[i], 1)
    last_cell <- terra::cellFromRowCol(
      r_template,
      b$row[i] + b$nrows[i] - 1,
      nc
    )

    cells <- first_cell:last_cell
    cell_ids <- cells

    # Write chunk values
    terra::writeValues(
      r_template,
      cell_ids,
      start = b$row[i],
      nrows = b$nrows[i]
    )

    # Progress reporting
    if (!quiet && (i %% max(1, b$n %/% 10) == 0 || i == b$n)) {
      pct <- round(i / b$n * 100)
      message(sprintf("  Progress: %d%% (%d/%d chunks)", pct, i, b$n))
    }
  }

  # Finalize the raster file
  r_out <- terra::writeStop(r_template)

  # --- 6. POST-PROCESSING (CLIPPING) ---
  if (isTRUE(dot_args$clip_to_input)) {
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

      # Use the same extension as the output file to preserve format
      output_ext <- tools::file_ext(dsn)
      temp_masked <- tempfile(
        fileext = if (nzchar(output_ext)) paste0(".", output_ext) else ""
      )

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

  return(invisible(dsn))
}

# =============================================================================
# RAT SUPPORT DISABLED - See RASTER_REFACTORING_PLAN.md
# =============================================================================
# The following RAT-related code has been disabled. Users who need Raster
# Attribute Tables should:
# 1. Generate the grid in-memory: r <- inspire_grid(..., output_type = "spatraster")
# 2. Attach the RAT manually: terra::levels(r) <- rat_dataframe
# 3. Write to file: terra::writeRaster(r, "output.tif")
#
# Previous RAT implementation (commented out for reference):
#
# if (include_rat) {
#   n_cells <- nrows * ncols
#   if (!quiet) {
#     message("Generating Raster Attribute Table (RAT)...")
#   }
#   r_final <- terra::rast(dsn)
#   coords <- terra::xyFromCell(r_final, seq_len(n_cells))
#   x_llc <- coords[, 1] - (cellsize_m / 2)
#   y_llc <- coords[, 2] - (cellsize_m / 2)
#
#   # ID generation logic
#   nzeros <- .tz_count(cellsize_m)
#   div <- as.integer(10^nzeros)
#   size_lbl <- if (cellsize_m >= 1000) {
#     paste0(cellsize_m / 1000, "km")
#   } else {
#     paste0(cellsize_m, "m")
#   }
#   epsg <- grid_crs$epsg %||% 3035
#   axis_order <- dot_args$axis_order %||% "NE"
#   id_format <- dot_args$id_format %||% "both"
#
#   id_long <- sprintf("CRS%sRES%smN%.0fE%.0f", epsg, cellsize_m, y_llc, x_llc)
#   id_short <- if (axis_order == "NE") {
#     sprintf("%sN%.0fE%.0f", size_lbl, y_llc / div, x_llc / div)
#   } else {
#     sprintf("%sE%.0fN%.0f", size_lbl, x_llc / div, y_llc / div)
#   }
#
#   # Create RAT dataframe
#   if (id_format == "both") {
#     rat_df <- data.frame(
#       Value = seq_len(n_cells),
#       GRD_ID_SHORT = id_short,
#       GRD_ID_LONG = id_long,
#       stringsAsFactors = FALSE
#     )
#   } else if (id_format == "short") {
#     rat_df <- data.frame(Value = seq_len(n_cells), GRD_ID = id_short)
#   } else {
#     rat_df <- data.frame(Value = seq_len(n_cells), GRD_ID = id_long)
#   }
#
#   # Filter RAT to exclude entries for NA cells (after clipping)
#   raster_values <- terra::values(r_final)
#   valid_values <- unique(raster_values[!is.na(raster_values)])
#   if (length(valid_values) < n_cells) {
#     rat_df <- rat_df[rat_df$Value %in% valid_values, ]
#     if (!quiet) {
#       message("  Filtered RAT: ", nrow(rat_df), " entries ",
#               "(removed ", n_cells - nrow(rat_df), " entries for masked cells)")
#     }
#   }
#
#   # ... RAT persistence code omitted for brevity ...
# }
# =============================================================================
