# This is the internal, non-exported workhorse function for creating a grid.
# It is called either directly for sequential processing or by the parallel
# handlers for each tile.
inspire_grid_from_extent_internal <- function(
  grid_extent,
  cellsize_m,
  crs = NULL,
  output_type = c("sf_polygons", "sf_points", "dataframe", "spatraster"),
  clip_to_input = FALSE,
  use_convex_hull = FALSE,
  buffer_m = 0,
  id_format = c("both", "long", "short", "none"),
  axis_order = c("NE", "EN"),
  include_llc = TRUE,
  point_type = c("centroid", "llc"),
  vector_grid_backend = getOption("gridmaker.vector_grid_backend", "cpp"),
  ...
) {
  # --- 1. PRE-CHECKS AND HELPERS ---
  output_type <- match.arg(output_type)
  id_format <- match.arg(id_format)
  axis_order <- match.arg(axis_order)
  point_type <- match.arg(point_type)
  vector_grid_backend <- match.arg(vector_grid_backend, c("cpp", "sfheaders"))

  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("The 'sf' package is required. Please install it.", call. = FALSE)
  }
  if (
    output_type == "sf_polygons" &&
      vector_grid_backend == "sfheaders" &&
      !requireNamespace("sfheaders", quietly = TRUE)
  ) {
    stop(
      "Package 'sfheaders' is required for the legacy R backend. Please install it.",
      call. = FALSE
    )
  }



  # --- 2. VALIDATE INPUTS & DETERMINE CRS ---
  cellsize_m <- suppressWarnings(as.numeric(cellsize_m))
  if (
    is.na(cellsize_m) ||
      cellsize_m <= 0 ||
      abs(cellsize_m - round(cellsize_m)) > 1e-9
  ) {
    stop("cellsize_m must be a positive integer (metres).", call. = FALSE)
  }
  cellsize_m <- as.integer(round(cellsize_m))

  grid_crs <- if (!is.null(crs)) sf::st_crs(crs) else sf::st_crs(NA)
  input_crs <- sf::st_crs(NA)
  if (inherits(grid_extent, c("sf", "sfc", "bbox"))) {
    input_crs <- sf::st_crs(grid_extent)
  }
  if (is.na(grid_crs)) {
    if (is.na(input_crs)) {
      stop(
        "A CRS must be provided either via the 'crs' parameter or as part of the 'grid_extent' object.",
        call. = FALSE
      )
    }
    grid_crs <- input_crs
  }

  if (is.na(grid_crs) || sf::st_is_longlat(grid_crs)) {
    stop(
      "Invalid CRS: The coordinate reference system must be a projected system (e.g., EPSG:3035) and not a geographic one (like WGS84, EPSG:4326).",
      call. = FALSE
    )
  }

  if (output_type != "sf_points" && point_type != "centroid") {
    warning(
      sprintf(
        "Argument 'point_type' ('%s') is ignored when output_type is '%s'.",
        point_type,
        output_type
      ),
      call. = FALSE
    )
  }

  # --- 3. PROCESS INPUT GEOMETRY ---
  input_sf <- NULL
  if (inherits(grid_extent, c("sf", "sfc"))) {
    input_sf <- grid_extent
    if (any(!sf::st_is_valid(input_sf), na.rm = TRUE)) {
      input_sf <- sf::st_make_valid(input_sf)
    }
    if (!is.na(sf::st_crs(input_sf)) && sf::st_crs(input_sf) != grid_crs) {
      input_sf <- sf::st_transform(input_sf, grid_crs)
    }
    if (all(sf::st_is_empty(input_sf))) {
      stop("Input geometry is empty.", call. = FALSE)
    }
    bbox <- sf::st_bbox(input_sf)
  } else if (inherits(grid_extent, "bbox")) {
    bbox_sfc <- sf::st_as_sfc(grid_extent)
    if (sf::st_crs(bbox_sfc) != grid_crs) {
      bbox_sfc <- sf::st_transform(bbox_sfc, grid_crs)
    }
    bbox <- sf::st_bbox(bbox_sfc)
  } else if (is.matrix(grid_extent) && all(dim(grid_extent) == c(2, 2))) {
    bbox <- sf::st_bbox(
      c(
        xmin = grid_extent[1, 1],
        ymin = grid_extent[2, 1],
        xmax = grid_extent[1, 2],
        ymax = grid_extent[2, 2]
      ),
      crs = grid_crs
    )
  } else if (is.numeric(grid_extent) && length(grid_extent) == 4) {
    bbox <- sf::st_bbox(
      c(
        xmin = grid_extent[1],
        ymin = grid_extent[2],
        xmax = grid_extent[3],
        ymax = grid_extent[4]
      ),
      crs = grid_crs
    )
  } else {
    stop(
      "Invalid grid_extent: provide sf/sfc/bbox, 2x2 matrix, or numeric c(xmin,ymin,xmax,ymax).",
      call. = FALSE
    )
  }

  if (any(!is.finite(bbox))) {
    stop("Input geometry has a non-finite bounding box.", call. = FALSE)
  }

  # --- 4. CREATE ALIGNED BOUNDING BOX ---
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

  # --- 5. PREPARE CLIPPING TARGET (needed for both raster and vector) ---
  clipping_target <- NULL
  if (clip_to_input) {
    if (!is.null(input_sf)) {
      target <- if (use_convex_hull) {
        sf::st_convex_hull(sf::st_union(input_sf))
      } else {
        input_sf
      }
      if (buffer_m > 0) {
        target <- sf::st_buffer(target, dist = buffer_m)
      }
      clipping_target <- target
    } else {
      warning(
        "Clipping requested but grid_extent is not a spatial object; skipping clip.",
        call. = FALSE
      )
    }
  }

  # --- 6. RASTER OUTPUT ---
  if (output_type == "spatraster") {
    # 1. Create Template Raster
    r <- terra::rast(
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      ymax = ymax,
      resolution = cellsize_m,
      crs = if (!is.na(grid_crs)) grid_crs$wkt else NA
    )

    # 2. Assign Values (Sequential Integers)
    # We init with 1..N so we can attach attributes (levels) later
    terra::values(r) <- seq_len(terra::ncell(r))

    # 3. Generate IDs and Assign as Levels (Factor Raster)
    if (id_format != "none") {
      # xyFromCell returns centroids. For large rasters, this is memory-intensive.
      # We replicate the logic using efficient vector generation.
      # Rows (Y) are outer loop (slowest changing), Columns (X) are inner loop (fastest).
      
      nrows <- terra::nrow(r)
      ncols <- terra::ncol(r)

      # Calculate LLC for make_ids directly
      # X LLC: seq from xmin
      x_llc_seq <- seq.int(from = xmin, by = cellsize_m, length.out = ncols)
      # Y LLC: seq from ymax - cellsize (top-left Y LLC) down to ymin
      y_llc_seq <- seq.int(from = ymax - cellsize_m, by = -cellsize_m, length.out = nrows)
      
      # Create full vectors
      x_llc <- rep(x_llc_seq, times = nrows)
      y_llc <- rep(y_llc_seq, each = ncols)

      # Use C++ kernel for ID generation
      nzeros <- .tz_count(cellsize_m)
      div <- 10^nzeros
      size_lbl <- if (cellsize_m >= 1000) paste0(cellsize_m / 1000, "km") else paste0(cellsize_m, "m")

      ids_list <- generate_ids_rcpp(
        x_llc = x_llc,
        y_llc = y_llc,
        cellsize = as.double(cellsize_m),
        epsg = as.integer(grid_crs$epsg %||% 3035),
        size_lbl = size_lbl,
        divider = as.double(div),
        axis_order = axis_order,
        id_format = id_format
      )

      # Determine which ID format to use for the label
      # Rasters typically hold one label per cell
      final_ids <- if (id_format == "short") {
        ids_list$id_short
      } else if (id_format == "long") {
        ids_list$id_long
      } else if (id_format == "both") {
        # For "both", we use short as the primary label and include long in RAT
        ids_list$id_short
      }

      # Create Attribute Table (RAT)
      if (id_format == "both") {
        levels_df <- data.frame(
          id = seq_len(terra::ncell(r)),
          GRD_ID_SHORT = ids_list$id_short,
          GRD_ID_LONG = ids_list$id_long,
          stringsAsFactors = FALSE
        )
      } else {
        levels_df <- data.frame(
          id = seq_len(terra::ncell(r)),
          GRD_ID = final_ids,
          stringsAsFactors = FALSE
        )
      }

      # Assign to raster
      names(r) <- "grid_id"
      levels(r) <- levels_df
    } else {
      names(r) <- "cell_index"
    }

    # 4. Clipping
    if (clip_to_input && !is.null(clipping_target)) {
      # Convert sf clipping_target to SpatVector for terra
      v_target <- terra::vect(clipping_target)

      # Mask sets values outside geometry to NA
      r <- terra::mask(r, v_target)

      # Note: We do not run terra::trim() automatically to preserve the
      # grid alignment requested by the user, but this results in NA cells.
    }

    return(r)
  }

  # --- 7. GENERATE GRID POINTS ---
  # --- OPTIONAL C++ BACKEND ---
  # Only supported for sf_polygons and when backend="cpp" option is set
  if (
    output_type == "sf_polygons" && 
    isTRUE(vector_grid_backend == "cpp")
  ) {
    # Generate sequences
    x_coords <- seq.int(from = xmin, to = xmax - 1, by = cellsize_m)
    y_coords <- seq.int(from = ymin, to = ymax - 1, by = cellsize_m)
    
    # Expand grid VECTORS efficiently (flat N-length vectors)
    # expand.grid order: x varies fastest
    n_x <- length(x_coords)
    n_y <- length(y_coords)
    
    if (n_x > 0 && n_y > 0) {
      x_llc <- rep(x_coords, times = n_y)
      y_llc <- rep(y_coords, each = n_x) 
      
      # Call C++ Wrapper
      # If clipping, SKIP ID generation initially (huge speedup)
      should_generate_ids_now <- !(clip_to_input && !is.null(clipping_target))
      
      out_obj <- as_inspire_grid_cpp(
        x_llc, 
        y_llc, 
        cellsize_m, 
        grid_crs, 
        axis_order, 
        id_format,
        include_llc = include_llc,
        generate_ids = should_generate_ids_now
      )
      
      # Clipping (if requested)
      if (clip_to_input && !is.null(clipping_target)) {
        # Intersect full grid with target
        intersects_list <- sf::st_intersects(out_obj, clipping_target)
        keep_indices <- lengths(intersects_list) > 0
        
        # Subset output object
        out_obj <- out_obj[keep_indices, ]
        row.names(out_obj) <- NULL
        
        # Subset LLC coordinates to match kept indices for ID generation
        x_llc <- x_llc[keep_indices]
        y_llc <- y_llc[keep_indices]
        
        # Post-Clip ID Generation
        if (nrow(out_obj) > 0 && id_format != "none") {
            # Helper to generate IDs for the subset using C++
            nzeros <- .tz_count(cellsize_m)
            div <- 10^nzeros
            size_lbl <- if (cellsize_m >= 1000) paste0(cellsize_m / 1000, "km") else paste0(cellsize_m, "m")
            
            # Using the new exported ID generator
            ids <- generate_ids_rcpp(
                x_llc = x_llc,
                y_llc = y_llc,
                cellsize = as.double(cellsize_m),
                epsg = as.integer(grid_crs$epsg %||% 3035),
                size_lbl = size_lbl,
                divider = as.double(div),
                axis_order = axis_order,
                id_format = id_format
            )
            
            if (id_format == "both") {
                out_obj$GRD_ID_LONG <- ids$id_long
                out_obj$GRD_ID_SHORT <- ids$id_short
            } else if (id_format == "long") {
                out_obj$GRD_ID <- ids$id_long
            } else if (id_format == "short") {
                out_obj$GRD_ID <- ids$id_short
            }
        }
      }
      
      # Clean up columns and reorder
      out_obj <- clean_and_order_grid(
        out_obj, 
        output_type, 
        point_type, 
        include_llc
      )
      
      return(out_obj)
    }
  }

  x_coords <- seq.int(from = xmin, to = xmax - 1, by = cellsize_m)
  y_coords <- seq.int(from = ymin, to = ymax - 1, by = cellsize_m)
  if (length(x_coords) == 0 || length(y_coords) == 0) {
    return(
      if (output_type %in% c("sf_polygons", "sf_points")) {
        sf::st_sf(geometry = sf::st_sfc(crs = grid_crs))
      } else {
        data.frame()
      }
    )
  }
  grid_df <- expand.grid(X_LLC = x_coords, Y_LLC = y_coords)

  # --- 8. HANDLE OUTPUT TYPE ---
  out_obj <- as_inspire_grid(
    coords = grid_df,
    cellsize = cellsize_m,
    crs = grid_crs,
    output_type = output_type,
    point_type = point_type,
    clipping_target = clipping_target
  )

  # --- 9. ADD ID ---
  if (nrow(out_obj) > 0 && id_format != "none") {
    # Use C++ kernel for ID generation
    nzeros <- .tz_count(cellsize_m)
    div <- 10^nzeros
    size_lbl <- if (cellsize_m >= 1000) paste0(cellsize_m / 1000, "km") else paste0(cellsize_m, "m")

    ids <- generate_ids_rcpp(
      x_llc = out_obj$X_LLC,
      y_llc = out_obj$Y_LLC,
      cellsize = as.double(cellsize_m),
      epsg = as.integer(grid_crs$epsg %||% 3035),
      size_lbl = size_lbl,
      divider = as.double(div),
      axis_order = axis_order,
      id_format = id_format
    )

    if (id_format == "long") {
      out_obj$GRD_ID <- ids$id_long
    } else if (id_format == "short") {
      out_obj$GRD_ID <- ids$id_short
    } else if (id_format == "both") {
      out_obj$GRD_ID_LONG <- ids$id_long
      out_obj$GRD_ID_SHORT <- ids$id_short
    }
  }

  # --- 10. CLEAN UP COLUMNS & REORDER (USING HELPER) ---
  out_obj <- clean_and_order_grid(
    out_obj,
    output_type = output_type,
    point_type = point_type,
    include_llc = include_llc
  )

  return(out_obj)
}
