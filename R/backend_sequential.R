# This is the internal, non-exported workhorse function for creating a grid.
# It is called either directly for sequential processing or by the parallel
# handlers for each tile.
create_grid_internal <- function(
  grid_extent,
  cellsize_m,
  crs = NULL,
  output_type = c("sf_polygons", "sf_points", "dataframe"),
  clip_to_input = FALSE,
  use_convex_hull = FALSE,
  buffer_m = 0,
  id_format = c("both", "long", "short", "none"),
  include_llc = TRUE,
  point_type = c("centroid", "llc")
) {
  # --- 1. PRE-CHECKS AND HELPERS ---
  output_type <- match.arg(output_type)
  id_format <- match.arg(id_format)
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

  tz_count <- function(x) {
    n <- 0L
    x <- as.integer(x)
    while (x %% 10L == 0L && x != 0) {
      n <- n + 1L
      x <- x %/% 10L
    }
    n
  }
  make_ids <- function(x_llc, y_llc, cs, epsg = 3035) {
    nzeros <- tz_count(cs)
    div <- as.integer(10^nzeros)
    size_lbl <- if (cs >= 1000) paste0(cs / 1000, "km") else paste0(cs, "m")
    id_long <- sprintf("CRS%sRES%smN%.0fE%.0f", epsg, cs, y_llc, x_llc)
    id_short <- sprintf("%sN%.0fE%.0f", size_lbl, y_llc / div, x_llc / div)
    list(long = id_long, short = id_short)
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

  # Determine the grid CRS
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

  # --- 5. GENERATE GRID POINTS ---
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

  # --- 6. HANDLE CLIPPING TARGET PREPARATION ---
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

  # --- 7. HANDLE OUTPUT TYPE ---
  out_obj <- as_grid(
    grid_df,
    cellsize = cellsize_m,
    crs = grid_crs,
    output_type = output_type,
    point_type = point_type,
    clipping_target = clipping_target
  )

  # --- 8. ADD ID & CLEAN UP COLUMNS ---
  if (nrow(out_obj) == 0) {
    return(out_obj)
  }

  if (id_format != "none") {
    ids <- make_ids(
      out_obj$X_LLC,
      out_obj$Y_LLC,
      cellsize_m,
      epsg = grid_crs$epsg %||% 3035
    )
    if (id_format == "long") {
      out_obj$GRD_ID <- ids$long
    } else if (id_format == "short") {
      out_obj$GRD_ID <- ids$short
    } else if (id_format == "both") {
      out_obj$GRD_ID_LONG <- ids$long
      out_obj$GRD_ID_SHORT <- ids$short
    }
  }

  if (!include_llc && !"geometry" %in% names(out_obj)) {
    out_obj$X_LLC <- NULL
    out_obj$Y_LLC <- NULL
  } else if (!include_llc && "geometry" %in% names(out_obj)) {
    # For sf objects, only remove if not the primary coordinate columns
    if (output_type == "sf_points" && point_type == "llc") {
      # Don't remove LLC columns as they are the source of the geometry
    } else {
      out_obj$X_LLC <- NULL
      out_obj$Y_LLC <- NULL
    }
  }

  if (output_type == "dataframe") {
    first_cols <- c("X_centroid", "Y_centroid")
    other_cols <- setdiff(names(out_obj), first_cols)
    out_obj <- out_obj[, c(first_cols, other_cols)]
  }

  # Ensure geometry column is last for sf objects
  if (inherits(out_obj, "sf")) {
    geom_col_name <- attr(out_obj, "sf_column")
    if (!is.null(geom_col_name) && geom_col_name %in% names(out_obj)) {
      other_cols <- setdiff(names(out_obj), geom_col_name)
      out_obj <- out_obj[, c(other_cols, geom_col_name)]
    }
  }

  return(out_obj)
}

