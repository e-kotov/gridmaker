#' Create a high-performance, standard-compliant spatial grid
#'
#' This function generates a regular spatial grid aligned to the CRS origin.
#' It combines high performance for large areas (using `sfheaders`) with a
#' flexible and robust set of features for input handling and output formatting,
#' including INSPIRE-compliant grid IDs.
#'
#' @param grid_extent A spatial object to define the grid's extent. Can be an
#'   `sf` or `sfc` object, a 2x2 `bbox` matrix, or a numeric vector of
#'   `c(xmin, ymin, xmax, ymax)`.
#' @param cellsize_m A single integer representing the grid cell size in metres
#'   (e.g., 1000 for a 1 km grid).
#' @param crs The coordinate reference system (CRS) for the output grid.
#'   Accepts various formats handled by `sf::st_crs()`: an integer or numeric
#'   EPSG code (e.g., `3035`), a string representation like `"epsg:3035"`, or
#'   a `crs` object. If `NULL` (default), the CRS is inherited from
#'   `grid_extent`. If `grid_extent` also lacks a CRS, the function will stop
#'   with an error.
#' @param output_type The class of the output object: `"sf_polygons"` (default) creates
#'   a spatial object with polygon geometries, `"sf_points"` creates an `sf`
#'   object with point geometries, and `"dataframe"` creates a data frame with
#'   grid cell centroid coordinates (`X_centroid`, `Y_centroid`).
#' @param clip_to_input A logical value. If `TRUE`, the grid is filtered to
#'   include only cells that intersect the `grid_extent`. This does not cut
#'   cell geometries.
#' @param use_convex_hull A logical value. If `TRUE` and `clip_to_input` is
#'   `TRUE`, the grid is clipped to the convex hull of the input geometry,
#'   which can be faster and simpler than using a complex polygon.
#' @param buffer_m A numeric value. If `clip_to_input` is `TRUE`, this specifies
#'   a buffer distance in metres to apply to the `grid_extent` geometry before
#'   clipping. Defaults to `0` (no buffer).
#' @param id_format A character string specifying which grid cell IDs to generate.
#'   Options are `"both"` (default), `"long"`, `"short"`, or `"none"`.
#' @param include_llc A logical value. If `TRUE` (default), columns for the
#'   lower-left corner coordinates (`X_LLC`, `Y_LLC`) of each cell are included
#'   in the output.
#' @param point_type A character string, used only when `output_type = "sf_points"`.
#'   Determines the location of the points: `"centroid"` (default) for the center
#'   of the cell, or `"llc"` for the lower-left corner.
#'
#' @return An `sf` object (with polygon or point geometries) or a `data.frame`
#'   representing the grid, with optional columns for coordinates and
#'   INSPIRE-compliant grid IDs.
#' @export
#' @examples
#' library(sf)
#' # Load the sample data from the sf package
#' nc_raw <- st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
#'
#' # Define target projected CRS and cell size
#' target_crs <- 5070 # NAD83 / Conus Albers
#' cellsize_m <- 10000 # 10 km
#'
#' # Project the data
#' nc <- st_transform(nc_raw, target_crs)
#'
#' # Create a grid covering the data
#' nc_grid <- create_grid(
#'   grid_extent = nc,
#'   cellsize_m = cellsize_m,
#'   output_type = "sf_polygons",
#'   clip_to_input = TRUE
#' )
#'
#' head(nc_grid, 3)
create_grid <- function(
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

  `%||%` <- function(a, b) if (!is.null(a)) a else b
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
  if (output_type == "sf_polygons") {
    # --- 7a. SF POLYGON OUTPUT ---
    n_polygons <- nrow(grid_df)
    x_llc_rep <- rep(grid_df$X_LLC, each = 5)
    y_llc_rep <- rep(grid_df$Y_LLC, each = 5)
    x_coords_poly <- x_llc_rep + c(0, cellsize_m, cellsize_m, 0, 0)
    y_coords_poly <- y_llc_rep + c(0, 0, cellsize_m, cellsize_m, 0)

    df_vertices <- data.frame(
      id = rep(seq_len(n_polygons), each = 5),
      x = x_coords_poly,
      y = y_coords_poly
    )
    grid_geoms <- sfheaders::sf_polygon(
      obj = df_vertices,
      x = "x",
      y = "y",
      polygon_id = "id"
    )
    grid_sf <- sf::st_sf(geometry = grid_geoms, crs = grid_crs)
    grid_sf$X_LLC <- grid_df$X_LLC
    grid_sf$Y_LLC <- grid_df$Y_LLC

    if (!is.null(clipping_target)) {
      intersects_list <- sf::st_intersects(grid_sf, clipping_target)
      keep_indices <- lengths(intersects_list) > 0
      grid_sf <- grid_sf[keep_indices, ]
    }
    out_obj <- grid_sf
  } else {
    # --- 7b. SF POINTS OR DATAFRAME OUTPUT ---
    if (output_type == "sf_points") {
      coords_to_use <- if (point_type == "centroid") {
        list(
          x = grid_df$X_LLC + (cellsize_m / 2),
          y = grid_df$Y_LLC + (cellsize_m / 2),
          names = c("X_centroid", "Y_centroid")
        )
      } else {
        # llc
        list(x = grid_df$X_LLC, y = grid_df$Y_LLC, names = c("X_LLC", "Y_LLC"))
      }
      points_df <- data.frame(x = coords_to_use$x, y = coords_to_use$y)
      out_obj <- sf::st_as_sf(points_df, coords = c("x", "y"), crs = grid_crs)
      # Re-attach all original attributes for consistency
      out_obj <- cbind(out_obj, grid_df)
    } else {
      # dataframe
      grid_df$X_centroid <- grid_df$X_LLC + (cellsize_m / 2)
      grid_df$Y_centroid <- grid_df$Y_LLC + (cellsize_m / 2)
      out_obj <- grid_df
    }

    if (!is.null(clipping_target)) {
      # For filtering, we always use centroids as the representative point
      points_for_filter <- sf::st_as_sf(
        data.frame(
          x = grid_df$X_LLC + (cellsize_m / 2),
          y = grid_df$Y_LLC + (cellsize_m / 2)
        ),
        coords = c("x", "y"),
        crs = grid_crs
      )
      keep_indices <- sf::st_intersects(
        points_for_filter,
        clipping_target,
        sparse = FALSE
      )
      out_obj <- out_obj[keep_indices[, 1], ]
    }
  }

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
