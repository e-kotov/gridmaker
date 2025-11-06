derive_grid_internal <- function(
  ids,
  point_type = c("llc", "centroid"),
  output_type = c("sf_polygons", "sf_points", "dataframe"),
  quiet = FALSE
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

  grid_df <- inspire_extract(ids, as_sf = FALSE)
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

  out_obj <- as_grid(
    grid_df,
    cellsize = grid_df$cellsize[[1]],
    crs = grid_crs,
    output_type = output_type,
    point_type = point_type
  )

  out_obj$id <- ids
  out_obj
}
