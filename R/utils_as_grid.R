as_inspire_grid <- function(
  coords,
  cellsize,
  crs,
  output_type = "sf_polygons",
  point_type = "centroid",
  clipping_target = NULL
) {
  if (!is.null(clipping_target)) {
    n_polygons <- nrow(coords)
    if (n_polygons > 0) {
      # Pre-compute vertex coordinates
      ids <- rep.int(seq_len(n_polygons), rep.int(5L, n_polygons))
      x_llc_rep <- rep(coords$X_LLC, each = 5L)
      y_llc_rep <- rep(coords$Y_LLC, each = 5L)

      # Use list + class assignment (faster than data.frame() constructor)
      df_vertices <- list(
        id = ids,
        x = x_llc_rep + c(0, cellsize, cellsize, 0, 0),
        y = y_llc_rep + c(0, 0, cellsize, cellsize, 0)
      )
      class(df_vertices) <- "data.frame"
      attr(df_vertices, "row.names") <- .set_row_names(length(ids))

      temp_sf_obj <- sfheaders::sf_polygon(
        obj = df_vertices,
        x = "x",
        y = "y",
        polygon_id = "id"
      )
      temp_grid_sf <- sf::st_set_crs(temp_sf_obj, crs)

      intersects_list <- sf::st_intersects(temp_grid_sf, clipping_target)
      keep_indices <- lengths(intersects_list) > 0
      coords <- coords[keep_indices, , drop = FALSE]
    }
  }

  out_obj <- switch(
    output_type,
    sf_polygons = as_inspire_grid_polygons(
      coords,
      cellsize = cellsize,
      crs = crs
    ),
    sf_points = as_inspire_grid_points(
      coords,
      cellsize = cellsize,
      crs = crs,
      point_type = point_type
    ),
    dataframe = as_inspire_grid_coordinates(coords, cellsize = cellsize)
  )

  out_obj
}

as_inspire_grid_polygons <- function(coords, cellsize, crs) {
  n_polygons <- nrow(coords)

  if (n_polygons == 0) {
    empty_sf <- sf::st_sf(
      X_LLC = numeric(0),
      Y_LLC = numeric(0),
      geometry = sf::st_sfc(crs = crs)
    )
    for (col_name in names(coords)) {
      if (!col_name %in% names(empty_sf)) {
        empty_sf[[col_name]] <- vector(
          mode = typeof(coords[[col_name]]),
          length = 0
        )
      }
    }
    return(empty_sf)
  }

  # Pre-compute vertex coordinates
  ids <- rep.int(seq_len(n_polygons), rep.int(5L, n_polygons))
  x_llc_rep <- rep(coords$X_LLC, each = 5L)
  y_llc_rep <- rep(coords$Y_LLC, each = 5L)

  # Use list + class assignment (faster than data.frame() constructor)
  df_vertices <- list(
    id = ids,
    x = x_llc_rep + c(0, cellsize, cellsize, 0, 0),
    y = y_llc_rep + c(0, 0, cellsize, cellsize, 0)
  )
  class(df_vertices) <- "data.frame"
  attr(df_vertices, "row.names") <- .set_row_names(length(ids))

  temp_sf_obj <- sfheaders::sf_polygon(
    obj = df_vertices,
    x = "x",
    y = "y",
    polygon_id = "id"
  )

  grid_geoms_sfc <- sf::st_geometry(temp_sf_obj)
  grid_sf <- sf::st_sf(coords, geometry = grid_geoms_sfc, crs = crs)

  grid_sf
}

as_inspire_grid_points <- function(
  coords,
  cellsize,
  crs,
  point_type = "centroid"
) {
  if (nrow(coords) == 0) {
    return(sf::st_sf(coords, geometry = sf::st_sfc(crs = crs)))
  }

  coords_to_use <- if (point_type == "centroid") {
    list(
      x = coords$X_LLC + (cellsize / 2),
      y = coords$Y_LLC + (cellsize / 2)
    )
  } else {
    list(x = coords$X_LLC, y = coords$Y_LLC)
  }

  points_df <- data.frame(x = coords_to_use$x, y = coords_to_use$y)
  out_obj <- sf::st_as_sf(points_df, coords = c("x", "y"), crs = crs)

  cbind(out_obj, coords)
}

as_inspire_grid_coordinates <- function(coords, cellsize) {
  coords$X_centroid <- coords$X_LLC + (cellsize / 2)
  coords$Y_centroid <- coords$Y_LLC + (cellsize / 2)
  coords
}

#' Helper to clean up LLC columns and reorder based on output type
#' @noRd
clean_and_order_grid <- function(
  grid_obj,
  output_type,
  point_type,
  include_llc
) {
  # 1. Handle LLC removal
  # Logic: Keep LLC if requested OR if they define the point geometry
  has_geom <- inherits(grid_obj, "sf") || "geometry" %in% names(grid_obj)

  if (!include_llc) {
    if (!has_geom) {
      grid_obj$X_LLC <- NULL
      grid_obj$Y_LLC <- NULL
    } else {
      # If it is spatial, only keep if they represent the point geometry
      # (i.e. if point_type is 'llc')
      if (!(output_type == "sf_points" && point_type == "llc")) {
        grid_obj$X_LLC <- NULL
        grid_obj$Y_LLC <- NULL
      }
    }
  }

  # 2. Reorder for Dataframe
  if (output_type == "dataframe") {
    # We prefer IDs and Centroids at the beginning
    prio_cols <- c(
      "id",
      "GRD_ID",
      "GRD_ID_LONG",
      "GRD_ID_SHORT",
      "X_centroid",
      "Y_centroid"
    )
    existing_prio <- intersect(prio_cols, names(grid_obj))
    other_cols <- setdiff(names(grid_obj), existing_prio)
    grid_obj <- grid_obj[, c(existing_prio, other_cols)]
  }

  # 3. Reorder for SF (ensure geometry is last)
  if (inherits(grid_obj, "sf")) {
    geom_col_name <- attr(grid_obj, "sf_column")
    if (!is.null(geom_col_name) && geom_col_name %in% names(grid_obj)) {
      other_cols <- setdiff(names(grid_obj), geom_col_name)
      grid_obj <- grid_obj[, c(other_cols, geom_col_name)]
    }
  }

  grid_obj
}
