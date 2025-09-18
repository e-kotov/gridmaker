as_grid <- function(
    coords,
    cellsize,
    crs,
    output_type = "sf_polygons",
    point_type = "centroid",
    clipping_target = NULL
) {
  out_obj <- switch(
    output_type,
    sf_polygons = as_grid_polygons(
      coords,
      cellsize = cellsize,
      crs = crs,
      clipping_target = clipping_target
    ),
    sf_points = as_grid_points(
      coords,
      cellsize = cellsize,
      crs = crs,
      point_type = point_type
    ),
    dataframe = as_grid_coordinates(coords, cellsize = cellsize)
  )

  if (!is.null(clipping_target) && !identical(output_type, "sf_polygons")) {
    # For filtering, we always use centroids as the representative point
    points_for_filter <- sf::st_as_sf(
      data.frame(
        x = coords$X_LLC + (cellsize / 2),
        y = coords$Y_LLC + (cellsize / 2)
      ),
      coords = c("x", "y"),
      crs = crs
    )
    keep_indices <- sf::st_intersects(
      points_for_filter,
      clipping_target,
      sparse = FALSE
    )
    out_obj <- out_obj[keep_indices[, 1], ]
  }

  out_obj
}



as_grid_polygons <- function(coords, cellsize, crs, clipping_target = NULL) {
  n_polygons <- nrow(coords)
  x_llc_rep <- rep(coords$X_LLC, each = 5)
  y_llc_rep <- rep(coords$Y_LLC, each = 5)
  x_coords_poly <- x_llc_rep + c(0, cellsize, cellsize, 0, 0)
  y_coords_poly <- y_llc_rep + c(0, 0, cellsize, cellsize, 0)

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
  grid_sf <- sf::st_sf(geometry = grid_geoms, crs = crs)
  grid_sf$X_LLC <- coords$X_LLC
  grid_sf$Y_LLC <- coords$Y_LLC

  if (!is.null(clipping_target)) {
    intersects_list <- sf::st_intersects(grid_sf, clipping_target)
    keep_indices <- lengths(intersects_list) > 0
    grid_sf <- grid_sf[keep_indices, ]
  }

  grid_sf
}


as_grid_points <- function(
    coords,
    cellsize,
    crs,
    point_type = "centroid"
) {
  coords_to_use <- if (point_type == "centroid") {
    list(
      x = coords$X_LLC + (cellsize / 2),
      y = coords$Y_LLC + (cellsize / 2),
      names = c("X_centroid", "Y_centroid")
    )
  } else {
    # llc
    list(x = coords$X_LLC, y = coords$Y_LLC, names = c("X_LLC", "Y_LLC"))
  }
  points_df <- data.frame(x = coords_to_use$x, y = coords_to_use$y)
  out_obj <- sf::st_as_sf(points_df, coords = c("x", "y"), crs = crs)
  # Re-attach all original attributes for consistency
  cbind(out_obj, coords)
}


as_grid_coordinates <- function(grid, cellsize) {
  coords$X_centroid <- coords$X_LLC + (cellsize / 2)
  coords$Y_centroid <- coords$Y_LLC + (cellsize / 2)
  coords
}
