as_grid <- function(
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
      x_llc_rep <- rep(coords$X_LLC, each = 5)
      y_llc_rep <- rep(coords$Y_LLC, each = 5)
      x_coords_poly <- x_llc_rep + c(0, cellsize, cellsize, 0, 0)
      y_coords_poly <- y_llc_rep + c(0, 0, cellsize, cellsize, 0)

      df_vertices <- data.frame(
        id = rep(seq_len(n_polygons), each = 5),
        x = x_coords_poly,
        y = y_coords_poly
      )

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
    sf_polygons = as_grid_polygons(
      coords,
      cellsize = cellsize,
      crs = crs
    ),
    sf_points = as_grid_points(
      coords,
      cellsize = cellsize,
      crs = crs,
      point_type = point_type
    ),
    dataframe = as_grid_coordinates(coords, cellsize = cellsize)
  )

  out_obj
}

as_grid_polygons <- function(coords, cellsize, crs) {
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

  x_llc_rep <- rep(coords$X_LLC, each = 5)
  y_llc_rep <- rep(coords$Y_LLC, each = 5)
  x_coords_poly <- x_llc_rep + c(0, cellsize, cellsize, 0, 0)
  y_coords_poly <- y_llc_rep + c(0, 0, cellsize, cellsize, 0)

  df_vertices <- data.frame(
    id = rep(seq_len(n_polygons), each = 5),
    x = x_coords_poly,
    y = y_coords_poly
  )

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

as_grid_points <- function(
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

as_grid_coordinates <- function(coords, cellsize) {
  coords$X_centroid <- coords$X_LLC + (cellsize / 2)
  coords$Y_centroid <- coords$Y_LLC + (cellsize / 2)
  coords
}
