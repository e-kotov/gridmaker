test_that("Vector and Raster outputs are consistent (Spatial Match)", {
  skip_if_not_installed("terra")
  skip_if_not_installed("sf")

  # 1. Generate Vector Grid (Points)
  vec <- create_grid(
    grid_extent = nc,
    cellsize_m = CELLSIZE,
    crs = TARGET_CRS,
    output_type = "sf_points",
    id_format = "short",
    quiet = TRUE
  )

  # 2. Generate Raster Grid
  rst <- create_grid(
    grid_extent = nc,
    cellsize_m = CELLSIZE,
    crs = TARGET_CRS,
    output_type = "spatraster",
    id_format = "short",
    quiet = TRUE
  )

  # 3. Extract Raster Values at Vector Coordinates
  coords <- sf::st_coordinates(vec)
  extracted <- terra::extract(rst, coords)

  # Get the value column (name matches layer name "GRD_ID")
  val <- extracted[["GRD_ID"]]

  # 4. Normalize to Strings
  # If terra returns numeric indices, look them up in the RAT.
  if (is.numeric(val)) {
    rat <- terra::cats(rst)[[1]]
    # RAT usually has 'id' as first column and 'GRD_ID' as label
    # match val against rat$id
    val <- rat$GRD_ID[match(val, rat$id)]
  }

  # 5. Compare
  expect_equal(vec$GRD_ID, as.character(val))
})

test_that("Raster Output supports 'both' ID format in RAT", {
  skip_if_not_installed("terra")

  bbox <- sf::st_bbox(
    c(xmin = 0, ymin = 0, xmax = 2000, ymax = 2000),
    crs = 3035
  )

  r <- create_grid(
    grid_extent = bbox,
    cellsize_m = 1000,
    output_type = "spatraster",
    id_format = "both",
    quiet = TRUE
  )

  rat <- terra::cats(r)[[1]]

  expect_true("GRD_ID_SHORT" %in% names(rat))
  expect_true("GRD_ID_LONG" %in% names(rat))
  expect_true("1kmN0E0" %in% rat$GRD_ID_SHORT)
})
