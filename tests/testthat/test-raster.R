test_that("inspire_grid_from_extent generates SpatRaster correctly", {
  skip_if_not_installed("terra")

  # Setup simple extent (4 cells: 2x2)
  # 0,0 to 2000,2000 with 1000m resolution
  bbox <- sf::st_bbox(
    c(xmin = 0, ymin = 0, xmax = 2000, ymax = 2000),
    crs = 3035
  )

  # 1. Test In-Memory Generation
  r <- inspire_grid_from_extent(
    grid_extent = bbox,
    cellsize_m = 1000,
    output_type = "spatraster",
    id_format = "short",
    quiet = TRUE
  )

  expect_s4_class(r, "SpatRaster")
  expect_equal(terra::ncell(r), 4)
  expect_equal(terra::res(r)[1], 1000)

  # Check Factor Levels (IDs)
  cats <- terra::cats(r)[[1]]
  expect_true("GRD_ID" %in% names(cats))
  # Should contain 1kmN0E0 (bottom-left)
  expect_true("1kmN0E0" %in% cats$GRD_ID)
})

test_that("inspire_grid_from_extent generates SpatRaster with different id_format options", {
  skip_if_not_installed("terra")

  bbox <- sf::st_bbox(
    c(xmin = 0, ymin = 0, xmax = 2000, ymax = 2000),
    crs = 3035
  )

  # Test "short" format
  r_short <- inspire_grid_from_extent(
    grid_extent = bbox,
    cellsize_m = 1000,
    output_type = "spatraster",
    id_format = "short",
    quiet = TRUE
  )
  cats_short <- terra::cats(r_short)[[1]]
  expect_true("GRD_ID" %in% names(cats_short))
  expect_true("1kmN0E0" %in% cats_short$GRD_ID)

  # Test "long" format
  r_long <- inspire_grid_from_extent(
    grid_extent = bbox,
    cellsize_m = 1000,
    output_type = "spatraster",
    id_format = "long",
    quiet = TRUE
  )
  cats_long <- terra::cats(r_long)[[1]]
  expect_true("GRD_ID" %in% names(cats_long))
  expect_true(any(grepl("CRS3035RES1000mN0E0", cats_long$GRD_ID)))

  # Test "both" format
  r_both <- inspire_grid_from_extent(
    grid_extent = bbox,
    cellsize_m = 1000,
    output_type = "spatraster",
    id_format = "both",
    quiet = TRUE
  )
  cats_both <- terra::cats(r_both)[[1]]
  expect_true("GRD_ID_SHORT" %in% names(cats_both))
  expect_true("GRD_ID_LONG" %in% names(cats_both))
  expect_true("1kmN0E0" %in% cats_both$GRD_ID_SHORT)
  expect_true(any(grepl("CRS3035RES1000mN0E0", cats_both$GRD_ID_LONG)))

  # Test "none" format
  r_none <- inspire_grid_from_extent(
    grid_extent = bbox,
    cellsize_m = 1000,
    output_type = "spatraster",
    id_format = "none",
    quiet = TRUE
  )
  expect_equal(names(r_none), "cell_index")
})

test_that("inspire_grid_from_extent with spatraster handles clipping correctly", {
  skip_if_not_installed("terra")

  # Define a bbox covering 4 cells (2x2 grid)
  bbox <- sf::st_bbox(
    c(xmin = 0, ymin = 0, xmax = 2000, ymax = 2000),
    crs = 3035
  )

  # Create a polygon covering only the bottom-left and bottom-right cells
  poly <- sf::st_as_sfc(
    sf::st_bbox(c(xmin = 0, ymin = 0, xmax = 2000, ymax = 900), crs = 3035)
  )

  # Generate raster without clipping first
  r_full <- inspire_grid_from_extent(
    grid_extent = bbox, # Use full bbox
    cellsize_m = 1000,
    output_type = "spatraster",
    clip_to_input = FALSE,
    quiet = TRUE
  )

  # Should have 4 cells (full extent)
  expect_equal(terra::ncell(r_full), 4)

  # Now test clipping by providing a polygon with clip_to_input=TRUE
  r_clipped2 <- inspire_grid_from_extent(
    grid_extent = poly, # Polygon that's only half the height
    cellsize_m = 1000,
    output_type = "spatraster",
    clip_to_input = TRUE,
    id_format = "short",
    quiet = TRUE
  )

  # This creates 2 cells (1000x2000 area = 2 cells of 1000x1000)
  expect_equal(terra::ncell(r_clipped2), 2)
})

test_that("inspire_grid_from_extent writes Raster to disk", {
  skip_if_not_installed("terra")

  tmp_tif <- tempfile(fileext = ".tif")
  on.exit(unlink(tmp_tif), add = TRUE)

  bbox <- sf::st_bbox(
    c(xmin = 0, ymin = 0, xmax = 2000, ymax = 2000),
    crs = 3035
  )

  res <- inspire_grid_from_extent(
    grid_extent = bbox,
    cellsize_m = 1000,
    output_type = "spatraster",
    id_format = "short",
    dsn = tmp_tif,
    quiet = TRUE
  )

  expect_true(file.exists(tmp_tif))
  expect_equal(res, tmp_tif)

  # Read back and verify
  r_in <- terra::rast(tmp_tif)
  expect_equal(terra::res(r_in)[1], 1000)
  expect_equal(terra::ncell(r_in), 4)
})

test_that("spatraster output respects axis_order for short IDs", {
  skip_if_not_installed("terra")

  bbox <- sf::st_bbox(
    c(xmin = 0, ymin = 0, xmax = 2000, ymax = 2000),
    crs = 3035
  )

  # Test NE order (default)
  r_ne <- inspire_grid_from_extent(
    grid_extent = bbox,
    cellsize_m = 1000,
    output_type = "spatraster",
    id_format = "short",
    axis_order = "NE",
    quiet = TRUE
  )
  cats_ne <- terra::cats(r_ne)[[1]]
  expect_true("1kmN0E0" %in% cats_ne$GRD_ID)

  # Test EN order
  r_en <- inspire_grid_from_extent(
    grid_extent = bbox,
    cellsize_m = 1000,
    output_type = "spatraster",
    id_format = "short",
    axis_order = "EN",
    quiet = TRUE
  )
  cats_en <- terra::cats(r_en)[[1]]
  expect_true("1kmE0N0" %in% cats_en$GRD_ID)
})

test_that("spatraster output is silent with quiet = TRUE", {
  skip_if_not_installed("terra")

  bbox <- sf::st_bbox(
    c(xmin = 0, ymin = 0, xmax = 2000, ymax = 2000),
    crs = 3035
  )

  # With quiet = TRUE, no message should be shown
  expect_silent(
    inspire_grid_from_extent(
      grid_extent = bbox,
      cellsize_m = 1000,
      output_type = "spatraster",
      parallel = "auto",
      quiet = TRUE
    )
  )
})

test_that("spatraster output has correct CRS", {
  skip_if_not_installed("terra")

  bbox <- sf::st_bbox(
    c(xmin = 0, ymin = 0, xmax = 2000, ymax = 2000),
    crs = 3035
  )

  r <- inspire_grid_from_extent(
    grid_extent = bbox,
    cellsize_m = 1000,
    output_type = "spatraster",
    id_format = "none",
    quiet = TRUE
  )

  # Check that CRS is set correctly
  expect_equal(terra::crs(r, describe = TRUE)$code, "3035")
})

test_that("spatraster works with larger grid from nc data", {
  skip_if_not_installed("terra")

  # Use the test nc data
  r <- inspire_grid_from_extent(
    grid_extent = nc,
    cellsize_m = CELLSIZE,
    crs = TARGET_CRS,
    output_type = "spatraster",
    id_format = "short",
    clip_to_input = FALSE,
    quiet = TRUE
  )

  expect_s4_class(r, "SpatRaster")
  expect_gt(terra::ncell(r), 0)
  expect_equal(terra::res(r)[1], CELLSIZE)

  # Check that it has the correct CRS
  expect_equal(
    terra::crs(r, describe = TRUE)$code,
    as.character(TARGET_CRS)
  )
})

test_that("inspire_grid_from_extent with spatraster supports 'use_convex_hull' and 'buffer_m'", {
  skip_if_not_installed("terra")

  # 1. Create a non-convex shape (L-shape)
  # A 2000x2000 box with the top-right 1000x1000 quadrant missing
  p1 <- rbind(
    c(0, 0),
    c(2000, 0),
    c(2000, 1000),
    c(1000, 1000),
    c(1000, 2000),
    c(0, 2000),
    c(0, 0)
  )
  poly <- sf::st_polygon(list(p1))
  poly_sfc <- sf::st_sfc(poly, crs = 3035)

  # 2. Baseline: Standard clipping
  # Grid cell size 500. Total area 2000x2000 = 16 cells.
  # The L-shape occupies 3 quadrants (12 cells).
  r_base <- inspire_grid_from_extent(
    grid_extent = poly_sfc,
    cellsize_m = 500,
    output_type = "spatraster",
    clip_to_input = TRUE,
    quiet = TRUE
  )
  # Count non-NA cells
  count_base <- sum(!is.na(terra::values(r_base)))

  # 3. Test Convex Hull
  # The convex hull of the L-shape is the full 2000x2000 square.
  # Should have more cells than baseline (filling the missing quadrant).
  r_hull <- inspire_grid_from_extent(
    grid_extent = poly_sfc,
    cellsize_m = 500,
    output_type = "spatraster",
    clip_to_input = TRUE,
    use_convex_hull = TRUE,
    quiet = TRUE
  )
  count_hull <- sum(!is.na(terra::values(r_hull)))

  expect_gt(count_hull, count_base)

  # 4. Test Buffer
  # Buffer the L-shape. Should result in more cells than baseline.
  r_buff <- inspire_grid_from_extent(
    grid_extent = poly_sfc,
    cellsize_m = 500,
    output_type = "spatraster",
    clip_to_input = TRUE,
    buffer_m = 200,
    quiet = TRUE
  )
  count_buff <- sum(!is.na(terra::values(r_buff)))

  expect_gt(count_buff, count_base)
})
