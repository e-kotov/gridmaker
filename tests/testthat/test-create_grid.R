test_that("Function handles various input 'grid_extent' types", {
  # 1. sf object
  grid_sf <- create_grid(nc, CELLSIZE, crs = TARGET_CRS)
  expect_s3_class(grid_sf, "sf")
  expect_gt(nrow(grid_sf), 0)

  # 2. sfc object
  grid_sfc <- create_grid(st_geometry(nc), CELLSIZE, crs = TARGET_CRS)
  expect_equal(nrow(grid_sf), nrow(grid_sfc))

  # 3. bbox object
  grid_bbox <- create_grid(st_bbox(nc), CELLSIZE, crs = TARGET_CRS)
  expect_equal(nrow(grid_sf), nrow(grid_bbox))

  # 4. Numeric vector
  nc_bbox_num <- as.numeric(st_bbox(nc))
  grid_num <- create_grid(nc_bbox_num, CELLSIZE, crs = TARGET_CRS)
  expect_equal(nrow(grid_sf), nrow(grid_num))

  # 5. Matrix
  nc_bbox_mat <- matrix(nc_bbox_num, nrow = 2)
  grid_mat <- create_grid(nc_bbox_mat, CELLSIZE, crs = TARGET_CRS)
  expect_equal(nrow(grid_sf), nrow(grid_mat))
})

test_that("Function handles different 'output_type' options", {
  # Polygons (default)
  grid_poly <- create_grid(
    nc,
    CELLSIZE,
    crs = TARGET_CRS,
    output_type = "sf_polygons"
  )
  expect_s3_class(grid_poly, "sf")
  expect_true(any(class(st_geometry(grid_poly)) == "sfc_POLYGON"))

  # Points
  grid_pts <- create_grid(
    nc,
    CELLSIZE,
    crs = TARGET_CRS,
    output_type = "sf_points"
  )
  expect_s3_class(grid_pts, "sf")
  expect_true(any(class(st_geometry(grid_pts)) == "sfc_POINT"))

  # Data frame
  grid_df <- create_grid(
    nc,
    CELLSIZE,
    crs = TARGET_CRS,
    output_type = "dataframe"
  )
  expect_s3_class(grid_df, "data.frame")
  expect_true(all(c("X_centroid", "Y_centroid") %in% names(grid_df)))

  # Check consistency in number of cells
  expect_equal(nrow(grid_poly), nrow(grid_pts))
  expect_equal(nrow(grid_poly), nrow(grid_df))
})

test_that("Clipping logic works as expected", {
  # Baseline (no clipping)
  grid_full <- create_grid(
    nc,
    CELLSIZE,
    crs = TARGET_CRS,
    clip_to_input = FALSE
  )

  # Standard clipping
  grid_clipped <- create_grid(
    nc,
    CELLSIZE,
    crs = TARGET_CRS,
    clip_to_input = TRUE
  )
  expect_lt(nrow(grid_clipped), nrow(grid_full))

  # Convex hull clipping
  grid_chull <- create_grid(
    nc,
    CELLSIZE,
    crs = TARGET_CRS,
    clip_to_input = TRUE,
    use_convex_hull = TRUE
  )
  expect_lt(nrow(grid_chull), nrow(grid_full))
  # Convex hull area is >= original area, so cell count is >=
  expect_gte(nrow(grid_chull), nrow(grid_clipped))

  # Clipping with buffer
  grid_buffered <- create_grid(
    nc,
    CELLSIZE,
    crs = TARGET_CRS,
    clip_to_input = TRUE,
    buffer_m = 50000
  )
  expect_gt(nrow(grid_buffered), nrow(grid_clipped))
  expect_lt(nrow(grid_buffered), nrow(grid_full))
})

test_that("Generated IDs are correct", {
  # Use a very simple extent for predictable IDs
  # Lower-left corner: (1,000,000, 1,000,000)
  # Cell size: 100,000 (100km)
  # This guarantees a single cell with LLC at (1000000, 1000000)
  simple_extent <- c(1000001, 1000001, 1000002, 1000002)
  cs <- 100000

  # Expected IDs for X=1000000, Y=1000000
  expected_long <- sprintf("CRS%dRES%dmN1000000E1000000", TARGET_CRS, cs)
  expected_short <- sprintf("%skmN10E10", cs / 1000)

  # Test "both"
  grid_both <- create_grid(
    simple_extent,
    cs,
    crs = TARGET_CRS,
    id_format = "both"
  )
  expect_equal(grid_both$GRD_ID_LONG, expected_long)
  expect_equal(grid_both$GRD_ID_SHORT, expected_short)

  # Test "long"
  grid_long <- create_grid(
    simple_extent,
    cs,
    crs = TARGET_CRS,
    id_format = "long"
  )
  expect_equal(grid_long$GRD_ID, expected_long)
  expect_false("GRD_ID_SHORT" %in% names(grid_long))

  # Test "short"
  grid_short <- create_grid(
    simple_extent,
    cs,
    crs = TARGET_CRS,
    id_format = "short"
  )
  expect_equal(grid_short$GRD_ID, expected_short)
  expect_false("GRD_ID_LONG" %in% names(grid_short))

  # Test "none"
  grid_none <- create_grid(
    simple_extent,
    cs,
    crs = TARGET_CRS,
    id_format = "none"
  )
  expect_false(any(grepl("GRD_ID", names(grid_none))))
})

test_that("Other arguments function correctly", {
  # Test include_llc = FALSE
  grid_no_llc <- create_grid(
    nc,
    CELLSIZE,
    crs = TARGET_CRS,
    include_llc = FALSE
  )
  expect_false("X_LLC" %in% names(grid_no_llc))
  expect_false("Y_LLC" %in% names(grid_no_llc))

  # Test point_type = "llc"
  grid_pts_llc <- create_grid(
    nc,
    CELLSIZE,
    crs = TARGET_CRS,
    output_type = "sf_points",
    point_type = "llc"
  )
  coords <- st_coordinates(grid_pts_llc)
  # The point geometry should match the LLC columns
  expect_equal(coords[, "X"], grid_pts_llc$X_LLC)
  expect_equal(coords[, "Y"], grid_pts_llc$Y_LLC)
})


test_that("Function handles errors and edge cases", {
  # Invalid cell size
  expect_error(
    create_grid(nc, 0, crs = TARGET_CRS),
    "cellsize_m must be a positive integer"
  )
  expect_error(
    create_grid(nc, -100, crs = TARGET_CRS),
    "cellsize_m must be a positive integer"
  )

  # Invalid CRS (geographic)
  expect_error(
    create_grid(nc, CELLSIZE, crs = 4326),
    "must be a projected system"
  )

  # Empty input geometry
  empty_sf <- nc[0, ]
  expect_error(
    create_grid(empty_sf, CELLSIZE, crs = TARGET_CRS),
    "Input geometry is empty"
  )

  # Check that a tiny geometry still produces a grid
  point_geom <- st_sfc(st_point(c(15e5, 15e5)), crs = TARGET_CRS)
  grid_from_point <- create_grid(point_geom, CELLSIZE, crs = TARGET_CRS)
  expect_equal(nrow(grid_from_point), 1)
})

test_that("Geometry column is always last in sf output", {
  # For sf_polygons
  grid_poly <- create_grid(
    nc,
    CELLSIZE,
    crs = TARGET_CRS,
    output_type = "sf_polygons"
  )
  col_names_poly <- names(grid_poly)
  geom_col_poly <- attr(grid_poly, "sf_column")
  expect_equal(tail(col_names_poly, 1), geom_col_poly)

  # For sf_points
  grid_pts <- create_grid(
    nc,
    CELLSIZE,
    crs = TARGET_CRS,
    output_type = "sf_points"
  )
  col_names_pts <- names(grid_pts)
  geom_col_pts <- attr(grid_pts, "sf_column")
  expect_equal(tail(col_names_pts, 1), geom_col_pts)
})

test_that("CRS is handled in various formats", {
  # EPSG code as integer
  grid_int <- create_grid(nc, CELLSIZE, crs = 3035)
  expect_s3_class(grid_int, "sf")
  expect_true(sf::st_crs(grid_int) == sf::st_crs(3035))

  # EPSG code as numeric
  grid_num <- create_grid(nc, CELLSIZE, crs = 3035)
  expect_s3_class(grid_num, "sf")
  expect_true(sf::st_crs(grid_num) == sf::st_crs(3035))

  # "epsg:<code>" string
  grid_epsg_str <- create_grid(nc, CELLSIZE, crs = "epsg:3035")
  expect_s3_class(grid_epsg_str, "sf")
  expect_true(sf::st_crs(grid_epsg_str) == sf::st_crs(3035))

  # sf_crs object
  crs_obj <- sf::st_crs(3035)
  grid_obj <- create_grid(nc, CELLSIZE, crs = crs_obj)
  expect_s3_class(grid_obj, "sf")
  expect_true(sf::st_crs(grid_obj) == sf::st_crs(3035))

  # Invalid: "<code>" string - this should fail as it's ambiguous
  # The error comes from sf::st_crs()
  expect_error(
    create_grid(nc, CELLSIZE, crs = "3035")
  )
})
