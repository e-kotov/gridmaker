test_that("Function handles various input 'grid_extent' types", {
  # 1. sf object
  grid_sf <- inspire_grid_from_extent(nc, CELLSIZE, crs = TARGET_CRS)
  expect_s3_class(grid_sf, "sf")
  expect_gt(nrow(grid_sf), 0)

  # 2. sfc object
  grid_sfc <- inspire_grid_from_extent(st_geometry(nc), CELLSIZE, crs = TARGET_CRS)
  expect_equal(nrow(grid_sf), nrow(grid_sfc))

  # 3. bbox object
  grid_bbox <- inspire_grid_from_extent(st_bbox(nc), CELLSIZE, crs = TARGET_CRS)
  expect_equal(nrow(grid_sf), nrow(grid_bbox))

  # 4. Numeric vector
  nc_bbox_num <- as.numeric(st_bbox(nc))
  grid_num <- inspire_grid_from_extent(nc_bbox_num, CELLSIZE, crs = TARGET_CRS)
  expect_equal(nrow(grid_sf), nrow(grid_num))

  # 5. Matrix
  nc_bbox_mat <- matrix(nc_bbox_num, nrow = 2)
  grid_mat <- inspire_grid_from_extent(nc_bbox_mat, CELLSIZE, crs = TARGET_CRS)
  expect_equal(nrow(grid_sf), nrow(grid_mat))
})

test_that("Function handles different 'output_type' options", {
  # Polygons (default)
  grid_poly <- inspire_grid_from_extent(
    nc,
    CELLSIZE,
    crs = TARGET_CRS,
    output_type = "sf_polygons"
  )
  expect_s3_class(grid_poly, "sf")
  expect_true(any(class(st_geometry(grid_poly)) == "sfc_POLYGON"))

  # Points
  grid_pts <- inspire_grid_from_extent(
    nc,
    CELLSIZE,
    crs = TARGET_CRS,
    output_type = "sf_points"
  )
  expect_s3_class(grid_pts, "sf")
  expect_true(any(class(st_geometry(grid_pts)) == "sfc_POINT"))

  # Data frame
  grid_df <- inspire_grid_from_extent(
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
  grid_full <- inspire_grid_from_extent(
    nc,
    CELLSIZE,
    crs = TARGET_CRS,
    clip_to_input = FALSE
  )

  # Standard clipping
  grid_clipped <- inspire_grid_from_extent(
    nc,
    CELLSIZE,
    crs = TARGET_CRS,
    clip_to_input = TRUE
  )
  expect_lt(nrow(grid_clipped), nrow(grid_full))

  # Convex hull clipping
  grid_chull <- inspire_grid_from_extent(
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
  grid_buffered <- inspire_grid_from_extent(
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
  grid_both <- inspire_grid_from_extent(
    simple_extent,
    cs,
    crs = TARGET_CRS,
    id_format = "both"
  )
  expect_equal(grid_both$GRD_ID_LONG, expected_long)
  expect_equal(grid_both$GRD_ID_SHORT, expected_short)

  # Test "long"
  grid_long <- inspire_grid_from_extent(
    simple_extent,
    cs,
    crs = TARGET_CRS,
    id_format = "long"
  )
  expect_equal(grid_long$GRD_ID, expected_long)
  expect_false("GRD_ID_SHORT" %in% names(grid_long))

  # Test "short"
  grid_short <- inspire_grid_from_extent(
    simple_extent,
    cs,
    crs = TARGET_CRS,
    id_format = "short"
  )
  expect_equal(grid_short$GRD_ID, expected_short)
  expect_false("GRD_ID_LONG" %in% names(grid_short))

  # Test "none"
  grid_none <- inspire_grid_from_extent(
    simple_extent,
    cs,
    crs = TARGET_CRS,
    id_format = "none"
  )
  expect_false(any(grepl("GRD_ID", names(grid_none))))
})

test_that("Other arguments function correctly", {
  # Test include_llc = FALSE
  grid_no_llc <- inspire_grid_from_extent(
    nc,
    CELLSIZE,
    crs = TARGET_CRS,
    include_llc = FALSE
  )
  expect_false("X_LLC" %in% names(grid_no_llc))
  expect_false("Y_LLC" %in% names(grid_no_llc))

  # Test point_type = "llc"
  grid_pts_llc <- inspire_grid_from_extent(
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
    inspire_grid_from_extent(nc, 0, crs = TARGET_CRS),
    "cellsize_m must be a positive integer"
  )
  expect_error(
    inspire_grid_from_extent(nc, -100, crs = TARGET_CRS),
    "cellsize_m must be a positive integer"
  )

  # Invalid CRS (geographic)
  expect_error(
    inspire_grid_from_extent(nc, CELLSIZE, crs = 4326),
    "must be a projected system"
  )

  # Empty input geometry
  empty_sf <- nc[0, ]
  expect_error(
    inspire_grid_from_extent(empty_sf, CELLSIZE, crs = TARGET_CRS),
    "Input geometry is empty"
  )

  # Check that a tiny geometry still produces a grid
  point_geom <- st_sfc(st_point(c(15e5, 15e5)), crs = TARGET_CRS)
  grid_from_point <- inspire_grid_from_extent(point_geom, CELLSIZE, crs = TARGET_CRS)
  expect_equal(nrow(grid_from_point), 1)
})

test_that("Geometry column is always last in sf output", {
  # For sf_polygons
  grid_poly <- inspire_grid_from_extent(
    nc,
    CELLSIZE,
    crs = TARGET_CRS,
    output_type = "sf_polygons"
  )
  col_names_poly <- names(grid_poly)
  geom_col_poly <- attr(grid_poly, "sf_column")
  expect_equal(tail(col_names_poly, 1), geom_col_poly)

  # For sf_points
  grid_pts <- inspire_grid_from_extent(
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
  grid_int <- inspire_grid_from_extent(nc, CELLSIZE, crs = 3035)
  expect_s3_class(grid_int, "sf")
  expect_true(sf::st_crs(grid_int) == sf::st_crs(3035))

  # EPSG code as numeric
  grid_num <- inspire_grid_from_extent(nc, CELLSIZE, crs = 3035)
  expect_s3_class(grid_num, "sf")
  expect_true(sf::st_crs(grid_num) == sf::st_crs(3035))

  # "epsg:<code>" string
  grid_epsg_str <- inspire_grid_from_extent(nc, CELLSIZE, crs = "epsg:3035")
  expect_s3_class(grid_epsg_str, "sf")
  expect_true(sf::st_crs(grid_epsg_str) == sf::st_crs(3035))

  # sf_crs object
  crs_obj <- sf::st_crs(3035)
  grid_obj <- inspire_grid_from_extent(nc, CELLSIZE, crs = crs_obj)
  expect_s3_class(grid_obj, "sf")
  expect_true(sf::st_crs(grid_obj) == sf::st_crs(3035))

  # Invalid: "<code>" string - this should fail as it's ambiguous
  # The error comes from sf::st_crs()
  expect_error(
    inspire_grid_from_extent(nc, CELLSIZE, crs = "3035")
  )
})

test_that("`quiet` parameter correctly suppresses messages", {
  # Test 1: quiet = TRUE should not produce any messages or progress bars.
  # We wrap the function call in expect_silent() to verify this.
  expect_silent(
    grid_silent <- inspire_grid_from_extent(
      grid_extent = nc,
      cellsize_m = CELLSIZE,
      parallel = FALSE, # Force sequential mode to test its message suppression
      quiet = TRUE
    )
  )

  # Also, perform a basic check to ensure the output is still valid.
  expect_s3_class(grid_silent, "sf")
  expect_gt(nrow(grid_silent), 0) # Check that the grid is not empty

  # Test 2: quiet = FALSE (explicitly set) should produce a message.
  # We check for the specific message produced when running in sequential mode.
  expect_message(
    grid_verbose <- inspire_grid_from_extent(
      grid_extent = nc,
      cellsize_m = CELLSIZE,
      parallel = FALSE, # Force sequential mode to get a predictable message
      quiet = FALSE
    ),
    regexp = "Running in sequential mode."
  )

  # Verify the output is still correct and consistent with the silent run.
  expect_s3_class(grid_verbose, "sf")
  expect_equal(nrow(grid_silent), nrow(grid_verbose))

  # Test 3: Check the default behavior (quiet is not specified).
  # The default is FALSE, so it should produce the fallback message
  # when no parallel backend is configured.
  expect_message(
    inspire_grid_from_extent(
      grid_extent = nc,
      cellsize_m = CELLSIZE,
      parallel = "auto" # Allow auto-detection to fall back to sequential
    ),
    regexp = "No parallel backend detected"
  )
})

test_that("dataframe, sf_points and sf_polygons outputs are consistent", {
  # Generate both types of grids with the same parameters, ensuring IDs are created
  # for reliable row-wise comparison.
  grid_poly <- inspire_grid_from_extent(
    nc,
    CELLSIZE,
    crs = TARGET_CRS,
    clip_to_input = TRUE,
    id_format = "long",
    output_type = "sf_polygons"
  )

  grid_pts <- inspire_grid_from_extent(
    nc,
    CELLSIZE,
    crs = TARGET_CRS,
    clip_to_input = TRUE,
    id_format = "long",
    output_type = "sf_points"
  )

  grid_df <- inspire_grid_from_extent(
    nc,
    CELLSIZE,
    crs = TARGET_CRS,
    clip_to_input = TRUE,
    id_format = "long",
    output_type = "dataframe"
  )

  # Check for the same number of features
  expect_equal(nrow(grid_poly), nrow(grid_pts))
  expect_equal(nrow(grid_poly), nrow(grid_df))
  expect_equal(nrow(grid_pts), nrow(grid_df))

  # Check for spatial correspondence
  # Ensure both are sorted by the same ID to match rows correctly
  poly_sorted <- grid_poly[order(grid_poly$GRD_ID), ]
  pts_sorted <- grid_pts[order(grid_pts$GRD_ID), ]

  # Check the ids
  expect_equal(poly_sorted$GRD_ID, pts_sorted$GRD_ID)
  expect_equal(poly_sorted$GRD_ID, grid_df$GRD_ID)
  expect_equal(pts_sorted$GRD_ID, grid_df$GRD_ID)

  # Check that each polygon contains its corresponding point (centroid)
  # st_contains() returns a sparse list by default; we can check its diagonal
  # when converted to a dense matrix.
  contains_matrix <- sf::st_contains(poly_sorted, pts_sorted, sparse = FALSE)

  # The diagonal of this matrix must be all TRUE, meaning polygon `i`
  # contains point `i`.
  expect_true(all(diag(contains_matrix)))

  # As a sanity check, the total number of TRUEs in the matrix should equal
  # the number of points, meaning each point falls into exactly one polygon.
  expect_equal(sum(contains_matrix), nrow(pts_sorted))
})

test_that("GRD_IDs are unique", {
  # Standard case
  grid_sf <- inspire_grid_from_extent(nc, CELLSIZE, crs = TARGET_CRS, id_format = "long")
  expect_true("GRD_ID" %in% names(grid_sf))
  expect_false(any(duplicated(grid_sf$GRD_ID)))

  # Short IDs
  grid_short <- inspire_grid_from_extent(nc, CELLSIZE, crs = TARGET_CRS, id_format = "short")
  expect_true("GRD_ID" %in% names(grid_short))
  expect_false(any(duplicated(grid_short$GRD_ID)))

  # Both IDs
  grid_both <- inspire_grid_from_extent(nc, CELLSIZE, crs = TARGET_CRS, id_format = "both")
  expect_true("GRD_ID_LONG" %in% names(grid_both))
  expect_true("GRD_ID_SHORT" %in% names(grid_both))
  expect_false(any(duplicated(grid_both$GRD_ID_LONG)))
  expect_false(any(duplicated(grid_both$GRD_ID_SHORT)))
})

test_that("Memory warning is triggered with insufficient (fake) RAM", {
  # Set fake RAM to a ridiculously small value to guarantee a warning
  withr::with_options(
    list(gridmaker.fake_ram = 0.001), # approx 1 Mb
    # options("gridmaker.fake_ram" = 0.001)
    # options("gridmaker.fake_ram" = NULL)
    # .get_ram_gb("avail")
    {
      expect_warning(
        inspire_grid_from_extent(nc, CELLSIZE, crs = TARGET_CRS),
        regexp = "Estimated grid size is.*which may exceed your available system memory"
      )
    }
  )

  # Ensure no warning is issued when there's plenty of (fake) RAM
  withr::with_options(
    list(gridmaker.fake_ram = 1000), # 1000 GB should be enough
    {
      # Using expect_silent because expect_no_warning is not in testthat < 3.0.0
      # and we want to be robust. expect_silent checks for warnings, messages,
      # and other output.
      expect_silent(
        inspire_grid_from_extent(nc, CELLSIZE, crs = TARGET_CRS, quiet = TRUE)
      )
    }
  )

  # Ensure the option is unset and doesn't interfere with other tests
  expect_null(getOption("gridmaker.fake_ram"))
})

test_that("inspire_grid_from_extent respects axis_order argument for Short IDs", {
  # Use a simple bounding box for testing
  # 10km grid
  # LLC at 0,0
  simple_extent <- sf::st_bbox(c(xmin = 0, ymin = 0, xmax = 20000, ymax = 10000), crs = 3035)
  cellsize <- 10000

  # 1. Test Default (NE)
  grid_ne <- inspire_grid_from_extent(
    simple_extent,
    cellsize_m = cellsize,
    id_format = "short",
    axis_order = "NE",
    quiet = TRUE
  )

  # Expect format: 10kmN0E0 (Northing then Easting)
  # Check the first ID (LLC 0,0)
  expected_ne <- "10kmN0E0"
  expect_true(expected_ne %in% grid_ne$GRD_ID)

  # 2. Test EN Order
  grid_en <- inspire_grid_from_extent(
    simple_extent,
    cellsize_m = cellsize,
    id_format = "short",
    axis_order = "EN",
    quiet = TRUE
  )

  # Expect format: 10kmE0N0 (Easting then Northing)
  expected_en <- "10kmE0N0"
  expect_true(expected_en %in% grid_en$GRD_ID)

  # 3. Test that Long IDs are NOT affected (Must remain N...E for standard compliance)
  grid_both_en <- inspire_grid_from_extent(
    simple_extent,
    cellsize_m = cellsize,
    id_format = "both",
    axis_order = "EN",
    quiet = TRUE
  )

  # Short ID should be EN
  expect_true("10kmE0N0" %in% grid_both_en$GRD_ID_SHORT)

  # Long ID should still be NE (CRS...RES...N...E...)
  # 0,0 is usually N0E0
  long_id <- grid_both_en$GRD_ID_LONG[grid_both_en$GRD_ID_SHORT == "10kmE0N0"]
  expect_true(grepl("N0E0", long_id))
  expect_false(grepl("E0N0$", long_id)) # Should not end in E...N...
})

test_that("inspire_grid_from_extent throws error for invalid axis_order", {
  expect_error(
    inspire_grid_from_extent(nc, CELLSIZE, axis_order = "XY"),
    regexp = "'arg' should be one of"
  )
})

test_that("S3 dispatch works correctly for inspire_grid()", {
  # Test 1: inspire_grid() with sf object should call inspire_grid_from_extent
  grid_s3_sf <- inspire_grid(nc, cellsize_m = CELLSIZE, crs = TARGET_CRS, quiet = TRUE)
  grid_direct_sf <- inspire_grid_from_extent(nc, cellsize_m = CELLSIZE, crs = TARGET_CRS, quiet = TRUE)
  
  expect_s3_class(grid_s3_sf, "sf")
  expect_equal(nrow(grid_s3_sf), nrow(grid_direct_sf))
  expect_equal(names(grid_s3_sf), names(grid_direct_sf))
  
  # Test 2: inspire_grid() with sfc object
  grid_s3_sfc <- inspire_grid(st_geometry(nc), cellsize_m = CELLSIZE, crs = TARGET_CRS, quiet = TRUE)
  grid_direct_sfc <- inspire_grid_from_extent(st_geometry(nc), cellsize_m = CELLSIZE, crs = TARGET_CRS, quiet = TRUE)
  
  expect_equal(nrow(grid_s3_sfc), nrow(grid_direct_sfc))
  
  # Test 3: inspire_grid() with bbox object
  grid_s3_bbox <- inspire_grid(st_bbox(nc), cellsize_m = CELLSIZE, crs = TARGET_CRS, quiet = TRUE)
  grid_direct_bbox <- inspire_grid_from_extent(st_bbox(nc), cellsize_m = CELLSIZE, crs = TARGET_CRS, quiet = TRUE)
  
  expect_equal(nrow(grid_s3_bbox), nrow(grid_direct_bbox))
  
  # Test 4: inspire_grid() with numeric vector
  nc_bbox_num <- as.numeric(st_bbox(nc))
  grid_s3_num <- inspire_grid(nc_bbox_num, cellsize_m = CELLSIZE, crs = TARGET_CRS, quiet = TRUE)
  grid_direct_num <- inspire_grid_from_extent(nc_bbox_num, cellsize_m = CELLSIZE, crs = TARGET_CRS, quiet = TRUE)
  
  expect_equal(nrow(grid_s3_num), nrow(grid_direct_num))
  
  # Test 5: inspire_grid() with matrix
  nc_bbox_mat <- matrix(nc_bbox_num, nrow = 2)
  grid_s3_mat <- inspire_grid(nc_bbox_mat, cellsize_m = CELLSIZE, crs = TARGET_CRS, quiet = TRUE)
  grid_direct_mat <- inspire_grid_from_extent(nc_bbox_mat, cellsize_m = CELLSIZE, crs = TARGET_CRS, quiet = TRUE)
  
  expect_equal(nrow(grid_s3_mat), nrow(grid_direct_mat))
  
  # Test 6: inspire_grid() with character vector (INSPIRE IDs)
  inspire_ids <- c(
    "CRS3035RES100000mN2600000E4300000",
    "CRS3035RES100000mN2600000E4400000",
    "CRS3035RES100000mN2700000E4100000"
  )
  
  grid_s3_ids <- inspire_grid(inspire_ids, quiet = TRUE)
  grid_direct_ids <- inspire_grid_from_ids(inspire_ids, quiet = TRUE)
  
  expect_s3_class(grid_s3_ids, "sf")
  expect_equal(nrow(grid_s3_ids), nrow(grid_direct_ids))
  expect_equal(nrow(grid_s3_ids), 3)
  expect_equal(grid_s3_ids$id, grid_direct_ids$id)
})

test_that("S3 dispatch preserves all arguments", {
  # Test that arguments are correctly passed through S3 dispatch
  grid_s3 <- inspire_grid(
    nc,
    cellsize_m = CELLSIZE,
    crs = TARGET_CRS,
    output_type = "sf_points",
    clip_to_input = TRUE,
    id_format = "short",
    axis_order = "EN",
    include_llc = FALSE,
    quiet = TRUE
  )

  grid_direct <- inspire_grid_from_extent(
    grid_extent = nc,
    cellsize_m = CELLSIZE,
    crs = TARGET_CRS,
    output_type = "sf_points",
    clip_to_input = TRUE,
    id_format = "short",
    axis_order = "EN",
    include_llc = FALSE,
    quiet = TRUE
  )

  # Should produce identical results
  expect_equal(nrow(grid_s3), nrow(grid_direct))
  expect_equal(names(grid_s3), names(grid_direct))
  expect_false("X_LLC" %in% names(grid_s3))
  expect_true(any(class(st_geometry(grid_s3)) == "sfc_POINT"))

  # Check that axis_order was respected
  expect_true(all(grepl("E.*N", grid_s3$GRD_ID)))
})

test_that("API Safety: inspire_grid.character warns on irrelevant arguments", {
  # Create some dummy IDs
  ids <- c("CRS3035RES1000mN3000000E4000000")

  # 1. Test that extent-specific arguments trigger a warning
  # We expect a warning about "clip_to_input" being ignored
  expect_warning(
    inspire_grid(ids, clip_to_input = TRUE, quiet = TRUE),
    regexp = "Arguments.*are ignored"
  )

  expect_warning(
    inspire_grid(ids, cellsize_m = 500, quiet = TRUE),
    regexp = "Arguments.*are ignored"
  )

  # 2. Test that these arguments do NOT cause a crash
  # (i.e., they are successfully removed from '...' before hitting internal functions)
  res <- suppressWarnings(
    inspire_grid(ids, buffer_m = 500, use_convex_hull = TRUE, quiet = TRUE)
  )
  expect_s3_class(res, "sf")
  expect_equal(nrow(res), 1)
})

test_that("API Safety: inspire_grid.character allows 'crs' to pass through", {
  # Use Short IDs which require a CRS
  short_ids <- c("1kmN3000E4000")
  target_crs <- 3857

  # 1. Ensure passing 'crs' works and is applied
  # If 'crs' was accidentally in the "Argument Sink", this would default to 3035
  grid_3857 <- inspire_grid(short_ids, crs = target_crs, quiet = TRUE)

  expect_equal(sf::st_crs(grid_3857)$epsg, target_crs)

  # 2. Ensure passing 'crs' does NOT trigger the "ignored argument" warning
  # We might get other warnings (like "CRS not specified" if the logic is off),
  # but we specifically don't want the "Arguments ... are ignored" warning for 'crs'.
  expect_warning(
    inspire_grid(short_ids, crs = target_crs, quiet = TRUE),
    regexp = NA # Expect NO warning
  )
})

test_that("API Safety: inspire_grid generic defaults do not pollute methods", {
  # The generic has defaults (e.g., cellsize_m = NULL).
  # We need to ensure that simply calling inspire_grid(ids)
  # does NOT trigger warnings just because the default values exist.

  ids <- c("CRS3035RES1000mN3000000E4000000")

  # Should be silent (no warnings about ignored NULLs)
  expect_silent(
    inspire_grid(ids, quiet = TRUE)
  )
})
