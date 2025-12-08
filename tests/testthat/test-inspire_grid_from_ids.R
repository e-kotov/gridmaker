test_that("inspire_grid_from_ids_internal produces correct output types", {
  ids <- c(
    "CRS3035RES1000mN3500000E4400000",
    "CRS3035RES1000mN3501000E4400000"
  )

  # Test sf_polygons output (default id_format = "both")
  grid_poly <- inspire_grid_from_ids_internal(ids, output_type = "sf_polygons")
  expect_s3_class(grid_poly, "sf")
  expect_true(inherits(st_geometry(grid_poly), "sfc_POLYGON"))
  expect_equal(nrow(grid_poly), 2)
  expect_true(all(c("GRD_ID_LONG", "GRD_ID_SHORT") %in% names(grid_poly)))
  expect_equal(st_crs(grid_poly), st_crs(3035))

  # Test sf_points output
  grid_pts <- inspire_grid_from_ids_internal(ids, output_type = "sf_points")
  expect_s3_class(grid_pts, "sf")
  expect_true(inherits(st_geometry(grid_pts), "sfc_POINT"))
  expect_equal(nrow(grid_pts), 2)
  expect_true(all(c("GRD_ID_LONG", "GRD_ID_SHORT") %in% names(grid_pts)))
  expect_equal(st_crs(grid_pts), st_crs(3035))

  # Test dataframe output
  grid_df <- inspire_grid_from_ids_internal(ids, output_type = "dataframe")
  expect_s3_class(grid_df, "data.frame")
  expect_equal(nrow(grid_df), 2)
  expect_true(all(c("GRD_ID_LONG", "GRD_ID_SHORT") %in% names(grid_df)))
  expect_true(all(
    c("X_LLC", "Y_LLC", "X_centroid", "Y_centroid") %in% names(grid_df)
  ))
})

test_that("inspire_grid_from_ids_internal point_type logic is correct", {
  id <- "CRS3035RES1000mN3500000E4400000"
  cellsize <- 1000
  x_llc <- 4400000
  y_llc <- 3500000

  # Test llc points
  grid_llc <- inspire_grid_from_ids_internal(
    id,
    output_type = "sf_points",
    point_type = "llc"
  )
  coords_llc <- st_coordinates(grid_llc)
  expect_equal(as.numeric(coords_llc[1, "X"]), x_llc)
  expect_equal(as.numeric(coords_llc[1, "Y"]), y_llc)

  # Test centroid points
  grid_centroid <- inspire_grid_from_ids_internal(
    id,
    output_type = "sf_points",
    point_type = "centroid"
  )
  coords_centroid <- st_coordinates(grid_centroid)
  expect_equal(as.numeric(coords_centroid[1, "X"]), x_llc + cellsize / 2)
  expect_equal(as.numeric(coords_centroid[1, "Y"]), y_llc + cellsize / 2)
})

test_that("inspire_grid_from_ids_internal handles short format IDs", {
  # The underlying inspire_id_to_coords is tested thoroughly, this is an integration check
  short_ids <- c("10kmN350E440", "10kmN351E440")
  expect_warning(
    grid_short <- inspire_grid_from_ids_internal(short_ids, output_type = "sf_polygons"),
    regexp = "CRS not specified for short-form IDs"
  )
  expect_s3_class(grid_short, "sf")
  expect_equal(nrow(grid_short), 2)
  expect_equal(st_crs(grid_short), st_crs(3035))
})


test_that("inspire_grid_from_ids_internal throws errors for invalid ID sets", {
  # 1. Mixed CRSs
  mixed_crs_ids <- c(
    "CRS3035RES1000mN3500000E4400000",
    "CRS3857RES1000mN3501000E4400000"
  )
  expect_error(
    inspire_grid_from_ids_internal(mixed_crs_ids),
    "INSPIRE identifiers contain more than one CRS"
  )

  # 2. Mixed cell sizes
  mixed_cellsize_ids <- c(
    "CRS3035RES1000mN3500000E4400000",
    "CRS3035RES100mN3501000E4400000"
  )
  expect_error(
    inspire_grid_from_ids_internal(mixed_cellsize_ids),
    "Multiple different cell sizes found"
  )

  # 3. Geographic (long/lat) CRS
  geo_id <- "CRS4326RES100mN50E10"
  expect_error(
    inspire_grid_from_ids_internal(geo_id),
    "The coordinate reference system must be a projected system"
  )
})

test_that("inspire_grid_from_ids_internal handles empty input", {
  empty_ids <- character(0)

  # Polygons
  expect_error(
    inspire_grid_from_ids_internal(empty_ids),
    "Input 'inspire' cannot be an empty vector."
  )

  # Points
  expect_error(
    inspire_grid_from_ids_internal(empty_ids, output_type = "sf_points"),
    "Input 'inspire' cannot be an empty vector."
  )

  # Data frame
  expect_error(
    inspire_grid_from_ids_internal(empty_ids, output_type = "dataframe"),
    "Input 'inspire' cannot be an empty vector."
  )
})

test_that("Round-trip: grid from extent -> short IDs -> grid from IDs matches original", {
  # This test verifies that a grid can be correctly reconstructed from short IDs
  # when CRS is provided, fixing the coordinate scaling and CRS propagation bugs.

  # Step 1: Create a small extent for testing
  test_extent <- st_bbox(
    c(xmin = 1000000, ymin = 1000000, xmax = 1030000, ymax = 1030000),
    crs = st_crs(TARGET_CRS)
  )

  # Step 2: Generate the original grid from extent with short IDs
  grid_original <- inspire_grid_from_extent(
    grid_extent = test_extent,
    cellsize_m = CELLSIZE,
    crs = TARGET_CRS,
    output_type = "sf_polygons",
    id_format = "short"
  )

  expect_s3_class(grid_original, "sf")
  expect_gt(nrow(grid_original), 0)
  expect_true("GRD_ID" %in% names(grid_original))

  # Step 3: Extract the short IDs
  short_ids <- grid_original$GRD_ID

  # Verify they are indeed short format (no "CRS" prefix)
  expect_true(all(!startsWith(short_ids, "CRS")))

  # Step 4: Regenerate grid from short IDs WITH crs parameter
  # This should NOT produce a warning and should use the correct CRS
  grid_regenerated <- inspire_grid_from_ids_internal(
    short_ids,
    output_type = "sf_polygons",
    crs = TARGET_CRS
  )

  # Step 5: Compare the two grids
  expect_s3_class(grid_regenerated, "sf")
  expect_equal(nrow(grid_regenerated), nrow(grid_original))
  expect_equal(st_crs(grid_regenerated), st_crs(grid_original))

  # The regenerated grid should have the same cell positions
  # Sort both by short ID to ensure proper comparison
  grid_original_sorted <- grid_original[order(grid_original$GRD_ID), ]
  grid_regenerated_sorted <- grid_regenerated[order(grid_regenerated$GRD_ID_SHORT), ]

  # Compare LLC coordinates (these should match exactly)
  expect_equal(
    grid_original_sorted$X_LLC,
    grid_regenerated_sorted$X_LLC,
    tolerance = 1e-6
  )
  expect_equal(
    grid_original_sorted$Y_LLC,
    grid_regenerated_sorted$Y_LLC,
    tolerance = 1e-6
  )

  # Compare geometries (should be identical)
  geom_original <- st_geometry(grid_original_sorted)
  geom_regenerated <- st_geometry(grid_regenerated_sorted)

  # Extract centroids and compare
  centroid_original <- st_coordinates(st_centroid(geom_original))
  centroid_regenerated <- st_coordinates(st_centroid(geom_regenerated))

  expect_equal(
    centroid_original[, "X"],
    centroid_regenerated[, "X"],
    tolerance = 1e-6
  )
  expect_equal(
    centroid_original[, "Y"],
    centroid_regenerated[, "Y"],
    tolerance = 1e-6
  )
})

test_that("Short IDs with explicit CRS parameter do not produce warnings", {
  # This test verifies that the CRS propagation fix works correctly
  short_ids <- c("10kmN100E100", "10kmN101E100")

  # With CRS provided, there should be NO warning
  expect_no_warning(
    grid_with_crs <- inspire_grid_from_ids_internal(
      short_ids,
      output_type = "sf_polygons",
      crs = TARGET_CRS
    )
  )

  expect_s3_class(grid_with_crs, "sf")
  expect_equal(st_crs(grid_with_crs), st_crs(TARGET_CRS))
})

test_that("id_format = 'long' produces long format IDs", {
  # From long input IDs
  long_ids <- c(
    "CRS3035RES1000mN3500000E4400000",
    "CRS3035RES1000mN3501000E4400000"
  )

  grid_long <- inspire_grid_from_ids_internal(
    long_ids,
    output_type = "sf_polygons",
    id_format = "long"
  )

  expect_true("GRD_ID" %in% names(grid_long))
  expect_false(any(c("GRD_ID_LONG", "GRD_ID_SHORT") %in% names(grid_long)))
  expect_true(all(startsWith(grid_long$GRD_ID, "CRS")))
  expect_equal(grid_long$GRD_ID, long_ids)

  # From short input IDs - should convert to long
  short_ids <- c("1kmN3500E4400", "1kmN3501E4400")

  expect_warning(
    grid_from_short <- inspire_grid_from_ids_internal(
      short_ids,
      output_type = "sf_polygons",
      id_format = "long"
    ),
    regexp = "CRS not specified"
  )

  expect_true("GRD_ID" %in% names(grid_from_short))
  expect_true(all(startsWith(grid_from_short$GRD_ID, "CRS")))
})

test_that("id_format = 'short' produces short format IDs", {
  # From long input IDs
  long_ids <- c(
    "CRS3035RES1000mN3500000E4400000",
    "CRS3035RES1000mN3501000E4400000"
  )

  grid_short <- inspire_grid_from_ids_internal(
    long_ids,
    output_type = "sf_polygons",
    id_format = "short"
  )

  expect_true("GRD_ID" %in% names(grid_short))
  expect_false(any(c("GRD_ID_LONG", "GRD_ID_SHORT") %in% names(grid_short)))
  expect_false(any(startsWith(grid_short$GRD_ID, "CRS")))
  # Default axis_order is "NE", so IDs should start with "1kmN"
  expect_true(all(startsWith(grid_short$GRD_ID, "1kmN")))
})

test_that("id_format = 'both' produces both long and short format IDs", {
  long_ids <- c(
    "CRS3035RES1000mN3500000E4400000",
    "CRS3035RES1000mN3501000E4400000"
  )

  grid_both <- inspire_grid_from_ids_internal(
    long_ids,
    output_type = "sf_polygons",
    id_format = "both"
  )

  expect_true(all(c("GRD_ID_LONG", "GRD_ID_SHORT") %in% names(grid_both)))
  expect_false("GRD_ID" %in% names(grid_both))
  expect_true(all(startsWith(grid_both$GRD_ID_LONG, "CRS")))
  expect_false(any(startsWith(grid_both$GRD_ID_SHORT, "CRS")))
})

test_that("axis_order = 'EN' produces E...N format short IDs", {
  long_ids <- c(
    "CRS3035RES1000mN3500000E4400000",
    "CRS3035RES1000mN3501000E4400000"
  )

  # Test with id_format = "short"
  grid_en <- inspire_grid_from_ids_internal(
    long_ids,
    output_type = "sf_polygons",
    id_format = "short",
    axis_order = "EN"
  )

  expect_true("GRD_ID" %in% names(grid_en))
  # IDs should start with "1kmE" (not "1kmN")
  expect_true(all(startsWith(grid_en$GRD_ID, "1kmE")))
  expect_true(all(grepl("E[0-9]+N[0-9]+$", grid_en$GRD_ID)))

  # Test with id_format = "both"
  grid_both_en <- inspire_grid_from_ids_internal(
    long_ids,
    output_type = "sf_polygons",
    id_format = "both",
    axis_order = "EN"
  )

  expect_true(all(startsWith(grid_both_en$GRD_ID_SHORT, "1kmE")))
})

test_that("axis_order = 'NE' produces N...E format short IDs (default)", {
  long_ids <- c(
    "CRS3035RES1000mN3500000E4400000",
    "CRS3035RES1000mN3501000E4400000"
  )

  grid_ne <- inspire_grid_from_ids_internal(
    long_ids,
    output_type = "sf_polygons",
    id_format = "short",
    axis_order = "NE"
  )

  expect_true("GRD_ID" %in% names(grid_ne))
  # IDs should start with "1kmN" (not "1kmE")
  expect_true(all(startsWith(grid_ne$GRD_ID, "1kmN")))
  expect_true(all(grepl("N[0-9]+E[0-9]+$", grid_ne$GRD_ID)))
})

test_that("axis_order converts short IDs between NE and EN formats", {
  # Input short IDs in NE format
  short_ids_ne <- c("1kmN3500E4400", "1kmN3501E4400")

  expect_warning(
    grid_to_en <- inspire_grid_from_ids_internal(
      short_ids_ne,
      output_type = "sf_polygons",
      id_format = "short",
      axis_order = "EN"
    ),
    regexp = "CRS not specified"
  )

  # Should convert NE -> Long -> EN
  expect_true(all(startsWith(grid_to_en$GRD_ID, "1kmE")))
  expect_equal(grid_to_en$GRD_ID, c("1kmE4400N3500", "1kmE4400N3501"))
})

test_that("inspire_grid S3 generic passes id_format and axis_order correctly", {
  long_ids <- c(
    "CRS3035RES1000mN3500000E4400000",
    "CRS3035RES1000mN3501000E4400000"
  )

  # Test via S3 generic with id_format = "long"
  grid_long <- inspire_grid(
    x = long_ids,
    id_format = "long"
  )

  expect_true("GRD_ID" %in% names(grid_long))
  expect_true(all(startsWith(grid_long$GRD_ID, "CRS")))

  # Test via S3 generic with id_format = "short" and axis_order = "EN"
  grid_short_en <- inspire_grid(
    x = long_ids,
    id_format = "short",
    axis_order = "EN"
  )

  expect_true("GRD_ID" %in% names(grid_short_en))
  expect_true(all(startsWith(grid_short_en$GRD_ID, "1kmE")))
})
