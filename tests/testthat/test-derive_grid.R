test_that("derive_grid_internal produces correct output types", {
  ids <- c(
    "CRS3035RES1000mN3500000E4400000",
    "CRS3035RES1000mN3501000E4400000"
  )

  # Test sf_polygons output
  grid_poly <- derive_grid_internal(ids, output_type = "sf_polygons")
  expect_s3_class(grid_poly, "sf")
  expect_true(inherits(st_geometry(grid_poly), "sfc_POLYGON"))
  expect_equal(nrow(grid_poly), 2)
  expect_true("id" %in% names(grid_poly))
  expect_equal(st_crs(grid_poly), st_crs(3035))

  # Test sf_points output
  grid_pts <- derive_grid_internal(ids, output_type = "sf_points")
  expect_s3_class(grid_pts, "sf")
  expect_true(inherits(st_geometry(grid_pts), "sfc_POINT"))
  expect_equal(nrow(grid_pts), 2)
  expect_true("id" %in% names(grid_pts))
  expect_equal(st_crs(grid_pts), st_crs(3035))

  # Test dataframe output
  grid_df <- derive_grid_internal(ids, output_type = "dataframe")
  expect_s3_class(grid_df, "data.frame")
  expect_equal(nrow(grid_df), 2)
  expect_true("id" %in% names(grid_df))
  expect_true(all(
    c("X_LLC", "Y_LLC", "X_centroid", "Y_centroid") %in% names(grid_df)
  ))
})

test_that("derive_grid_internal point_type logic is correct", {
  id <- "CRS3035RES1000mN3500000E4400000"
  cellsize <- 1000
  x_llc <- 4400000
  y_llc <- 3500000

  # Test llc points
  grid_llc <- derive_grid_internal(
    id,
    output_type = "sf_points",
    point_type = "llc"
  )
  coords_llc <- st_coordinates(grid_llc)
  expect_equal(as.numeric(coords_llc[1, "X"]), x_llc)
  expect_equal(as.numeric(coords_llc[1, "Y"]), y_llc)

  # Test centroid points
  grid_centroid <- derive_grid_internal(
    id,
    output_type = "sf_points",
    point_type = "centroid"
  )
  coords_centroid <- st_coordinates(grid_centroid)
  expect_equal(as.numeric(coords_centroid[1, "X"]), x_llc + cellsize / 2)
  expect_equal(as.numeric(coords_centroid[1, "Y"]), y_llc + cellsize / 2)
})

test_that("derive_grid_internal handles short format IDs", {
  # The underlying inspire_extract is tested thoroughly, this is an integration check
  short_ids <- c("10kmN350E440", "10kmN351E440")
  expect_warning(
    grid_short <- derive_grid_internal(short_ids, output_type = "sf_polygons"),
    regexp = "CRS not specified for short-form IDs"
  )
  expect_s3_class(grid_short, "sf")
  expect_equal(nrow(grid_short), 2)
  expect_equal(st_crs(grid_short), st_crs(3035))
})


test_that("derive_grid_internal throws errors for invalid ID sets", {
  # 1. Mixed CRSs
  mixed_crs_ids <- c(
    "CRS3035RES1000mN3500000E4400000",
    "CRS3857RES1000mN3501000E4400000"
  )
  expect_error(
    derive_grid_internal(mixed_crs_ids),
    "INSPIRE identifiers contain more than one CRS"
  )

  # 2. Mixed cell sizes
  mixed_cellsize_ids <- c(
    "CRS3035RES1000mN3500000E4400000",
    "CRS3035RES100mN3501000E4400000"
  )
  expect_error(
    derive_grid_internal(mixed_cellsize_ids),
    "Multiple different cell sizes found"
  )

  # 3. Geographic (long/lat) CRS
  geo_id <- "CRS4326RES100mN50E10"
  expect_error(
    derive_grid_internal(geo_id),
    "The coordinate reference system must be a projected system"
  )
})

test_that("derive_grid_internal handles empty input", {
  empty_ids <- character(0)

  # Polygons
  expect_error(
    derive_grid_internal(empty_ids),
    "Input 'inspire' cannot be an empty vector."
  )

  # Points
  expect_error(
    derive_grid_internal(empty_ids, output_type = "sf_points"),
    "Input 'inspire' cannot be an empty vector."
  )

  # Data frame
  expect_error(
    derive_grid_internal(empty_ids, output_type = "dataframe"),
    "Input 'inspire' cannot be an empty vector."
  )
})
