test_that("inspire_grid raster streaming via terra works", {
  skip_if_not_installed("terra")
  skip_if_not_installed("sf")

  # Setup small test area
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  nc_proj <- sf::st_transform(nc, 5070)
  nc_sub <- nc_proj[1, ]

  cellsize <- 5000

  # 1. Test basic streaming to disk (NO RAT by default)
  tf <- tempfile(fileext = ".tif")

  expect_message(
    res_dsn <- inspire_grid(
      nc_sub,
      cellsize_m = cellsize,
      output_type = "spatraster",
      dsn = tf,
      quiet = FALSE
    ),
    "Streaming raster to disk"
  )

  expect_type(res_dsn, "character")
  expect_equal(normalizePath(res_dsn), normalizePath(tf))
  expect_true(file.exists(tf))

  r <- terra::rast(tf)
  expect_s4_class(r, "SpatRaster")
  expect_true(terra::ncell(r) > 0)

  # Verify NO RAT by default
  cats <- terra::cats(r)
  expect_true(length(cats) == 0 || is.null(cats[[1]]) || nrow(cats[[1]]) == 0)

  # 2. Compare with in-memory result
  r_mem <- inspire_grid(
    nc_sub,
    cellsize_m = cellsize,
    output_type = "spatraster",
    dsn = NULL,
    quiet = TRUE
  )

  expect_equal(as.vector(terra::ext(r)), as.vector(terra::ext(r_mem)))
  expect_equal(terra::ncell(r), terra::ncell(r_mem))

  # Values check
  v1 <- terra::values(r, mat = FALSE)
  v2 <- terra::values(r_mem, mat = FALSE)
  expect_equal(v1, v2)

  # 3. Test Clipping in Streaming
  tf_clipped <- tempfile(fileext = ".tif")

  expect_message(
    inspire_grid(
      nc_sub,
      cellsize_m = cellsize,
      output_type = "spatraster",
      dsn = tf_clipped,
      clip_to_input = TRUE,
      quiet = FALSE
    ),
    "Applying mask"
  )

  r_cl <- terra::rast(tf_clipped)
  v_cl <- terra::values(r_cl, mat = FALSE)

  expect_true(any(is.na(v_cl)))
  expect_true(any(!is.na(v_cl)))
})

test_that("inspire_grid streaming with include_rat is deprecated", {
  skip_if_not_installed("terra")
  skip_if_not_installed("sf")

  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  nc_proj <- sf::st_transform(nc, 5070)
  nc_sub <- nc_proj[1, ]

  cellsize <- 10000 # Larger cells for faster test
  tf_rat <- tempfile(fileext = ".tif")

  # Should warn about deprecation
  expect_warning(
    inspire_grid(
      nc_sub,
      cellsize_m = cellsize,
      output_type = "spatraster",
      dsn = tf_rat,
      id_format = "short",
      include_rat = TRUE,
      quiet = TRUE
    ),
    "deprecated"
  )

  # Verify NO RAT was created (feature is disabled)
  r_rat <- terra::rast(tf_rat)
  cats <- terra::cats(r_rat)

  # RAT should NOT exist (deprecated feature)
  expect_true(length(cats) == 0 || is.null(cats[[1]]) || nrow(cats[[1]]) == 0)
})

test_that("inspire_grid streaming works for .tiff (long extension)", {
  skip_if_not_installed("terra")
  skip_if_not_installed("sf")

  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  nc_proj <- sf::st_transform(nc, 5070)
  nc_sub <- nc_proj[1, ]

  tf <- tempfile(fileext = ".tiff")
  res <- inspire_grid(
    nc_sub,
    cellsize_m = 10000,
    output_type = "spatraster",
    dsn = tf,
    quiet = TRUE
  )
  expect_true(file.exists(tf))
  expect_s4_class(terra::rast(tf), "SpatRaster")
})

test_that("inspire_grid streaming errors for missing file extension", {
  skip_if_not_installed("terra")
  skip_if_not_installed("sf")

  expect_error(
    inspire_grid(
      c(0, 0, 1000, 1000),
      100,
      output_type = "spatraster",
      crs = 3035,
      dsn = "testfile_no_extension"
    ),
    "requires a file extension"
  )
})

test_that("File format is preserved after clipping for GeoTIFF", {
  skip_if_not_installed("terra")
  skip_if_not_installed("sf")

  # Create test polygon for clipping
  test_poly <- sf::st_as_sfc(
    "POLYGON((4000000 2800000, 4040000 2800000, 4040000 2840000, 4000000 2840000, 4000000 2800000))"
  )
  test_poly <- sf::st_set_crs(test_poly, 3035)

  tf <- tempfile(fileext = ".tif")

  inspire_grid(
    test_poly,
    cellsize_m = 10000,
    output_type = "spatraster",
    dsn = tf,
    clip_to_input = TRUE,
    quiet = TRUE
  )

  # Verify file exists and can be read
  expect_true(file.exists(tf))
  r <- terra::rast(tf)
  expect_s4_class(r, "SpatRaster")

  # Verify format by checking GDAL driver
  info <- terra::describe(tf)
  expect_true(any(grepl("GTiff", info, ignore.case = TRUE)))
})

test_that("File format is preserved after clipping for NetCDF", {
  skip_if_not_installed("terra")
  skip_if_not_installed("sf")

  # Check for NetCDF driver
  drivers <- terra::gdal(drivers = TRUE)
  netcdf_row <- drivers[drivers$name == "netCDF", ]
  has_netcdf <- nrow(netcdf_row) > 0 && grepl("write", netcdf_row$can)

  if (!has_netcdf) {
    skip("NetCDF driver not available in terra/GDAL")
  }

  test_poly <- sf::st_as_sfc(
    "POLYGON((4000000 2800000, 4040000 2800000, 4040000 2840000, 4000000 2840000, 4000000 2800000))"
  )
  test_poly <- sf::st_set_crs(test_poly, 3035)

  tf <- tempfile(fileext = ".nc")

  inspire_grid(
    test_poly,
    cellsize_m = 10000,
    output_type = "spatraster",
    dsn = tf,
    clip_to_input = TRUE,
    quiet = TRUE
  )

  # Verify file exists and can be read
  expect_true(file.exists(tf))
  r <- terra::rast(tf)
  expect_s4_class(r, "SpatRaster")

  # Verify format by checking GDAL driver
  info <- terra::describe(tf)
  expect_true(any(grepl("netCDF", info, ignore.case = TRUE)))
})

test_that("File format is preserved after clipping for KEA", {
  skip_if_not_installed("terra")
  skip_if_not_installed("sf")

  # Check for KEA driver
  drivers <- terra::gdal(drivers = TRUE)
  kea_row <- drivers[drivers$name == "KEA", ]
  has_kea <- nrow(kea_row) > 0 && grepl("write", kea_row$can)

  if (!has_kea) {
    skip("KEA driver not available in terra/GDAL")
  }

  test_poly <- sf::st_as_sfc(
    "POLYGON((4000000 2800000, 4040000 2800000, 4040000 2840000, 4000000 2840000, 4000000 2800000))"
  )
  test_poly <- sf::st_set_crs(test_poly, 3035)

  tf <- tempfile(fileext = ".kea")

  inspire_grid(
    test_poly,
    cellsize_m = 10000,
    output_type = "spatraster",
    dsn = tf,
    clip_to_input = TRUE,
    quiet = TRUE
  )

  # Verify file exists and can be read
  expect_true(file.exists(tf))
  r <- terra::rast(tf)
  expect_s4_class(r, "SpatRaster")

  # Verify format by checking GDAL driver
  info <- terra::describe(tf)
  expect_true(any(grepl("KEA", info, ignore.case = TRUE)))
})

test_that("File format is preserved after clipping for Erdas Imagine", {
  skip_if_not_installed("terra")
  skip_if_not_installed("sf")

  # Check for HFA (Erdas Imagine) driver
  drivers <- terra::gdal(drivers = TRUE)
  hfa_row <- drivers[drivers$name == "HFA", ]
  has_hfa <- nrow(hfa_row) > 0 && grepl("write", hfa_row$can)

  if (!has_hfa) {
    skip("HFA (Erdas Imagine) driver not available in terra/GDAL")
  }

  test_poly <- sf::st_as_sfc(
    "POLYGON((4000000 2800000, 4040000 2800000, 4040000 2840000, 4000000 2840000, 4000000 2800000))"
  )
  test_poly <- sf::st_set_crs(test_poly, 3035)

  tf <- tempfile(fileext = ".img")

  inspire_grid(
    test_poly,
    cellsize_m = 10000,
    output_type = "spatraster",
    dsn = tf,
    clip_to_input = TRUE,
    quiet = TRUE
  )

  # Verify file exists and can be read
  expect_true(file.exists(tf))
  r <- terra::rast(tf)
  expect_s4_class(r, "SpatRaster")

  # Verify format by checking GDAL driver
  info <- terra::describe(tf)
  expect_true(any(grepl("HFA", info, ignore.case = TRUE)))
})

test_that("File format is preserved after clipping for HDF5", {
  skip_if_not_installed("terra")
  skip_if_not_installed("sf")

  # Check for HDF5 driver
  drivers <- terra::gdal(drivers = TRUE)
  hdf5_row <- drivers[drivers$name == "HDF5", ]
  has_hdf5 <- nrow(hdf5_row) > 0 && grepl("write", hdf5_row$can)

  if (!has_hdf5) {
    skip("HDF5 driver not available in terra/GDAL")
  }

  test_poly <- sf::st_as_sfc(
    "POLYGON((4000000 2800000, 4040000 2800000, 4040000 2840000, 4000000 2840000, 4000000 2800000))"
  )
  test_poly <- sf::st_set_crs(test_poly, 3035)

  tf <- tempfile(fileext = ".hdf")

  inspire_grid(
    test_poly,
    cellsize_m = 10000,
    output_type = "spatraster",
    dsn = tf,
    clip_to_input = TRUE,
    quiet = TRUE
  )

  # Verify file exists and can be read
  expect_true(file.exists(tf))
  r <- terra::rast(tf)
  expect_s4_class(r, "SpatRaster")

  # Verify format by checking GDAL driver
  info <- terra::describe(tf)
  expect_true(any(grepl("HDF5", info, ignore.case = TRUE)))
})

test_that("Chunked raster generation produces correct cell values", {
  skip_if_not_installed("terra")
  skip_if_not_installed("sf")

  # Create a simple test polygon
  test_poly <- sf::st_as_sfc(
    "POLYGON((4000000 2800000, 4030000 2800000, 4030000 2830000, 4000000 2830000, 4000000 2800000))"
  )
  test_poly <- sf::st_set_crs(test_poly, 3035)

  tf <- tempfile(fileext = ".tif")
  cellsize <- 10000

  inspire_grid(
    test_poly,
    cellsize_m = cellsize,
    output_type = "spatraster",
    dsn = tf,
    quiet = TRUE
  )

  r <- terra::rast(tf)

  # For a 3x3 grid, cell IDs should be 1-9
  vals <- terra::values(r, mat = FALSE)
  expected_vals <- 1:9
  expect_equal(sort(vals), expected_vals)

  # Verify cell 1 is top-left, cell 9 is bottom-right (row-major order)
  expect_equal(vals[1], 1)
  expect_equal(vals[9], 9)
})
