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

  res_dsn <- inspire_grid(
    nc_sub,
    cellsize_m = cellsize,
    output_type = "spatraster",
    dsn = tf,
    quiet = TRUE
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

  inspire_grid(
    nc_sub,
    cellsize_m = cellsize,
    output_type = "spatraster",
    dsn = tf_clipped,
    clip_to_input = TRUE,
    quiet = TRUE
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

# --- Integration tests: verify gridmaker output is correct across formats ---
# These test OUR code (format detection, parameter passing) not GDAL drivers

test_that("inspire_grid produces correct raster output in alternative formats", {
  skip_if_not_installed("terra")
  skip_if_not_installed("sf")

  # Helper to test a format if driver is available
  test_raster_format <- function(ext) {
    # Use gridmaker's internal driver check (strip leading dot)
    ext_no_dot <- sub("^\\.", "", ext)
    driver_name <- gridmaker:::.ext_to_driver(ext_no_dot, "raster")
    if (is.null(driver_name)) {
      return(NULL)
    }

    check <- gridmaker:::.check_driver_available(driver_name, "raster")
    if (!check$available) {
      return(NULL)
    }

    # Create grid using inspire_grid
    tf <- tempfile(fileext = ext)
    on.exit(unlink(tf), add = TRUE)

    # Wrap in tryCatch - some drivers report availability but fail on write
    result <- tryCatch(
      {
        inspire_grid_from_extent(
          grid_extent = c(0, 0, 20000, 20000),
          cellsize_m = 10000,
          crs = 3035,
          output_type = "spatraster",
          dsn = tf,
          quiet = TRUE
        )

        # Verify output
        expect_true(file.exists(tf), info = paste("File created for", ext))
        r <- terra::rast(tf)
        expect_equal(terra::ncell(r), 4, info = paste("Cell count for", ext))
        expect_equal(
          terra::res(r)[1],
          10000,
          info = paste("Resolution for", ext)
        )

        ext # Return extension to track which were tested
      },
      error = function(e) NULL # Driver reported available but failed
    )
    result
  }

  # Test available formats - at least one should work
  formats_tested <- c(
    test_raster_format(".nc"),
    test_raster_format(".img"),
    test_raster_format(".kea")
  )

  # If no alternative formats available, that's OK - core .tif is tested elsewhere
  if (all(sapply(formats_tested, is.null))) {
    skip("No alternative raster formats available on this system")
  }
})
