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

test_that("inspire_grid streaming with include_rat works for GeoTIFF", {
  skip_if_not_installed("terra")
  skip_if_not_installed("sf")

  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  nc_proj <- sf::st_transform(nc, 5070)
  nc_sub <- nc_proj[1, ]

  cellsize <- 10000 # Larger cells for faster test
  tf_rat <- tempfile(fileext = ".tif")

  expect_message(
    inspire_grid(
      nc_sub,
      cellsize_m = cellsize,
      output_type = "spatraster",
      dsn = tf_rat,
      id_format = "short",
      include_rat = TRUE,
      quiet = FALSE
    ),
    "RAT"
  )

  # Verify RAT was created
  r_rat <- terra::rast(tf_rat)
  cats <- terra::cats(r_rat)

  # RAT should exist now
  expect_true(length(cats) >= 1)
  if (length(cats) >= 1 && !is.null(cats[[1]])) {
    df <- cats[[1]]
    expect_true(
      "grid_id" %in%
        names(df) ||
        "GRD_ID" %in% names(df) ||
        "GRD_ID_SHORT" %in% names(df)
    )
  }
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

test_that("inspire_grid streaming with include_rat works for NetCDF", {
  skip_if_not_installed("terra")
  skip_if_not_installed("sf")

  # NetCDF driver check - handle different terra versions
  drivers <- terra::gdal(lib = "drivers")
  has_netcdf <- if (is.data.frame(drivers)) {
    "netCDF" %in% drivers$name
  } else {
    any(grepl("netCDF", drivers))
  }

  if (!has_netcdf) {
    skip("NetCDF driver not available in terra/GDAL")
  }

  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  nc_proj <- sf::st_transform(nc, 5070)
  nc_sub <- nc_proj[1, ]

  tf <- tempfile(fileext = ".nc")
  expect_message(
    inspire_grid(
      nc_sub,
      cellsize_m = 10000,
      output_type = "spatraster",
      dsn = tf,
      include_rat = TRUE,
      quiet = FALSE
    ),
    "RAT"
  )

  r <- terra::rast(tf)
  expect_true(length(terra::cats(r)) >= 1)
})

test_that("inspire_grid streaming errors for HDF5 with include_rat", {
  skip_if_not_installed("terra")
  skip_if_not_installed("sf")

  expect_error(
    inspire_grid(
      c(0, 0, 1000, 1000),
      100,
      output_type = "spatraster",
      crs = 3035,
      dsn = "test.hdf",
      include_rat = TRUE
    ),
    "supported"
  )
})

test_that("RAT is filtered to exclude NA cells after clipping", {
  skip_if_not_installed("terra")
  skip_if_not_installed("sf")

  # Create an L-shaped polygon that will force some cells to be masked
  test_poly <- sf::st_as_sfc(
    "POLYGON((4000000 2800000, 4040000 2800000, 4040000 2840000, 4020000 2840000, 4020000 2820000, 4000000 2820000, 4000000 2800000))"
  )
  test_poly <- sf::st_set_crs(test_poly, 3035)

  tf_clipped <- tempfile(fileext = ".tif")

  # Generate grid with clipping and RAT
  expect_message(
    inspire_grid(
      test_poly,
      cellsize_m = 10000,
      output_type = "spatraster",
      dsn = tf_clipped,
      include_rat = TRUE,
      clip_to_input = TRUE,
      quiet = FALSE
    ),
    "Filtered RAT"
  )

  # Load the raster
  r <- terra::rast(tf_clipped)

  # Get the RAT
  rat <- terra::levels(r)[[1]]

  # Get actual non-NA cell values
  raster_values <- terra::values(r)
  non_na_values <- unique(raster_values[!is.na(raster_values)])

  # Key assertion: RAT should only contain entries for non-NA cells
  expect_equal(nrow(rat), length(non_na_values))

  # Verify all RAT entries correspond to actual cell values
  expect_true(all(rat$Value %in% non_na_values))

  # Verify no orphaned entries (RAT values not in raster)
  orphaned <- setdiff(rat$Value, non_na_values)
  expect_equal(length(orphaned), 0)

  # Verify there ARE some NA cells (to confirm clipping worked)
  expect_true(sum(is.na(raster_values)) > 0)
})
