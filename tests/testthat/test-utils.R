
test_that("regex_match works correctly", {
  text <- c("a1", "b2", "c3")
  pattern <- "([a-z])([0-9])"
  
  # Basic match
  res <- regex_match(text, pattern)
  expect_length(res, 3)
  expect_equal(res[[1]][1], "a1")
  expect_equal(res[[1]][2], "a")
  expect_equal(res[[1]][3], "1")
  
  # With index extraction
  res_idx <- regex_match(text, pattern, i = 2)
  expect_equal(res_idx, c("a", "b", "c"))
  
  res_idx_3 <- regex_match(text, pattern, i = 3)
  expect_equal(res_idx_3, c("1", "2", "3"))
  
  # Out of bounds index
  res_oob <- regex_match(text, pattern, i = 4)
  expect_true(all(is.na(res_oob)))
})

test_that(".get_ram_gb handles options and system info", {
  # Test mock
  withr::with_options(list(gridmaker.fake_ram = 100), {
    expect_equal(.get_ram_gb("avail"), 100)
    expect_equal(.get_ram_gb()$available, 100)
  })
  
  # Test actual fallback (smoke test)
  skip_on_cran() # PS package behavior might vary
  res <- .get_ram_gb()
  expect_type(res, "list")
  # Expect reasonable keys usually present
  # But won't assert too strictly on system dependent stuff
})

test_that(".estimate_grid_memory_gb calculates reasonable estimates", {
  # Mock environment where we know sizing roughly?
  # Or just smoke test that it returns a number.
  
  grid_extent <- sf::st_bbox(c(xmin = 0, ymin = 0, xmax = 1000, ymax = 1000), crs = 3857)
  
  est <- .estimate_grid_memory_gb(
    grid_extent = grid_extent,
    cellsize_m = 10,
    crs = 3857,
    output_type = "sf_polygons",
    id_format = "both",
    include_llc = TRUE,
    point_type = "centroid"
  )
  
  expect_type(est, "double")
  expect_gte(est, 0)
  
  # Trivial grid
  est_zero <- .estimate_grid_memory_gb(
    grid_extent = sf::st_bbox(c(xmin=0,ymin=0,xmax=1,ymax=1), crs=3857),
    cellsize_m = 100,
    crs = 3857,
    output_type = "sf_polygons",
    id_format = "both",
    include_llc = TRUE,
    point_type = "centroid"
  )
  expect_equal(est_zero, 0)
})

test_that("validate_disk_compatibility rejects invalid combinations", {
  # DataFrame to KEA
  expect_error(
    validate_disk_compatibility("dataframe", "test.kea"),
    "cannot be written to"
  )
  
  # Raster to CSV
  # Raster requires specific extensions
  expect_error(
    validate_disk_compatibility("spatraster", "test.txt"),
    "Unsupported raster format"
  )
  
  # Raster to unknown
  expect_error(
    validate_disk_compatibility("spatraster", "test.xyz"),
    "Unsupported raster format"
  )
  
  # Vector to No-Append format (KML)
  expect_error(
    validate_disk_compatibility("sf_polygons", "test.kml"),
    "does not support appending"
  )
})

test_that(".ext_to_driver maps correctly", {
  expect_equal(.ext_to_driver("tif", "raster"), "GTiff")
  expect_equal(.ext_to_driver("gpkg", "vector"), "GPKG")
  expect_null(.ext_to_driver("xyz", "vector"))
})

test_that(".tz_count counts trailing zeros", {
  expect_equal(.tz_count(100), 2)
  expect_equal(.tz_count(10), 1)
  expect_equal(.tz_count(1), 0)
  expect_equal(.tz_count(0), 0)
  expect_equal(.tz_count(101), 0)
})
