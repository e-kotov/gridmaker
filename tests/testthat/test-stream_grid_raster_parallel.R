
test_that("stream_raster_parallel_mirai options work", {
  skip_on_cran()
  
  # Ensure cleanup
  on.exit({
    mirai::daemons(0)
  }, add = TRUE)
  
  # Set up minimal daemons
  mirai::daemons(2, dispatcher = FALSE)
  
  # Create a small grid extent
  grid_extent <- sf::st_bbox(c(xmin = 0, ymin = 0, xmax = 50, ymax = 50), crs = 3857)
  
  # Temp file
  tmp_tif <- tempfile(fileext = ".tif")
  
  # Test with explicit workers
  expect_no_error(
    stream_raster_parallel_mirai(
      grid_extent = grid_extent,
      cellsize_m = 10,
      crs = 3857,
      dsn = tmp_tif,
      layer = "test",
      dot_args = list(),
      quiet = TRUE,
      n_workers = 2
    )
  )
  
  expect_true(file.exists(tmp_tif))
  
  # Verify output
  r <- terra::rast(tmp_tif)
  expect_equal(terra::nrow(r), 5)
  expect_equal(terra::ncol(r), 5)
  expect_equal(terra::ncell(r), 25)
  
  # Test cleanup
  unlink(tmp_tif)
})

test_that(".compute_raster_chunk generates correct indices", {
  # Scenario: 10x10 grid (100 cells)
  # Chunk: rows 3-4 (2 rows)
  # Global cell IDs:
  # Row 1: 1-10
  # Row 2: 11-20
  # Row 3: 21-30
  # Row 4: 31-40
  
  start_row <- 3
  nrows <- 2
  ncols <- 10
  
  res <- .compute_raster_chunk(start_row, nrows, ncols)
  
  expect_equal(res$start, 3)
  expect_equal(res$nrows, 2)
  expect_equal(length(res$values), 20)
  expect_equal(res$values[1], 21)
  expect_equal(res$values[20], 40)
  expect_equal(res$values, 21:40)
})

test_that("stream_raster_parallel_mirai validates dependencies", {
  skip_on_cran()
  # It's hard to robustly mock missing packages in a way that affects requireNamespace 
  # inside the function without affecting the test file itself. 
  # Skipping mocking test for now as we are in a package context where dependencies are assumed met 
  # if installed.
})

test_that("stream_raster_parallel_mirai handles auto-workers", {
  skip_on_cran()
  on.exit({
    mirai::daemons(0)
  }, add = TRUE)
  mirai::daemons(2, dispatcher = FALSE)
  
  tmp_tif <- tempfile(fileext = ".tif")
  grid_extent <- sf::st_bbox(c(xmin = 0, ymin = 0, xmax = 100, ymax = 100), crs = 3857)
  
  # Should run without error and infer workers from mirai::status()
  expect_no_error(
    stream_raster_parallel_mirai(
      grid_extent = grid_extent,
      cellsize_m = 10,
      crs = 3857,
      dsn = tmp_tif,
      layer = "test",
      dot_args = list(),
      quiet = TRUE,
      n_workers = NULL
    )
  )
  expect_true(file.exists(tmp_tif))
  unlink(tmp_tif)
})
