test_that("inspire_grid rejects non-integer cellsize_m", {
  # Mock extent
  ext <- sf::st_bbox(c(xmin=4000000, ymin=3000000, xmax=4050000, ymax=3050000), crs=3035)

  # 1. Non-integer float
  expect_error(
    inspire_grid(ext, cellsize_m = 1500.5, quiet=TRUE),
    "must be an integer"
  )
  
  # 2. Integer float (should pass)
  expect_silent(
    grid <- inspire_grid(ext, cellsize_m = 1000.0, quiet=TRUE)
  )
  expect_s3_class(grid, "sf")
  
  # 3. Negative
  expect_error(
    inspire_grid(ext, cellsize_m = -100, quiet=TRUE),
    "must be positive"
  )
  
  # 4. Zero
  expect_error(
    inspire_grid(ext, cellsize_m = 0, quiet=TRUE),
    "must be positive"
  )
  
  # 5. Non-numeric
  expect_error(
    inspire_grid(ext, cellsize_m = "1000", quiet=TRUE),
    "must be a single numeric value"
  )
})

test_that("inspire_grid validation works across backends", {
  skip_if_not(getOption("gridmaker.test_cpp", TRUE))
  ext <- sf::st_bbox(c(xmin=4000000, ymin=3000000, xmax=4050000, ymax=3050000), crs=3035)
  
  # Force C++ backend
  # Even if parallel=FALSE (sequential), our top-level check should catch it
  expect_error(
    inspire_grid(ext, cellsize_m = 1500.5, vector_grid_backend = "cpp", parallel = FALSE, quiet=TRUE),
    "must be an integer"
  )
  
  # Force Legacy backend
  expect_error(
    inspire_grid(ext, cellsize_m = 1500.5, vector_grid_backend = "sfheaders", parallel = FALSE, quiet=TRUE),
    "must be an integer"
  )
})
