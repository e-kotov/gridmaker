# --- Tests for inspire_id_from_coords ---

test_that("inspire_id_from_coords works with different input types", {
  # 1. Data frame
  coords_df <- data.frame(x = 4400000, y = 3500000)
  id_df <- inspire_id_from_coords(coords_df, cellsize_m = 1000)
  expect_type(id_df, "character")
  expect_equal(length(id_df), 1)

  # 2. Matrix
  coords_mat <- matrix(c(4400000, 3500000), ncol = 2)
  colnames(coords_mat) <- c("x", "y") # Add colnames for consistency
  id_mat <- inspire_id_from_coords(coords_mat, cellsize_m = 1000)
  expect_equal(id_df, id_mat)

  # 3. sf object
  coords_sf <- st_as_sf(coords_df, coords = c("x", "y"), crs = 3035)
  id_sf <- inspire_id_from_coords(coords_sf, cellsize_m = 1000)
  expect_equal(id_df, id_sf)
})

test_that("inspire_id_from_coords produces correct long and short format IDs", {
  coords <- data.frame(x = 4447500, y = 3497500)
  cellsize <- 1000

  # Long format (default)
  expected_long <- "CRS3035RES1000mN3497000E4447000"
  gen_long <- inspire_id_from_coords(coords, cellsize_m = cellsize, llc = FALSE)
  expect_equal(gen_long, expected_long)

  # Short format
  expected_short <- "1kmN3497E4447"
  gen_short <- inspire_id_from_coords(
    coords,
    cellsize_m = cellsize,
    short = TRUE,
    llc = FALSE
  )
  expect_equal(gen_short, expected_short)

  # Test with different resolution (meters)
  coords_100m <- data.frame(x = 4447150, y = 3497250)
  expected_long_100m <- "CRS3035RES100mN3497200E4447100"
  gen_long_100m <- inspire_id_from_coords(coords_100m, cellsize_m = 100, llc = FALSE)
  expect_equal(gen_long_100m, expected_long_100m)

  expected_short_100m <- "100mN34972E44471"
  gen_short_100m <- inspire_id_from_coords(
    coords_100m,
    cellsize_m = 100,
    short = TRUE,
    llc = FALSE
  )
  expect_equal(gen_short_100m, expected_short_100m)
})

test_that("inspire_id_from_coords respects the axis_order argument", {
  coords <- data.frame(x = 4447500, y = 3497500)
  cellsize <- 1000

  # 1. Test short format with 'EN' order
  expected_short_en <- "1kmE4447N3497"
  gen_short_en <- inspire_id_from_coords(
    coords,
    cellsize_m = cellsize,
    short = TRUE,
    axis_order = "EN"
  )
  expect_equal(gen_short_en, expected_short_en)

  # 2. Test that axis_order is ignored for long format
  expected_long <- "CRS3035RES1000mN3497000E4447000"
  gen_long_ignored <- inspire_id_from_coords(
    coords,
    cellsize_m = cellsize,
    short = FALSE, # short is FALSE
    axis_order = "EN" # axis_order should be ignored
  )
  expect_equal(gen_long_ignored, expected_long)
})

test_that("inspire_id_from_coords throws an error for invalid axis_order", {
  coords <- data.frame(x = 4447500, y = 3497500)
  expect_error(
    inspire_id_from_coords(
      coords,
      cellsize_m = 1000,
      short = TRUE,
      axis_order = "XY"
    ),
    regexp = "'arg' should be one of"
  )
})

test_that("inspire_id_from_coords handles different coordinate column names", {
  base_id <- "CRS3035RES10000mN2495000E4095000"
  # Standard lowercase
  coords_xy <- data.frame(x = 4100000, y = 2500000)
  # Uppercase
  coords_XY <- data.frame(X = 4100000, Y = 2500000)
  # Positional (no 'x' or 'y' in names)
  coords_pos <- data.frame(first_col = 4100000, second_col = 2500000)

  expect_equal(inspire_id_from_coords(coords_xy, cellsize_m = 10000), base_id)
  expect_equal(inspire_id_from_coords(coords_XY, cellsize_m = 10000), base_id)
  expect_equal(inspire_id_from_coords(coords_pos, cellsize_m = 10000), base_id)
})

test_that("inspire_id_from_coords resolution guessing works", {
  # Create coordinates with a clear 10km step
  coords <- data.frame(
    x = c(4005000, 4015000, 4025000),
    y = c(3005000, 3005000, 3015000)
  )
  expected_ids <- c(
    "CRS3035RES10000mN3000000E4000000",
    "CRS3035RES10000mN3000000E4010000",
    "CRS3035RES10000mN3010000E4020000"
  )
  # cellsize_m = NULL should trigger guessing
  gen_ids <- inspire_id_from_coords(coords, cellsize_m = NULL)
  expect_equal(gen_ids, expected_ids)

  # Should fail with non-uniform grid distances
  coords_bad <- data.frame(x = c(1, 2, 4), y = c(1, 2, 3))
  expect_error(
    inspire_id_from_coords(coords_bad, cellsize_m = NULL),
    "Coordinates have non-uniform spacing"
  )
})

test_that("inspire_id_from_coords handles vectorization correctly", {
  coords <- data.frame(
    x = c(4334150, 4334250, 4334350),
    y = c(2684050, 2684150, 2684250)
  )
  expected <- c(
    "CRS3035RES100mN2684000E4334100",
    "CRS3035RES100mN2684100E4334200",
    "CRS3035RES100mN2684200E4334300"
  )
  # Specify resolution for a robust test
  generated <- inspire_id_from_coords(coords, cellsize_m = 100)
  expect_equal(generated, expected)
  expect_equal(length(generated), 3)
})

test_that("inspire_id_from_coords handles empty, NULL, and NA inputs gracefully", {
  # Empty data frame
  empty_df <- data.frame(x = numeric(0), y = numeric(0))
  expect_error(
    inspire_id_from_coords(empty_df, cellsize_m = 1000),
    "Input 'coords' cannot be empty"
  )

  # Empty matrix
  empty_mat <- matrix(numeric(0), ncol = 2)
  expect_error(
    inspire_id_from_coords(empty_mat, cellsize_m = 1000),
    "Input 'coords' cannot be empty"
  )

  # NA values in coordinates
  coords_na <- data.frame(
    x = c(4400000, NA, 4500000),
    y = c(3500000, 3600000, NA)
  )
  expect_error(
    inspire_id_from_coords(coords_na, cellsize_m = 1000),
    "contains NA values in the coordinates"
  )
})
