test_that("grid_worker_rcpp handles custom CRS (missing EPSG) correctly", {
  
  # Setup inputs
  x_llc <- c(100)
  y_llc <- c(100)
  cellsize <- 100
  epsg_na <- NA_integer_
  epsg_valid <- 3035L
  
  # 1. Custom CRS (EPSG=NA) + id_format="long" -> Should ERROR
  # Because "long" format requires a valid numeric EPSG code for the "CRS...RES..." string.
  expect_error(
    gridmaker:::grid_worker_rcpp(
      x_llc = x_llc, y_llc = y_llc, cellsize = cellsize,
      epsg = epsg_na, size_lbl = "100m", divider = 1,
      axis_order = "NE", id_format = "long", generate_ids = TRUE
    ),
    "epsg must be a valid integer code when generating long-format IDs"
  )
  
  # 2. Custom CRS (EPSG=NA) + id_format="both" -> Should ERROR
  # "both" implies "long", so same requirement applies.
  expect_error(
    gridmaker:::grid_worker_rcpp(
      x_llc = x_llc, y_llc = y_llc, cellsize = cellsize,
      epsg = epsg_na, size_lbl = "100m", divider = 1,
      axis_order = "NE", id_format = "both", generate_ids = TRUE
    ),
    "epsg must be a valid integer code when generating long-format IDs"
  )

  # 3. Custom CRS (EPSG=NA) + id_format="short" -> Should PASS
  # Short IDs ("N...E...") do not use EPSG code, so this should work.
  expect_no_error(
    res_short <- gridmaker:::grid_worker_rcpp(
      x_llc = x_llc, y_llc = y_llc, cellsize = cellsize,
      epsg = epsg_na, size_lbl = "100m", divider = 1,
      axis_order = "NE", id_format = "short", generate_ids = TRUE
    )
  )
  expect_true(length(res_short$id_short) == 1)
  expect_true(all(nzchar(res_short$id_short)))

  # 4. Custom CRS (EPSG=NA) + generate_ids=FALSE -> Should PASS
  # No IDs generated, so no CRS string needed.
  expect_no_error(
    res_noids <- gridmaker:::grid_worker_rcpp(
      x_llc = x_llc, y_llc = y_llc, cellsize = cellsize,
      epsg = epsg_na, size_lbl = "100m", divider = 1,
      axis_order = "NE", id_format = "none", generate_ids = FALSE
    )
  )
  expect_equal(length(res_noids), 3) # Returns list(geometry, id_long, id_short) always

  # 5. Valid CRS (EPSG=3035) -> Should PASS all
  expect_no_error(
    gridmaker:::grid_worker_rcpp(
      x_llc = x_llc, y_llc = y_llc, cellsize = cellsize,
      epsg = epsg_valid, size_lbl = "100m", divider = 1,
      axis_order = "NE", id_format = "long", generate_ids = TRUE
    )
  )
})

test_that("generate_ids_rcpp handles custom CRS (missing EPSG) correctly", {
  
  # Setup inputs
  x_llc <- c(100)
  y_llc <- c(100)
  cellsize <- 100
  epsg_na <- NA_integer_
  
  # 1. Custom CRS + id_format="long" -> Should ERROR
  expect_error(
    gridmaker:::generate_ids_rcpp(
      x_llc = x_llc, y_llc = y_llc, cellsize = cellsize,
      epsg = epsg_na, size_lbl = "100m", divider = 1,
      axis_order = "NE", id_format = "long"
    ),
    "epsg must be a valid integer code when generating long-format IDs"
  )

  # 2. Custom CRS + id_format="short" -> Should PASS
  expect_no_error(
    res <- gridmaker:::generate_ids_rcpp(
      x_llc = x_llc, y_llc = y_llc, cellsize = cellsize,
      epsg = epsg_na, size_lbl = "100m", divider = 1,
      axis_order = "NE", id_format = "short"
    )
  )
  expect_true(length(res$id_short) == 1)
})
