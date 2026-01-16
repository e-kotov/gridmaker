
test_that("stream_grid_mirai works for sf_polygons", {
  skip_on_cran()
  
  on.exit({
    mirai::daemons(0)
  }, add = TRUE)
  mirai::daemons(2, dispatcher = FALSE)
  
  # Setup
  tmp_gpkg <- tempfile(fileext = ".gpkg")
  grid_extent <- sf::st_bbox(c(xmin = 0, ymin = 0, xmax = 50, ymax = 50), crs = 3857)
  cellsize <- 10
  
  # Run stream_grid_mirai
  expect_no_error(
    stream_grid_mirai(
      grid_extent = grid_extent,
      cellsize_m = cellsize,
      crs = 3857,
      dsn = tmp_gpkg,
      layer = "grid",
      dot_args = list(output_type = "sf_polygons", id_format = "both"),
      quiet = TRUE
    )
  )
  
  expect_true(file.exists(tmp_gpkg))
  
  # Validate output
  res <- sf::st_read(tmp_gpkg, quiet = TRUE)
  expect_equal(nrow(res), 25)
  expect_true("GRD_ID_LONG" %in% names(res))
  
  unlink(tmp_gpkg)
})

test_that("stream_grid_mirai handles small grids with minimal workers", {
  skip_on_cran()
  
  on.exit({
    mirai::daemons(0)
  }, add = TRUE)
  mirai::daemons(2, dispatcher = FALSE)
  
  tmp_gpkg <- tempfile(fileext = ".gpkg")
  # Very small grid: 2x2
  grid_extent <- sf::st_bbox(c(xmin = 0, ymin = 0, xmax = 20, ymax = 20), crs = 3857)
  
  expect_no_error(
    stream_grid_mirai(
      grid_extent = grid_extent,
      cellsize_m = 10,
      crs = 3857,
      dsn = tmp_gpkg,
      layer = "grid",
      dot_args = list(output_type = "sf_polygons"),
      quiet = TRUE
    )
  )
  
  res <- sf::st_read(tmp_gpkg, quiet = TRUE)
  expect_equal(nrow(res), 4)
  unlink(tmp_gpkg)
})

test_that("stream_grid_mirai errors if daemons not set", {
  skip_on_cran()
  # Ensure no daemons
  mirai::daemons(0)
  
  tmp_gpkg <- tempfile(fileext = ".gpkg")
  grid_extent <- sf::st_bbox(c(xmin = 0, ymin = 0, xmax = 50, ymax = 50), crs = 3857)
  
  expect_error(
    stream_grid_mirai(
      grid_extent = grid_extent,
      cellsize_m = 10,
      crs = 3857,
      dsn = tmp_gpkg,
      layer = "grid",
      dot_args = list(),
      quiet = TRUE
    ),
    "daemons are not running"
  )
})

test_that("stream_grid_mirai works with clipping (convex hull)", {
  skip_on_cran()
  
  on.exit({
    mirai::daemons(0)
  }, add = TRUE)
  mirai::daemons(2, dispatcher = FALSE)
  
  tmp_gpkg <- tempfile(fileext = ".gpkg")
  
  # Create a polygon that covers only part of the bbox
  p1 <- sf::st_polygon(list(rbind(c(0,0), c(50,0), c(0,50), c(0,0))))
  p1_sfc <- sf::st_sfc(p1, crs = 3857)
  
  # Bbox of p1 is 0,0,50,50
  # Triangle covers half of it
  
  expect_no_error(
    stream_grid_mirai(
      grid_extent = p1_sfc, 
      cellsize_m = 10,
      crs = 3857,
      dsn = tmp_gpkg,
      layer = "grid",
      dot_args = list(
        output_type = "sf_polygons",
        clip_to_input = TRUE,
        use_convex_hull = FALSE # p1 is already convex-ish, but let's just use it as is
      ),
      quiet = TRUE
    )
  )
  
  res <- sf::st_read(tmp_gpkg, quiet = TRUE)
  # Total cells in 50x50 at 10m is 25.
  # Triangle should intersect fewer.
  expect_lt(nrow(res), 25)
  expect_gt(nrow(res), 0)
  
  unlink(tmp_gpkg)
})
