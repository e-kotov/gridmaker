
test_that("build_spatial_index argument controls index creation", {
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("DBI")
  skip_if_not_installed("sf")
  
  # Setup
  tmp_dir <- tempdir()
  # Create a small bbox in EPSG:3035
  bbox <- sf::st_bbox(c(xmin=4000000, ymin=3000000, xmax=4050000, ymax=3050000), crs=sf::st_crs(3035))
  
  # Helper to check for rtree tables
  # Note: GPKG spatial index uses rtree tables (usually rtree_<table_name>_<geom_col>)
  has_index <- function(f) {
    con <- DBI::dbConnect(RSQLite::SQLite(), f)
    on.exit(DBI::dbDisconnect(con))
    tabs <- DBI::dbListTables(con)
    any(grepl("rtree_", tabs))
  }
  
  # Case 1: Default (TRUE) - Should have index
  f1 <- file.path(tmp_dir, "idx_default.gpkg")
  if (file.exists(f1)) unlink(f1)
  
  expect_silent(
    inspire_grid(bbox, cellsize_m = 10000, dsn = f1, quiet = TRUE, layer="grid")
  )
  
  expect_true(has_index(f1), "Default behavior should create spatial index")
  
  # Case 2: Explicit FALSE - Should NOT have index & Should message (if quiet=FALSE)
  f2 <- file.path(tmp_dir, "idx_false.gpkg")
  if (file.exists(f2)) unlink(f2)
  

  
  # Capture all messages because inspire_grid emits multiple messages (parallel detection, progress, etc.)
  msgs <- capture.output({
    suppressWarnings(
      inspire_grid(bbox, cellsize_m = 10000, dsn = f2, quiet = FALSE, layer="grid", build_spatial_index = FALSE)
    )
  }, type = "message")
  
  expect_true(any(grepl("Spatial index disabled", msgs)), "Should warn about disabled spatial index")
  
  expect_false(has_index(f2), "build_spatial_index=FALSE should NOT create spatial index")
  
  # Case 3: Explicit FALSE with Quiet=TRUE - Should NOT message
  f3 <- file.path(tmp_dir, "idx_quiet.gpkg")
  if (file.exists(f3)) unlink(f3)
  
  expect_silent(
    inspire_grid(bbox, cellsize_m = 10000, dsn = f3, quiet = TRUE, layer="grid", build_spatial_index = FALSE)
  )
  expect_false(has_index(f3), "build_spatial_index=FALSE should NOT create spatial index (quiet mode)")
  
})
