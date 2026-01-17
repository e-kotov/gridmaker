test_that("Legacy sfheaders backend works when explicitly requested", {
  skip_if_not_installed("sfheaders")
  
  ext <- c(xmin=0, ymin=0, xmax=2000, ymax=2000)
  bbox <- sf::st_bbox(ext, crs = sf::st_crs(3035))
  cellsize <- 1000
  
  # Explicitly request sfheaders
  grid_legacy <- inspire_grid(
    bbox, 
    cellsize, 
    output_type = "sf_polygons", 
    vector_grid_backend = "sfheaders",
    quiet = TRUE
  )
  
  expect_s3_class(grid_legacy, "sf")
  expect_equal(nrow(grid_legacy), 4)
  expect_equal(grid_legacy$GRD_ID_LONG[1], "CRS3035RES1000mN0E0")
})

test_that("Legacy backend handles clipping via sfheaders path", {
  skip_if_not_installed("sfheaders")
  
  ext <- c(xmin=0, ymin=0, xmax=2000, ymax=2000)
  bbox <- sf::st_bbox(ext, crs = sf::st_crs(3035))
  cellsize <- 1000
  
  # Create a clipping target (bottom-left cell only)
  poly <- sf::st_polygon(list(matrix(c(100,100, 900,100, 900,900, 100,900, 100,100), ncol=2, byrow=TRUE)))
  target <- sf::st_sfc(poly, crs = 3035)
  
  # This triggers the as_inspire_grid path in backend_sequential.R
  # which uses sfheaders for internal clipping preview
  grid_clipped <- inspire_grid(
    target, 
    cellsize, 
    output_type = "sf_polygons", 
    vector_grid_backend = "sfheaders",
    clip_to_input = TRUE,
    quiet = TRUE
  )
  
  expect_equal(nrow(grid_clipped), 1)
  expect_equal(grid_clipped$GRD_ID_LONG, "CRS3035RES1000mN0E0")
})
