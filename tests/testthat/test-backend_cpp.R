
test_that("C++ Backend produces identical output to Default", {
  skip_if_not(getOption("gridmaker.test_cpp", TRUE)) # Allow disabling if compilation fails in some envs?
  # But we expect it to work if package loaded.
  
  ext <- c(xmin=0, ymin=0, xmax=5000, ymax=5000)
  bbox <- sf::st_bbox(ext, crs = sf::st_crs(3035))
  cellsize <- 1000

  # 1. Generate Default (Explicit)
  grid_default <- inspire_grid(
    bbox, 
    cellsize, 
    output_type = "sf_polygons", 
    id_format = "both", 
    include_llc = TRUE,
    vector_grid_backend = "sfheaders"
  )

  # 2. Generate C++ (Explicit)
  grid_cpp <- inspire_grid(
    bbox, 
    cellsize, 
    output_type = "sf_polygons", 
    id_format = "both", 
    include_llc = TRUE,
    vector_grid_backend = "cpp"
  )

  # 3. Generate C++ (Implicit via Option)
  withr::with_options(list(gridmaker.vector_grid_backend = "cpp"), {
    grid_cpp_opt <- inspire_grid(
      bbox, 
      cellsize, 
      output_type = "sf_polygons", 
      id_format = "both", 
      include_llc = TRUE
    )
  })

  # 4. Compare
  # Compare Default vs CPP
  expect_equal(class(grid_default), class(grid_cpp))
  expect_equal(sf::st_crs(grid_default), sf::st_crs(grid_cpp))
  expect_identical(names(grid_default), names(grid_cpp))

  # Sort by ID to ensure alignment
  grid_default <- grid_default[order(grid_default$GRD_ID_LONG), ]
  grid_cpp <- grid_cpp[order(grid_cpp$GRD_ID_LONG), ]
  grid_cpp_opt <- grid_cpp_opt[order(grid_cpp_opt$GRD_ID_LONG), ]
  
  # Reset row names
  rownames(grid_default) <- NULL
  rownames(grid_cpp) <- NULL
  rownames(grid_cpp_opt) <- NULL

  expect_equal(grid_default$GRD_ID_LONG, grid_cpp$GRD_ID_LONG)
  expect_equal(grid_default$GRD_ID_SHORT, grid_cpp$GRD_ID_SHORT)
  expect_equal(grid_default$X_LLC, grid_cpp$X_LLC)
  expect_equal(grid_default$Y_LLC, grid_cpp$Y_LLC)

  # Verify Implicit Option matches Explicit Argument
  expect_equal(grid_cpp, grid_cpp_opt)

  # Check Geometry matches
  # Use st_equals for topological equality
  matches <- sf::st_equals(grid_default, grid_cpp, sparse = FALSE)
  all_match <- all(diag(matches)) && nrow(matches) == ncol(matches)
  
  expect_true(all_match, label = "Geometries are topologically equal")
  
  # Use st_equals_exact for strict coordinate equality (tolerance 1e-9)
  is_exact <- sf::st_equals_exact(grid_default, grid_cpp, par = 1e-9, sparse = FALSE)
  all_exact <- all(diag(is_exact)) && nrow(is_exact) == ncol(is_exact)
  
  expect_true(all_exact, label = "Geometries are exactly equal (tolerance 1e-9)")
})
