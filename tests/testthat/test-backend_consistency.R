test_that("C++ vs sfheaders consistency for all output types", {
  skip_if_not_installed("sfheaders")
  
  ext <- c(xmin=0, ymin=0, xmax=2000, ymax=2000)
  bbox <- sf::st_bbox(ext, crs = sf::st_crs(3035))
  cellsize <- 1000
  
  types <- c("sf_polygons", "sf_points", "dataframe")
  
  for (type in types) {
    # Generate with sfheaders backend
    grid_sfh <- inspire_grid(
      bbox, 
      cellsize, 
      output_type = type, 
      id_format = "both", 
      vector_grid_backend = "sfheaders",
      quiet = TRUE
    )
    
    # Generate with cpp backend
    grid_cpp <- inspire_grid(
      bbox, 
      cellsize, 
      output_type = type, 
      id_format = "both", 
      vector_grid_backend = "cpp",
      quiet = TRUE
    )
    
    # Fundamental checks
    expect_equal(class(grid_sfh), class(grid_cpp), info = paste("Type:", type))
    expect_equal(nrow(grid_sfh), nrow(grid_cpp), info = paste("Type:", type))
    expect_identical(names(grid_sfh), names(grid_cpp), info = paste("Type:", type))
    
    # Sort and reset row names for robust comparison
    grid_sfh <- grid_sfh[order(grid_sfh$GRD_ID_LONG), ]
    grid_cpp <- grid_cpp[order(grid_cpp$GRD_ID_LONG), ]
    rownames(grid_sfh) <- NULL
    rownames(grid_cpp) <- NULL
    
    # Attribute checks
    expect_equal(grid_sfh$GRD_ID_LONG, grid_cpp$GRD_ID_LONG, info = paste("Type:", type))
    expect_equal(grid_sfh$GRD_ID_SHORT, grid_cpp$GRD_ID_SHORT, info = paste("Type:", type))
    
    # Spatial checks for sf types
    if (inherits(grid_sfh, "sf")) {
      expect_equal(sf::st_crs(grid_sfh), sf::st_crs(grid_cpp), info = paste("Type:", type))
      # Geometries should be equal (use st_equals for topological check)
      matches <- sf::st_equals(grid_sfh, grid_cpp, sparse = FALSE)
      expect_true(all(diag(matches)), info = paste("Geometries mismatch for type:", type))
    }
    
    # LLC consistency (if present)
    if ("X_LLC" %in% names(grid_sfh)) {
      expect_equal(grid_sfh$X_LLC, grid_cpp$X_LLC, info = paste("Type:", type))
      expect_equal(grid_sfh$Y_LLC, grid_cpp$Y_LLC, info = paste("Type:", type))
    }
  }
})

test_that("C++ vs sfheaders consistency for different projected CRS", {
  skip_if_not_installed("sfheaders")
  
  # EPSG:25832 (UTM zone 32N)
  ext_utm <- c(xmin=500000, ymin=5800000, xmax=502000, ymax=5802000)
  bbox_utm <- sf::st_bbox(ext_utm, crs = sf::st_crs(25832))
  cellsize <- 1000
  
  # Default/sfheaders
  grid_sfh <- inspire_grid(
    bbox_utm, 
    cellsize, 
    vector_grid_backend = "sfheaders",
    id_format = "long",
    quiet = TRUE
  )
  
  # C++
  grid_cpp <- inspire_grid(
    bbox_utm, 
    cellsize, 
    vector_grid_backend = "cpp",
    id_format = "long",
    quiet = TRUE
  )
  
  expect_equal(sf::st_crs(grid_cpp)$epsg, 25832)
  expect_match(grid_cpp$GRD_ID[1], "CRS25832")
  expect_equal(grid_cpp$GRD_ID, grid_sfh$GRD_ID)
  
  # Geometries
  matches <- sf::st_equals(grid_sfh, grid_cpp, sparse = FALSE)
  expect_true(all(diag(matches)))
})
