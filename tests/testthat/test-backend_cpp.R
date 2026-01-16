
test_that("C++ Backend produces identical output to Default (Comprehensive)", {
  skip_if_not(getOption("gridmaker.test_cpp", TRUE))
  ext <- c(xmin=0, ymin=0, xmax=5000, ymax=5000)
  bbox <- sf::st_bbox(ext, crs = sf::st_crs(3035))
  cellsize <- 1000

  # Define all configurations to test
  test_configs <- list(
    list(fmt = "both", llc = TRUE),
    list(fmt = "long", llc = TRUE),
    list(fmt = "short", llc = TRUE),
    list(fmt = "none", llc = TRUE),
    list(fmt = "both", llc = FALSE),
    list(fmt = "long", llc = FALSE),
    list(fmt = "short", llc = FALSE),
    list(fmt = "none", llc = FALSE)
  )

  for (cfg in test_configs) {
    desc <- paste0("id_format='", cfg$fmt, "', include_llc=", cfg$llc)
    
    # 1. Generate Default (Explicit)
    grid_default <- inspire_grid(
      bbox, 
      cellsize, 
      output_type = "sf_polygons", 
      id_format = cfg$fmt, 
      include_llc = cfg$llc,
      vector_grid_backend = "sfheaders"
    )
    
    # 2. Generate C++ (Explicit)
    grid_cpp <- inspire_grid(
      bbox, 
      cellsize, 
      output_type = "sf_polygons", 
      id_format = cfg$fmt, 
      include_llc = cfg$llc,
      vector_grid_backend = "cpp"
    )
    
    # Compare Default vs CPP
    expect_equal(class(grid_default), class(grid_cpp), info = desc)
    expect_equal(sf::st_crs(grid_default), sf::st_crs(grid_cpp), info = desc)
    expect_identical(names(grid_default), names(grid_cpp), info = desc)
    
    # Sort/Align for comparison
    cols_to_sort_by <- names(grid_default)[names(grid_default) != "geometry"]
    
    if (length(cols_to_sort_by) > 0) {
      # Use available columns. 
      # Prefer GRD_ID_LONG, then SHORT, then X_LLC
      sort_col <- NULL
      if ("GRD_ID_LONG" %in% names(grid_default)) sort_col <- "GRD_ID_LONG"
      else if ("GRD_ID_SHORT" %in% names(grid_default)) sort_col <- "GRD_ID_SHORT"
      else if ("X_LLC" %in% names(grid_default)) sort_col <- "X_LLC"
      
      if (!is.null(sort_col)) {
        # Get data frame without geometry for sorting to avoid ordering by geometry (which fails)
        df_def <- sf::st_drop_geometry(grid_default)
        df_cpp <- sf::st_drop_geometry(grid_cpp)
        
        # Ensure we only use the columns intended for sorting (and strictly all of them for stable sort if possible)
        # Using all attribute columns is safest.
        df_def <- df_def[cols_to_sort_by]
        df_cpp <- df_cpp[cols_to_sort_by]
        
        ord_def <- do.call(order, as.list(df_def))
        ord_cpp <- do.call(order, as.list(df_cpp))
        
        grid_default <- grid_default[ord_def, ]
        grid_cpp <- grid_cpp[ord_cpp, ]
      }
    } else {
      # Fallback: Sort by geometry centroid if no attributes
      suppressWarnings({
        coords_default <- sf::st_coordinates(sf::st_centroid(grid_default))
        coords_cpp <- sf::st_coordinates(sf::st_centroid(grid_cpp))
      })
      
      # Sort by X then Y
      ord_def <- order(coords_default[,1], coords_default[,2])
      ord_cpp <- order(coords_cpp[,1], coords_cpp[,2])
      
      grid_default <- grid_default[ord_def, ]
      grid_cpp <- grid_cpp[ord_cpp, ]
    }

    # Reset row names
    rownames(grid_default) <- NULL
    rownames(grid_cpp) <- NULL
    
    # Check data columns
    for (col in cols_to_sort_by) {
      expect_equal(grid_default[[col]], grid_cpp[[col]], info = paste(desc, "Column:", col))
    }
    
    # Check Geometry matches
    matches <- sf::st_equals(grid_default, grid_cpp, sparse = FALSE)
    all_match <- all(diag(matches)) && nrow(matches) == ncol(matches)
    
    expect_true(all_match, label = paste(desc, "- Geometries are topologically equal"))
    
    is_exact <- sf::st_equals_exact(grid_default, grid_cpp, par = 1e-9, sparse = FALSE)
    all_exact <- all(diag(is_exact)) && nrow(is_exact) == ncol(is_exact)
    
    expect_true(all_exact, label = paste(desc, "- Geometries are exactly equal (tolerance 1e-9)"))
  }
  
  # Also re-verify the implicit option one last time with default args
  withr::with_options(list(gridmaker.vector_grid_backend = "cpp"), {
    grid_cpp_opt <- inspire_grid(
      bbox, 
      cellsize, 
      output_type = "sf_polygons", 
      id_format = "both", 
      include_llc = TRUE
    )
    
    # Generate explicit CPP to match
    grid_cpp_explicit <- inspire_grid(
      bbox, 
      cellsize, 
      output_type = "sf_polygons", 
      id_format = "both", 
      include_llc = TRUE,
      vector_grid_backend = "cpp"
    )
    
    # Just check identity of implicit vs explicit
    grid_cpp_opt <- grid_cpp_opt[order(grid_cpp_opt$GRD_ID_LONG), ]
    grid_cpp_explicit <- grid_cpp_explicit[order(grid_cpp_explicit$GRD_ID_LONG), ]
    rownames(grid_cpp_opt) <- NULL
    rownames(grid_cpp_explicit) <- NULL
    
    expect_equal(grid_cpp_opt, grid_cpp_explicit, info = "Implicit Option vs Explicit Backend")
  })
})
