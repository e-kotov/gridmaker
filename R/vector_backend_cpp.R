as_inspire_grid_cpp <- function(x_llc, y_llc, cellsize, crs, axis_order, id_format, include_llc = TRUE, generate_ids = TRUE) {
  
  # Extract EPSG for ID generation
  # Try specific field first, then strict integer
  crs_obj <- sf::st_crs(crs)
  epsg <- crs_obj$epsg
  if (is.null(epsg) || is.na(epsg)) {
    # If CRS was passed as integer, use it
    if (is.numeric(crs)) {
      epsg <- as.integer(crs)
    } else {
      # Fallback or try to parse 'EPSG:3035' string?
      # For now default to 3035 as per original logic if missing
      epsg <- 3035 
    }
  }

  # Helpers for ID generation (reusing internal logic)
  nzeros <- .tz_count(cellsize)
  div <- 10^nzeros
  size_lbl <- if (cellsize >= 1000) paste0(cellsize / 1000, "km") else paste0(cellsize, "m")

  # Call C++ Kernel
  # This returns list(geometry, id_long, id_short)
  res <- grid_worker_rcpp(
    x_llc = x_llc, 
    y_llc = y_llc, 
    cellsize = as.double(cellsize),
    epsg = as.integer(epsg),
    size_lbl = size_lbl,
    divider = as.double(div),
    axis_order = axis_order,
    id_format = id_format,
    generate_ids = generate_ids
  )

  # --- Fast SF Construction ---
  
  # 1. Geometry Column (sfc)
  sfg_list <- res$geometry
  
  # fast bbox (vectors are N length)
  bb <- c(
    xmin = min(x_llc, na.rm = TRUE), 
    ymin = min(y_llc, na.rm = TRUE), 
    xmax = max(x_llc, na.rm = TRUE) + cellsize, 
    ymax = max(y_llc, na.rm = TRUE) + cellsize
  )
  class(bb) <- "bbox"
  attr(bb, "crs") <- crs_obj

  attr(sfg_list, "class") <- c("sfc_POLYGON", "sfc")
  attr(sfg_list, "precision") <- 0
  attr(sfg_list, "bbox") <- bb
  attr(sfg_list, "crs") <- crs_obj
  attr(sfg_list, "n_empty") <- 0L
  
  # 2. Data Frame Assembly
  # Use list structure directly to avoid data.frame() overhead
  out_list <- list()
  
  if (include_llc) {
      out_list$X_LLC <- x_llc
      out_list$Y_LLC <- y_llc
  }
  
  if (generate_ids) {
      if (id_format == "both") {
          out_list$GRD_ID_LONG <- res$id_long
          out_list$GRD_ID_SHORT <- res$id_short
      } else if (id_format == "long") {
          out_list$GRD_ID <- res$id_long
      } else if (id_format == "short") {
          out_list$GRD_ID <- res$id_short
      }
  }
  
  out_list$geometry <- sfg_list
  
  # 3. Class Assignment
  attr(out_list, "row.names") <- .set_row_names(length(x_llc))
  attr(out_list, "class") <- c("sf", "data.frame")
  attr(out_list, "sf_column") <- "geometry"
  
  out_list
}
