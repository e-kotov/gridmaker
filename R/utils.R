# Helper to provide a default value for NULL objects.
# From rlang::`%||%`
`%||%` <- function(a, b) if (!is.null(a)) a else b

regex_match <- function(text, pattern, i = NULL, ...) {
  match <- regmatches(text, regexec(pattern, text, ...))
  if (!is.null(i)) {
    match <- vapply(match, FUN.VALUE = character(1), function(x) {
      if (length(x) >= i) {
        x[[i]]
      } else {
        NA_character_
      }
    })
  }
  match
}

#' Get memory stats
#' @keywords internal
#' @return A `numeric` amount of available RAM in GB.
#' @noRd
.get_ram_gb <- function(type = NULL) {
  # Allow faking RAM for testing purposes
  fake_ram_gb <- getOption("gridmaker.fake_ram")
  if (!is.null(fake_ram_gb)) {
    if (is.null(type)) {
      return(list(total = 16, available = fake_ram_gb))
    }
    if (type == "available") {
      return(fake_ram_gb)
    }
  }

  mem_info <- ps::ps_system_memory()

  # Convert all values to GB and round to 2 decimal places
  mem_info_gb <- lapply(mem_info, function(x) {
    if (is.numeric(x)) {
      return(round(x / 1024^3, 2))
    }
    return(x) # Keep non-numeric values as-is (like $percent)
  })

  # If type is specified, return just that value
  if (!is.null(type)) {
    if (!type %in% names(mem_info_gb)) {
      # Fallback for systems that may not have 'available'
      if (type == "available" && "free" %in% names(mem_info_gb)) {
        return(mem_info_gb[["free"]])
      }
      return(NULL)
    }
    return(mem_info_gb[[type]])
  }

  # Otherwise return the whole list
  return(mem_info_gb)
}

#' Estimate the memory required to generate a grid in-memory.
#' @keywords internal
#' @return A `numeric` estimate of required memory in GB.
#' @noRd
.estimate_grid_memory_gb <- function(
  grid_extent,
  cellsize_m,
  crs,
  output_type,
  id_format,
  include_llc,
  point_type
) {
  # --- This logic is adapted from create_grid_internal ---
  grid_crs <- if (!is.null(crs)) sf::st_crs(crs) else sf::st_crs(NA)
  if (inherits(grid_extent, c("sf", "sfc", "bbox"))) {
    input_crs <- sf::st_crs(grid_extent)
    if (is.na(grid_crs)) grid_crs <- input_crs
  }

  # A CRS is essential for calculations
  if (is.na(grid_crs)) {
    return(0)
  }

  if (inherits(grid_extent, c("sf", "sfc"))) {
    if (sf::st_crs(grid_extent) != grid_crs) {
      grid_extent <- sf::st_transform(grid_extent, grid_crs)
    }
    bbox <- sf::st_bbox(grid_extent)
  } else if (inherits(grid_extent, "bbox")) {
    bbox <- sf::st_bbox(sf::st_transform(sf::st_as_sfc(grid_extent), grid_crs))
  } else if (is.matrix(grid_extent) && all(dim(grid_extent) == c(2, 2))) {
    bbox <- sf::st_bbox(
      c(
        xmin = grid_extent[1, 1],
        ymin = grid_extent[2, 1],
        xmax = grid_extent[1, 2],
        ymax = grid_extent[2, 2]
      ),
      crs = grid_crs
    )
  } else if (is.numeric(grid_extent) && length(grid_extent) == 4) {
    bbox <- sf::st_bbox(
      c(
        xmin = grid_extent[1],
        ymin = grid_extent[2],
        xmax = grid_extent[3],
        ymax = grid_extent[4]
      ),
      crs = grid_crs
    )
  } else {
    return(0) # Cannot determine bbox
  }
  # --- End of adapted logic ---

  # --- 1. Calculate total number of cells ---
  xmin <- floor(as.numeric(bbox["xmin"]) / cellsize_m) * cellsize_m
  ymin <- floor(as.numeric(bbox["ymin"]) / cellsize_m) * cellsize_m
  xmax <- ceiling(as.numeric(bbox["xmax"]) / cellsize_m) * cellsize_m
  ymax <- ceiling(as.numeric(bbox["ymax"]) / cellsize_m) * cellsize_m

  if (anyNA(c(xmin, ymin, xmax, ymax))) {
    return(0)
  }

  if (xmax <= xmin) {
    xmax <- xmin + cellsize_m
  }
  if (ymax <= ymin) {
    ymax <- ymin + cellsize_m
  }

  num_cols <- (xmax - xmin) / cellsize_m
  num_rows <- (ymax - ymin) / cellsize_m
  total_cells <- num_cols * num_rows

  # Avoid estimation if grid is trivially small or enormous
  if (total_cells == 0) {
    return(0)
  }
  if (!is.finite(total_cells)) {
    return(Inf)
  }

  # --- 2. Empirically determine memory per cell ---
  # Create a tiny extent guaranteed to produce just one cell
  one_cell_extent <- sf::st_bbox(
    c(xmin = 0, ymin = 0, xmax = cellsize_m, ymax = cellsize_m),
    crs = grid_crs
  )

  # Generate that single, representative cell
  one_cell_grid <- create_grid_internal(
    grid_extent = one_cell_extent,
    cellsize_m = cellsize_m,
    output_type = output_type,
    id_format = id_format,
    include_llc = include_llc,
    point_type = point_type
  )

  # Get its size in bytes
  size_per_cell_bytes <- as.numeric(object.size(one_cell_grid))

  # --- 3. Calculate total estimated size in GB ---
  total_memory_bytes <- size_per_cell_bytes * total_cells
  estimated_gb <- total_memory_bytes / (1024^3)

  return(estimated_gb)
}
