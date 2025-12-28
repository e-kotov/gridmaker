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
    if (type == "avail") {
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
      if (type == "avail" && "free" %in% names(mem_info_gb)) {
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
  grid_crs <- if (!is.null(crs)) sf::st_crs(crs) else sf::st_crs(grid_extent)
  if (is.na(grid_crs)) {
    return(0)
  }

  bbox <- .get_bbox_from_grid_extent(grid_extent, grid_crs)

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
  if (total_cells <= 1) {
    return(0)
  }
  if (!is.finite(total_cells)) {
    return(Inf)
  }

  # --- 2. Empirically determine memory per ADDITIONAL cell ---
  # To get an accurate per-cell cost, we measure the memory difference
  # between two small grids. This method calculates the slope of memory
  # growth, effectively removing the fixed overhead of the sf object structure
  # that skewed the previous one-cell estimation method.

  # Define the number of cells for our two sample points.
  n1 <- 10
  n2 <- 20

  # Create the first, smaller sample grid.
  sample_extent_1 <- sf::st_bbox(
    c(xmin = 0, ymin = 0, xmax = n1 * cellsize_m, ymax = cellsize_m),
    crs = grid_crs
  )
  sample_grid_1 <- inspire_grid_from_extent_internal(
    grid_extent = sample_extent_1,
    cellsize_m = cellsize_m,
    output_type = output_type,
    id_format = id_format,
    axis_order = "NE",
    include_llc = include_llc,
    point_type = point_type
  )
  size1 <- as.numeric(utils::object.size(sample_grid_1))

  # Create the second, larger sample grid.
  sample_extent_2 <- sf::st_bbox(
    c(xmin = 0, ymin = 0, xmax = n2 * cellsize_m, ymax = cellsize_m),
    crs = grid_crs
  )
  sample_grid_2 <- inspire_grid_from_extent_internal(
    grid_extent = sample_extent_2,
    cellsize_m = cellsize_m,
    output_type = output_type,
    id_format = id_format,
    axis_order = "NE",
    include_llc = include_llc,
    point_type = point_type
  )
  size2 <- as.numeric(utils::object.size(sample_grid_2))

  # Calculate the memory cost per additional cell (the slope).
  # Add a small epsilon to avoid division by zero if sizes are identical.
  size_per_additional_cell <- (size2 - size1) / (n2 - n1 + 1e-9)

  # --- 3. Calculate total estimated size in GB ---
  # Project the total memory using the accurate per-cell cost.
  # We add a safety factor because this estimates the final object size, and
  # peak memory allocation during the function run might be slightly higher.
  safety_factor <- 1.25
  total_memory_bytes <- (size_per_additional_cell * total_cells) * safety_factor
  estimated_gb <- total_memory_bytes / (1024^3)

  return(estimated_gb)
}

.get_bbox_from_grid_extent <- function(grid_extent, crs = NULL) {
  grid_crs <- if (!is.null(crs)) sf::st_crs(crs) else sf::st_crs(NA)
  if (inherits(grid_extent, c("sf", "sfc", "bbox"))) {
    input_crs <- sf::st_crs(grid_extent)
    if (is.na(grid_crs)) grid_crs <- input_crs
  }

  # A CRS is essential for calculations
  if (is.na(grid_crs)) {
    stop(
      "CRS is missing and cannot be derived from 'grid_extent'.",
      call. = FALSE
    )
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
    if (all(c("xmin", "ymin", "xmax", "ymax") %in% names(grid_extent))) {
      bbox <- sf::st_bbox(
        c(
          xmin = grid_extent["xmin"],
          ymin = grid_extent["ymin"],
          xmax = grid_extent["xmax"],
          ymax = grid_extent["ymax"]
        ),
        crs = grid_crs
      )
    } else {
      bbox <- sf::st_bbox(
        c(
          xmin = grid_extent[1],
          ymin = grid_extent[2],
          xmax = grid_extent[3],
          ymax = grid_extent[4]
        ),
        crs = grid_crs
      )
    }
  } else {
    stop("Invalid 'grid_extent' format.", call. = FALSE)
  }
  return(bbox)
}

#' Calculate the optimal number of rows per chunk based on memory constraints.
#' @keywords internal
#' @return A `numeric` value for the number of rows per chunk.
#' @noRd
.calculate_rows_per_chunk <- function(
  grid_extent,
  cellsize_m,
  crs,
  dot_args,
  max_memory_gb = NULL
) {
  # --- 1. Determine the memory limit ---
  # If user provides a limit, use it. Otherwise, use 50% of available RAM.
  limit_gb <- if (!is.null(max_memory_gb)) {
    max_memory_gb
  } else {
    available_gb <- .get_ram_gb("available")
    if (is.null(available_gb) || is.na(available_gb) || available_gb == 0) {
      # Fallback to a safe default of 1 GB if RAM can't be determined
      1
    } else {
      # Use 50% of available RAM as a safe default
      floor(available_gb * 0.5)
    }
  }
  limit_bytes <- limit_gb * (1024^3)

  # --- 2. Estimate memory usage per cell (Marginal Cost) ---
  # We use the same slope-based method as .estimate_grid_memory_gb
  # to avoid overestimating due to fixed sf overhead.

  grid_crs <- if (!is.null(crs)) sf::st_crs(crs) else sf::st_crs(grid_extent)

  # Define sample sizes
  n1 <- 10
  n2 <- 20

  # Create sample 1
  sample_extent_1 <- sf::st_bbox(
    c(xmin = 0, ymin = 0, xmax = n1 * cellsize_m, ymax = cellsize_m),
    crs = grid_crs
  )
  sample_grid_1 <- inspire_grid_from_extent_internal(
    grid_extent = sample_extent_1,
    cellsize_m = cellsize_m,
    output_type = dot_args$output_type %||% "sf_polygons",
    id_format = dot_args$id_format %||% "both",
    axis_order = dot_args$axis_order %||% "NE",
    include_llc = dot_args$include_llc %||% TRUE,
    point_type = dot_args$point_type %||% "centroid"
  )
  size1 <- as.numeric(utils::object.size(sample_grid_1))

  # Create sample 2
  sample_extent_2 <- sf::st_bbox(
    c(xmin = 0, ymin = 0, xmax = n2 * cellsize_m, ymax = cellsize_m),
    crs = grid_crs
  )
  sample_grid_2 <- inspire_grid_from_extent_internal(
    grid_extent = sample_extent_2,
    cellsize_m = cellsize_m,
    output_type = dot_args$output_type %||% "sf_polygons",
    id_format = dot_args$id_format %||% "both",
    axis_order = dot_args$axis_order %||% "NE",
    include_llc = dot_args$include_llc %||% TRUE,
    point_type = dot_args$point_type %||% "centroid"
  )
  size2 <- as.numeric(utils::object.size(sample_grid_2))

  # Calculate marginal bytes per cell
  bytes_per_cell <- (size2 - size1) / (n2 - n1 + 1e-9)

  # If the slope is non-positive (unlikely but possible with weird overheads),
  # fallback to average size of the larger sample
  if (bytes_per_cell <= 0) {
    bytes_per_cell <- size2 / n2
  }

  # --- 3. Calculate grid dimensions ---
  bbox <- .get_bbox_from_grid_extent(grid_extent, grid_crs)
  xmin <- floor(as.numeric(bbox["xmin"]) / cellsize_m) * cellsize_m
  xmax <- ceiling(as.numeric(bbox["xmax"]) / cellsize_m) * cellsize_m
  n_cols <- ceiling((xmax - xmin) / cellsize_m)

  if (n_cols == 0) {
    return(1) # Grid has no width
  }

  # --- 4. Determine rows per chunk ---
  # Calculate how many cells can fit in the memory limit
  # We add a safety buffer (e.g. 80% of limit) to account for overhead
  max_cells_in_memory <- floor((limit_bytes * 0.8) / bytes_per_cell)

  # Calculate how many rows that corresponds to
  rows_per_chunk <- floor(max_cells_in_memory / n_cols)

  # Check if user provided an explicit override via ... (dot_args)
  # Note: The calling functions pass '...' as 'dot_args'
  if (!is.null(dot_args$max_cells_per_chunk)) {
    user_rows <- floor(dot_args$max_cells_per_chunk / n_cols)
    if (user_rows > 0) {
      rows_per_chunk <- min(rows_per_chunk, user_rows)
    }
  }

  # Ensure at least one row is processed at a time
  if (rows_per_chunk == 0) {
    # If even one row is too big, we have to do 1 row.
    # But we should warn if it's extremely tight.
    rows_per_chunk <- 1
  }

  return(rows_per_chunk)
}

#' Validate if output_type and DSN extension are compatible
#' @keywords internal
#' @noRd
validate_disk_compatibility <- function(output_type, dsn) {
  if (is.null(dsn)) {
    return(TRUE)
  }

  ext <- tolower(tools::file_ext(dsn))
  is_text <- ext %in% c("csv", "tsv", "txt")
  is_spatial_vector <- output_type %in% c("sf_polygons", "sf_points")
  is_dataframe <- output_type == "dataframe"
  is_raster <- output_type == "spatraster"
  is_raster_format <- ext %in%
    c("tif", "tiff", "nc", "img", "asc", "grd", "hdf", "hdf5")

  # Vector formats that support append (required for chunked disk writes)
  # Empirically tested and confirmed to work:
  # - GeoPackage (.gpkg) and SQLite (.sqlite) - excellent append support
  # - GeoParquet (.parquet, .geoparquet) - modern columnar format, supports append (sf 1.0+/GDAL 3.5+)
  # - Shapefile (.shp) - supports append (but has other limitations like field name length)
  # - GeoJSON (.geojson, .json) - supports append
  # - FlatGeobuf (.fgb) - cloud-optimized, supports append
  # - OpenFileGDB (.gdb) - ESRI FileGDB, supports append
  # - GeoJSONSeq (.geojsonl, .geojsonseq) - newline-delimited GeoJSON, supports append
  append_safe_vector_formats <- c(
    "gpkg",
    "sqlite",
    "shp",
    "geojson",
    "json",
    "fgb",
    "gdb",
    "geojsonl",
    "geojsonseq",
    "parquet",
    "geoparquet"
  )

  # Formats explicitly confirmed to NOT support append
  no_append_formats <- c("kml", "gml")

  # 1. Prevent Dataframe -> Spatial Vector Format (e.g. gpkg, shp)
  if (is_dataframe && !is_text && !is_raster_format) {
    stop(
      sprintf(
        "Output type 'dataframe' cannot be written to file extension '.%s'.\n  Please use '.csv', '.tsv', or '.txt' for dataframes, or change output_type to 'sf_polygons'/'sf_points'.",
        ext
      ),
      call. = FALSE
    )
  }

  # 2. Prevent Raster -&gt; Missing Extension
  if (is_raster && !nzchar(ext)) {
    stop(
      "Output type 'spatraster' requires a file extension to determine the format.\n  Please specify a raster format extension, e.g., 'output.tif', 'output.nc', or 'output.kea'.",
      call. = FALSE
    )
  }

  # 3. Prevent SpatRaster -> Non-Raster Formats
  if (is_raster && !is_raster_format) {
    stop(
      sprintf(
        "Output type 'spatraster' cannot be written to file extension '.%s'.\n  Please use raster formats like '.tif', '.nc', '.asc', '.img', or '.grd'.",
        ext
      ),
      call. = FALSE
    )
  }

  # 3. Validate vector format supports append (required for chunked disk writes)
  if (is_spatial_vector && !is_text) {
    if (ext %in% no_append_formats) {
      # Explicitly unsupported formats
      stop(
        sprintf(
          "Output type '%s' cannot be written to '.%s' format.\n  The '.%s' format does not support appending to existing files.\n  Supported vector formats: %s\n  Or generate the grid in memory (dsn = NULL) and save manually.",
          output_type,
          ext,
          ext,
          paste0(".", append_safe_vector_formats, collapse = ", ")
        ),
        call. = FALSE
      )
    } else if (!ext %in% append_safe_vector_formats) {
      # Unknown/untested formats - provide a warning but more permissive
      warning(
        sprintf(
          "Output type '%s' with '.%s' format has not been tested for append support.\n  Tested formats: %s\n  The operation may fail if this format does not support appending.",
          output_type,
          ext,
          paste0(".", append_safe_vector_formats, collapse = ", ")
        ),
        call. = FALSE,
        immediate. = TRUE
      )
    }
  }

  # 4. Check for readr availability if text output is requested
  if (is_text) {
    if (!requireNamespace("readr", quietly = TRUE)) {
      stop(
        "Package 'readr' is required to write to .csv/.tsv/.txt files. Please install it.",
        call. = FALSE
      )
    }
  }

  return(TRUE)
}

#' Count trailing zeros
#' @keywords internal
#' @noRd
.tz_count <- function(x) {
  n <- 0L
  x <- as.integer(x)
  while (!is.na(x) && x != 0L && x %% 10L == 0L) {
    n <- n + 1L
    x <- x %/% 10L
  }
  n
}

#' Internal helper to write a grid chunk to disk (sf or flat file)
#' @keywords internal
#' @noRd
write_grid_chunk <- function(chunk, dsn, layer, append, quiet, ...) {
  ext <- tolower(tools::file_ext(dsn))

  # --- Text/Delimited Output (readr) ---
  if (ext %in% c("csv", "tsv", "txt")) {
    # Drop geometry if it exists (e.g. user asked for sf_polygons but wrote to .csv)
    if (inherits(chunk, "sf")) {
      chunk <- sf::st_drop_geometry(chunk)
    }

    # Determine delimiter
    delim <- if (ext == "csv") "," else "\t"

    # Prepare arguments for readr::write_delim
    # readr::write_delim does not accept '...', so we must filter args manually.
    # Allowed arguments based on readr 2.x
    dots <- list(...)
    readr_args <- c("na", "quote", "escape", "eol", "num_threads", "progress")
    valid_dots <- dots[names(dots) %in% readr_args]

    # Construct call
    call_args <- c(
      list(
        x = chunk,
        file = dsn,
        delim = delim,
        append = append,
        col_names = !append, # Write headers only if NOT appending
        progress = FALSE
      ),
      valid_dots
    )

    do.call(readr::write_delim, call_args)
  } else {
    # --- Spatial Output (sf) ---
    # sf::st_write accepts '...' for driver specific options

    # Determine if we need to specify the Parquet driver explicitly
    # (GDAL doesn't auto-detect .parquet extension)
    is_parquet <- ext %in% c("parquet", "geoparquet")

    if (is_parquet) {
      sf::st_write(
        chunk,
        dsn = dsn,
        layer = layer,
        driver = "Parquet",
        append = append,
        quiet = TRUE,
        ...
      )
    } else {
      sf::st_write(
        chunk,
        dsn = dsn,
        layer = layer,
        append = append,
        quiet = TRUE,
        ...
      )
    }
  }
}
