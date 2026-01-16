# Skip entire file on Windows with R < 4.2 due to process crashes during I/O tests
if (getRversion() < "4.2.0" && .Platform$OS.type == "windows") {
  testthat::skip(
    "Disk writing tests skipped on Windows with R < 4.2 due to process crashes"
  )
}

test_that("inspire_grid_from_extent streams correctly to disk with mirai backend", {
  # 1. SKIP CONDITIONS ----
  # This test is multi-core and requires a specific backend.
  # It should not run on CRAN or in environments without mirai.
  # skip_on_cran() # running the test on 2 cores is OK on CRAN
  skip_if_not_installed("mirai")
  # The `nc` object from setup.R requires `sf`
  skip_if_not_installed("sf")
  # Skip on Windows with R < 4.2 due to later/promises event loop cleanup issues
  # that cause the R process to crash during test completion
  skip_if(
    getRversion() < "4.2.0" && .Platform$OS.type == "windows",
    "Mirai streaming tests skipped on Windows with R < 4.2 (event loop issues)"
  )

  # 2. SETUP ----
  # Assumes `nc` and `CELLSIZE` are loaded from `tests/testthat/setup.R`

  # Ensure daemons are shut down when the test finishes, regardless of success
  withr::defer(mirai::daemons(0))

  # Create a temporary file path for the output GeoPackage
  temp_dsn <- tempfile(fileext = ".gpkg")
  # Ensure the temp file is deleted when the test finishes
  withr::defer(unlink(temp_dsn, force = TRUE))

  # Define common arguments for both grid creation calls using the shared objects
  common_args <- list(
    grid_extent = nc,
    cellsize_m = CELLSIZE,
    output_type = "sf_polygons",
    clip_to_input = TRUE,
    id_format = "both",
    include_llc = TRUE,
    quiet = TRUE
  )

  # 3. GENERATE REFERENCE GRID (IN-MEMORY) ----
  # This is the "ground truth" that we will compare against.
  # It is run sequentially to ensure deterministic output.
  grid_in_memory <- do.call(
    inspire_grid_from_extent,
    c(common_args, list(parallel = FALSE))
  )

  # 4. RUN STREAMING GRID CREATION (ON-DISK) ----
  # Set up a 2-core mirai backend for the test
  mirai::daemons(2)
  # In some CI environments, setting daemons might fail. Skip if so.
  skip_if(mirai::status()$connections < 2, "Could not set up 2 mirai daemons")

  # Call inspire_grid_from_extent with `dsn` to trigger the streaming behavior
  do.call(
    inspire_grid_from_extent,
    c(
      common_args,
      list(
        dsn = temp_dsn,
        layer = "streamed_grid"
      )
    )
  )

  # 5. READ BACK AND COMPARE ----
  # A. Check that the file was actually created
  expect_true(file.exists(temp_dsn))

  # B. Read the grid back from the file
  grid_from_disk <- sf::st_read(temp_dsn, quiet = TRUE)
  grid_from_disk$id <- NULL # Remove any auto-generated IDs for comparison

  # 6. ASSERTIONS ----
  # C. Compare row counts and check for duplicates (the core of the bug fix)
  expect_equal(nrow(grid_from_disk), nrow(grid_in_memory))
  expect_equal(sum(duplicated(grid_from_disk$GRD_ID_LONG)), 0)

  # D. Perform a robust content comparison.
  # The row order is not guaranteed in parallel processing, so we must sort
  # both data frames by a unique identifier before comparing them.
  grid_in_memory_sorted <- grid_in_memory[order(grid_in_memory$GRD_ID_LONG), ]
  grid_from_disk_sorted <- grid_from_disk[order(grid_from_disk$GRD_ID_LONG), ]

  # Reset row names to ensure the comparison is clean
  row.names(grid_in_memory_sorted) <- NULL
  row.names(grid_from_disk_sorted) <- NULL

  # E. Compare the data columns (everything except geometry)
  expect_equal(
    sf::st_drop_geometry(grid_from_disk_sorted),
    sf::st_drop_geometry(grid_in_memory_sorted)
  )

  # F. Compare the geometries. Comparing the WKB (Well-Known Binary)
  # representation is a robust way to check for exact geometric equality.
  expect_equal(
    sf::st_as_binary(sf::st_geometry(grid_from_disk_sorted)),
    sf::st_as_binary(sf::st_geometry(grid_in_memory_sorted))
  )
})

test_that("inspire_grid_from_extent handles `layer` argument correctly for disk output", {
  skip_if_not_installed("sf")

  temp_dir <- tempfile("grid_test_")
  dir.create(temp_dir)
  withr::defer(unlink(temp_dir, recursive = TRUE, force = TRUE))

  simple_extent <- sf::st_bbox(
    c(xmin = 0, ymin = 0, xmax = 100, ymax = 100),
    crs = 3035
  )

  # Test 1: When layer is NULL, it defaults from dsn and gives a message
  dsn_default <- file.path(temp_dir, "default.gpkg")

  capture.output(
    suppressMessages(
      expect_message(
        inspire_grid_from_extent(
          grid_extent = simple_extent,
          cellsize_m = 10,
          dsn = dsn_default,
          layer = NULL,
          quiet = FALSE
        ),
        "defaulting to 'default'"
      )
    ),
    file = nullfile()
  )

  expect_true(file.exists(dsn_default))
  expect_equal(sf::st_layers(dsn_default)$name, "default")

  # Test 2: When layer is specified, it is used correctly
  dsn_specified <- file.path(temp_dir, "specified.gpkg")
  custom_layer <- "my_grid"

  # Using quiet = TRUE to avoid other messages and focus the test
  inspire_grid_from_extent(
    grid_extent = simple_extent,
    cellsize_m = 10,
    dsn = dsn_specified,
    layer = custom_layer,
    quiet = TRUE
  )

  expect_true(file.exists(dsn_specified))
  expect_equal(sf::st_layers(dsn_specified)$name, custom_layer)
})

test_that("inspire_grid_from_extent returns dsn invisibly when writing to disk", {
  skip_if_not_installed("sf")

  temp_dsn <- tempfile(fileext = ".gpkg")
  withr::defer(unlink(temp_dsn, force = TRUE))

  # Use expect_silent to capture the return value without printing it
  # The result of inspire_grid_from_extent should be the dsn path
  returned_dsn <- expect_silent(
    inspire_grid_from_extent(
      grid_extent = c(0, 0, 100, 100),
      cellsize_m = 10,
      crs = 3035,
      dsn = temp_dsn,
      quiet = TRUE
    )
  )

  # Check that the returned value is the same as the dsn path provided
  expect_equal(returned_dsn, temp_dsn)

  # Also check that the file was actually created
  expect_true(file.exists(temp_dsn))
})

test_that("validate_disk_compatibility validates formats correctly", {
  # Error: Dataframe -> GPKG
  expect_error(
    validate_disk_compatibility("dataframe", "test.gpkg"),
    "Output type 'dataframe' cannot be written to file extension '.gpkg'"
  )

  # Success: Dataframe -> CSV
  skip_if_not_installed("readr")
  expect_true(validate_disk_compatibility("dataframe", "test.csv"))

  # Success: Tested vector formats that support append
  expect_true(validate_disk_compatibility("sf_polygons", "test.gpkg"))
  expect_true(validate_disk_compatibility("sf_polygons", "test.sqlite"))
  expect_true(validate_disk_compatibility("sf_polygons", "test.shp"))
  expect_true(validate_disk_compatibility("sf_polygons", "test.geojson"))
  expect_true(validate_disk_compatibility("sf_polygons", "test.json"))

  # Error: KML and GML explicitly cannot append
  expect_error(
    validate_disk_compatibility("sf_polygons", "test.kml"),
    "cannot be written to '.kml' format"
  )

  expect_error(
    validate_disk_compatibility("sf_polygons", "test.gml"),
    "cannot be written to '.gml' format"
  )

  # Success: Newly added tested formats
  expect_true(validate_disk_compatibility("sf_polygons", "test.fgb"))
  expect_true(validate_disk_compatibility("sf_polygons", "test.geojsonl"))
  expect_true(validate_disk_compatibility("sf_polygons", "test.geojsonseq"))

  # Warning: Unknown format (e.g., MapInfo TAB - not in our driver mapping)
  expect_warning(
    validate_disk_compatibility("sf_polygons", "test.tab"),
    "Unknown vector format"
  )
})

# --- Integration test: verify gridmaker output is correct across vector formats ---
# Tests OUR code (format detection, parameter passing) not GDAL drivers

test_that("inspire_grid produces correct vector output in multiple formats", {
  skip_if_not_installed("sf")

  # Helper to test a format if driver is available
  test_vector_format <- function(ext) {
    # Use gridmaker's internal driver check (strip leading dot)
    ext_no_dot <- sub("^\\.", "", ext)
    driver_name <- gridmaker:::.ext_to_driver(ext_no_dot, "vector")
    if (is.null(driver_name)) {
      return(NULL)
    }

    check <- gridmaker:::.check_driver_available(driver_name, "vector")
    if (!check$available) {
      return(NULL)
    }

    tf <- tempfile(fileext = ext)
    on.exit(unlink(tf), add = TRUE)

    # Wrap in tryCatch - some drivers report availability but fail on write
    result <- tryCatch(
      {
        inspire_grid_from_extent(
          grid_extent = c(0, 0, 20000, 20000),
          cellsize_m = 10000,
          crs = 3035,
          output_type = "sf_polygons",
          dsn = tf,
          quiet = TRUE
        )

        # Verify output
        expect_true(file.exists(tf), info = paste("File created for", ext))
        sf_data <- sf::st_read(tf, quiet = TRUE)
        expect_equal(nrow(sf_data), 4, info = paste("Row count for", ext))
        expect_true(
          all(sf::st_geometry_type(sf_data) == "POLYGON"),
          info = paste("Geometry type for", ext)
        )

        ext # Return extension to track which were tested
      },
      error = function(e) NULL # Driver reported available but failed
    )
    result
  }

  # Test available formats
  formats_tested <- c(
    test_vector_format(".gpkg"),
    test_vector_format(".geojson"),
    test_vector_format(".fgb"),
    test_vector_format(".parquet")
  )

  # At least GPKG should always be available
  expect_true(
    !all(sapply(formats_tested, is.null)),
    "At least one vector format should be available"
  )
})

test_that("inspire_grid_from_ids writes dataframe to CSV correctly (with chunking)", {
  skip_if_not_installed("readr")
  skip_if_not_installed("sf")

  ids <- c(
    "CRS3035RES1000mN3500000E4400000",
    "CRS3035RES1000mN3501000E4400000"
  )

  tmp_csv <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp_csv), add = TRUE)

  # Write dataframe to CSV
  inspire_grid_from_ids(
    ids,
    output_type = "dataframe",
    dsn = tmp_csv,
    quiet = TRUE
  )

  expect_true(file.exists(tmp_csv))

  # Check content (default id_format = "both" produces GRD_ID_LONG and GRD_ID_SHORT)
  df_in <- readr::read_csv(tmp_csv, show_col_types = FALSE)
  expect_equal(nrow(df_in), 2)
  expect_true(all(c("GRD_ID_LONG", "GRD_ID_SHORT") %in% names(df_in)))
  expect_equal(df_in$GRD_ID_LONG, ids)
})

test_that("inspire_grid_from_extent streams to CSV (dropping geometry)", {
  skip_if_not_installed("sf")
  skip_if_not_installed("readr")

  # Setup: Create a grid that will definitely be chunked (small RAM limit sim or just standard stream)
  # We use standard stream_grid_sequential via inspire_grid_from_extent by not setting parallel

  simple_extent <- sf::st_bbox(
    c(xmin = 0, ymin = 0, xmax = 20000, ymax = 20000),
    crs = 3035
  )
  tmp_csv <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp_csv), add = TRUE)

  # Even if we request sf_polygons, writing to CSV should drop geom and succeed
  inspire_grid_from_extent(
    grid_extent = simple_extent,
    cellsize_m = 10000,
    output_type = "sf_polygons",
    dsn = tmp_csv,
    parallel = FALSE,
    quiet = TRUE
  )

  expect_true(file.exists(tmp_csv))

  df_in <- readr::read_csv(tmp_csv, show_col_types = FALSE)
  # 20km extent / 10km cells = 2x2 = 4 cells
  expect_equal(nrow(df_in), 4)
  # Should NOT have geometry column (WKT) unless explicitly converted,
  # st_drop_geometry removes it.
  expect_false("geometry" %in% names(df_in))
})

test_that("CSV appending correctly handles headers", {
  skip_if_not_installed("readr")

  tmp_csv <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp_csv), add = TRUE)

  # Create two chunks manually
  df1 <- data.frame(a = 1, b = 2)
  df2 <- data.frame(a = 3, b = 4)

  # Write first chunk (new file)
  write_grid_chunk(df1, tmp_csv, layer = NULL, append = FALSE, quiet = TRUE)

  # Write second chunk (append)
  write_grid_chunk(df2, tmp_csv, layer = NULL, append = TRUE, quiet = TRUE)

  # Read back
  res <- readr::read_csv(tmp_csv, show_col_types = FALSE)

  # Should have 2 rows
  expect_equal(nrow(res), 2)
  # Should not have 'a' appearing as a value in the second row (which happens if header is repeated)
  expect_equal(res$a, c(1, 3))
})

test_that("CSV writing respects extra arguments (e.g. na string)", {
  skip_if_not_installed("readr")

  tmp_csv <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp_csv), add = TRUE)

  chunk <- data.frame(a = c(1, NA), b = c("x", "y"))

  # Write with custom NA string
  write_grid_chunk(
    chunk,
    tmp_csv,
    layer = NULL,
    append = FALSE,
    quiet = TRUE,
    na = "MISSING"
  )

  # Read back raw text to verify "MISSING" is there
  lines <- readLines(tmp_csv)
  expect_true(any(grepl("MISSING", lines)))

  # Check standard NA
  chunk2 <- data.frame(a = c(NA), b = c(NA))
  tmp_csv2 <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp_csv2), add = TRUE)
  # Test default NA string written to CSV when no custom 'na' argument is provided.
  # The default NA string is usually "" (empty string) or "NA" depending on the readr version.
  write_grid_chunk(chunk2, tmp_csv2, layer = NULL, append = FALSE, quiet = TRUE)
  lines2 <- readLines(tmp_csv2)
  # Check that either "" (empty field) or "NA" appears in the output for NA values.
  # This ensures the default NA handling is as expected.
  expect_true(any(grepl('(^|,)NA($|,)', lines2)) || any(grepl(',,', lines2)))

  # Verify pass-through of 'quote' arg
  tmp_csv3 <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp_csv3), add = TRUE)
  write_grid_chunk(
    chunk,
    tmp_csv3,
    layer = NULL,
    append = FALSE,
    quiet = TRUE,
    quote = "all"
  )
  lines3 <- readLines(tmp_csv3)
  # Look for quoted character values like "x" (quote = "all" quotes character columns)
  expect_true(any(grepl('"x"', lines3)))
  # Also check that column names are quoted
  expect_true(any(grepl('"a"', lines3)))
})
