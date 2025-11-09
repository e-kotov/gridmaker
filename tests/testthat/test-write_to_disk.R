test_that("create_grid streams correctly to disk with mirai backend", {
  # 1. SKIP CONDITIONS ----
  # This test is multi-core and requires a specific backend.
  # It should not run on CRAN or in environments without mirai.
  # skip_on_cran() # running the test on 2 cores is OK on CRAN
  skip_if_not_installed("mirai")
  # The `nc` object from setup.R requires `sf`
  skip_if_not_installed("sf")

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
  grid_in_memory <- do.call(create_grid, c(common_args, list(parallel = FALSE)))

  # 4. RUN STREAMING GRID CREATION (ON-DISK) ----
  # Set up a 2-core mirai backend for the test
  mirai::daemons(2)
  # In some CI environments, setting daemons might fail. Skip if so.
  skip_if(mirai::status()$connections < 2, "Could not set up 2 mirai daemons")

  # Call create_grid with `dsn` to trigger the streaming behavior
  do.call(
    create_grid,
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

test_that("create_grid handles `layer` argument correctly for disk output", {
  skip_if_not_installed("sf")

  temp_dir <- tempfile("grid_test_")
  dir.create(temp_dir)
  withr::defer(unlink(temp_dir, recursive = TRUE, force = TRUE))

  simple_extent <- c(xmin = 0, ymin = 0, xmax = 100, ymax = 100)

  # Test 1: When layer is NULL, it defaults from dsn and gives a message
  dsn_default <- file.path(temp_dir, "default.gpkg")

  expect_message(
    create_grid(
      grid_extent = simple_extent,
      cellsize_m = 10,
      crs = 3035,
      dsn = dsn_default,
      layer = NULL,
      quiet = FALSE
    ),
    "defaulting to 'default'"
  )

  expect_true(file.exists(dsn_default))
  expect_equal(sf::st_layers(dsn_default)$name, "default")

  # Test 2: When layer is specified, it is used correctly
  dsn_specified <- file.path(temp_dir, "specified.gpkg")
  custom_layer <- "my_grid"

  # Using quiet = TRUE to avoid other messages and focus the test
  create_grid(
    grid_extent = simple_extent,
    cellsize_m = 10,
    crs = 3035,
    dsn = dsn_specified,
    layer = custom_layer,
    quiet = TRUE
  )

  expect_true(file.exists(dsn_specified))
  expect_equal(sf::st_layers(dsn_specified)$name, custom_layer)
})
