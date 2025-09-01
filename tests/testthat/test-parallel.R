# This file contains tests for the parallel execution logic of create_grid().

# --- Test Setup ---
# A helper variable to control whether these tests should run.
# They require 'future' and 'mirai' to be installed and should be skipped on CRAN.
RUN_PARALLEL_TESTS <- Sys.getenv("NOT_CRAN") == "true" &&
  requireNamespace("future", quietly = TRUE) &&
  requireNamespace("mirai", quietly = TRUE)

# Load base data once for all tests in this file
nc_raw <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
TARGET_CRS <- 5070
CELLSIZE <- 20000 # Use a slightly larger cell size for faster tests
nc <- sf::st_transform(nc_raw, TARGET_CRS)


test_that("Parallel execution matches sequential result", {
  # Skip this entire test block if conditions are not met.
  if (!RUN_PARALLEL_TESTS) {
    skip("Skipping parallel tests: not on CRAN or missing packages.")
  }

  # --- 1. Generate the golden reference grid using the sequential method ---
  message("Generating sequential reference grid...")
  grid_seq <- create_grid(
    grid_extent = nc,
    cellsize_m = CELLSIZE,
    crs = TARGET_CRS,
    clip_to_input = TRUE,
    id_format = "both",
    parallel = FALSE
  )
  expect_s3_class(grid_seq, "sf")
  expect_gt(nrow(grid_seq), 0)
  seq_ids_long <- sort(grid_seq$GRD_ID_LONG)
  seq_ids_short <- sort(grid_seq$GRD_ID_SHORT)

  # --- 2. Test the `future` backend ---
  message("Testing 'future' backend...")

  # === FIX: Use on.exit() to manage the future plan state ===
  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  future::plan("multisession", workers = 2)
  # ==========================================================

  grid_fut <- create_grid(
    grid_extent = nc,
    cellsize_m = CELLSIZE,
    crs = TARGET_CRS,
    clip_to_input = TRUE,
    id_format = "both",
    parallel = "auto"
  )
  expect_s3_class(grid_fut, "sf")
  expect_equal(nrow(grid_fut), nrow(grid_seq))
  expect_equal(sf::st_crs(grid_fut), sf::st_crs(grid_seq))
  expect_equal(sort(grid_fut$GRD_ID_LONG), seq_ids_long)
  expect_equal(sort(grid_fut$GRD_ID_SHORT), seq_ids_short)

  # --- 3. Test the `mirai` backend ---
  message("Testing 'mirai' backend...")
  tryCatch(
    {
      mirai::daemons(2)
      grid_mirai <- create_grid(
        grid_extent = nc,
        cellsize_m = CELLSIZE,
        crs = TARGET_CRS,
        clip_to_input = TRUE,
        id_format = "both",
        parallel = "auto"
      )
      expect_s3_class(grid_mirai, "sf")
      expect_equal(nrow(grid_mirai), nrow(grid_seq))
      expect_equal(sf::st_crs(grid_mirai), sf::st_crs(grid_seq))
      expect_equal(sort(grid_mirai$GRD_ID_LONG), seq_ids_long)
      expect_equal(sort(grid_mirai$GRD_ID_SHORT), seq_ids_short)
    },
    finally = {
      mirai::daemons(0)
    }
  )
})


test_that("Backend detection logic with parallel = 'auto' works", {
  if (!RUN_PARALLEL_TESTS) {
    skip("Skipping parallel tests: not on CRAN or missing packages.")
  }

  # Scenario 1: No backend configured, should run sequentially
  expect_message(
    create_grid(nc, CELLSIZE, parallel = "auto"),
    "No parallel backend detected. Running in sequential mode."
  )

  # Scenario 2: Only 'future' is configured
  # === FIX: Use on.exit() to manage the future plan state ===
  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  future::plan("multisession", workers = 2)
  # ==========================================================

  expect_message(
    create_grid(nc, CELLSIZE, parallel = "auto"),
    "`future` backend detected. Running in parallel."
  )

  # Scenario 3: Only 'mirai' is configured
  tryCatch(
    {
      mirai::daemons(2)
      expect_message(
        create_grid(nc, CELLSIZE, parallel = "auto"),
        "`mirai` backend detected. Running in parallel."
      )
    },
    finally = {
      mirai::daemons(0)
    }
  )
})
