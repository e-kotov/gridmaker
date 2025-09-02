# This file contains tests for the parallel execution logic of create_grid().

test_that("Parallel execution matches sequential result", {
  # These tests are long-running and require optional packages.
  skip_on_cran()
  skip_if_not_installed("future")
  skip_if_not_installed("mirai")

  # --- 1. Generate the golden reference grid using the sequential method ---
  message("Generating sequential reference grid...")
  grid_seq <- create_grid(
    grid_extent = nc,
    cellsize_m = CELLSIZE,
    crs = TARGET_CRS,
    clip_to_input = TRUE,
    id_format = "both",
    parallel = FALSE,
    quiet = TRUE # Suppress messages for cleaner test output
  )
  expect_s3_class(grid_seq, "sf")
  expect_gt(nrow(grid_seq), 0)
  seq_ids_long <- sort(grid_seq$GRD_ID_LONG)
  seq_ids_short <- sort(grid_seq$GRD_ID_SHORT)

  # --- 2. Test the `future` backend ---
  message("Testing 'future' backend...")

  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  future::plan("multisession", workers = 2)

  grid_fut <- create_grid(
    grid_extent = nc,
    cellsize_m = CELLSIZE,
    crs = TARGET_CRS,
    clip_to_input = TRUE,
    id_format = "both",
    parallel = "auto",
    quiet = TRUE # Suppress messages for cleaner test output
  )
  expect_s3_class(grid_fut, "sf")
  expect_equal(nrow(grid_fut), nrow(grid_seq))
  expect_equal(sf::st_crs(grid_fut), sf::st_crs(grid_seq))
  expect_equal(sort(grid_fut$GRD_ID_LONG), seq_ids_long)
  expect_equal(sort(grid_fut$GRD_ID_SHORT), seq_ids_short)
  expect_false(any(duplicated(grid_fut$GRD_ID_LONG)))
  expect_false(any(duplicated(grid_fut$GRD_ID_SHORT)))

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
        parallel = "auto",
        quiet = TRUE # Suppress messages for cleaner test output
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
  skip_on_cran()
  skip_if_not_installed("future")
  skip_if_not_installed("mirai")

  # Scenario 1: No backend configured, should run sequentially
  expect_message(
    create_grid(nc, CELLSIZE, parallel = "auto"),
    "No parallel backend detected. Running in sequential mode."
  )

  # Scenario 2: Only 'future' is configured
  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  future::plan("multisession", workers = 2)

  expect_message(
    create_grid(nc, CELLSIZE, parallel = "auto"),
    "`future` backend detected. Running in parallel."
  )
  future::plan(old_plan) # Clean up immediately

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
