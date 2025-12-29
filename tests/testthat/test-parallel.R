# This file contains tests for the parallel execution logic of inspire_grid_from_extent().

test_that("Parallel execution (future) matches sequential result", {
  skip_if_not_installed("future")
  skip_if(getRversion() < "4.2.0")

  # --- 1. Setup ---
  grid_seq <- inspire_grid_from_extent(
    grid_extent = nc,
    cellsize_m = CELLSIZE,
    crs = TARGET_CRS,
    clip_to_input = TRUE,
    id_format = "both",
    parallel = FALSE,
    quiet = TRUE
  )
  seq_ids_long <- sort(grid_seq$GRD_ID_LONG)
  seq_ids_short <- sort(grid_seq$GRD_ID_SHORT)

  # --- 2. Test future ---
  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  future::plan("multisession", workers = 2)

  grid_fut <- inspire_grid_from_extent(
    grid_extent = nc,
    cellsize_m = CELLSIZE,
    crs = TARGET_CRS,
    clip_to_input = TRUE,
    id_format = "both",
    parallel = "auto",
    quiet = TRUE
  )
  expect_equal(sort(grid_fut$GRD_ID_LONG), seq_ids_long)
  expect_equal(sort(grid_fut$GRD_ID_SHORT), seq_ids_short)
})

test_that("Parallel execution (mirai) matches sequential result", {
  skip_if_not_installed("mirai")
  skip_if(getRversion() < "4.2.0")

  # --- 1. Setup ---
  grid_seq <- inspire_grid_from_extent(
    grid_extent = nc,
    cellsize_m = CELLSIZE,
    crs = TARGET_CRS,
    clip_to_input = TRUE,
    id_format = "both",
    parallel = FALSE,
    quiet = TRUE
  )
  seq_ids_long <- sort(grid_seq$GRD_ID_LONG)
  seq_ids_short <- sort(grid_seq$GRD_ID_SHORT)

  # --- 2. Test mirai ---
  tryCatch(
    {
      mirai::daemons(2)
      grid_mirai <- inspire_grid_from_extent(
        grid_extent = nc,
        cellsize_m = CELLSIZE,
        crs = TARGET_CRS,
        clip_to_input = TRUE,
        id_format = "both",
        parallel = "auto",
        quiet = TRUE
      )
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
  skip_if(getRversion() < "4.2.0")

  # Scenario 1: No backend configured, should run sequentially
  expect_message(
    inspire_grid_from_extent(nc, CELLSIZE, parallel = "auto"),
    "No parallel backend detected. Running in sequential mode."
  )

  # Scenario 2: Only 'future' is configured
  {
    old_plan <- future::plan()
    on.exit(future::plan(old_plan), add = TRUE)
    future::plan("multisession", workers = 2)

    expect_message(
      inspire_grid_from_extent(nc, CELLSIZE, parallel = "auto"),
      "`future` backend detected.*Running in parallel"
    )
    future::plan("sequential")
  }

  # Scenario 3: Only 'mirai' is configured
  tryCatch(
    {
      mirai::daemons(2)
      expect_message(
        inspire_grid_from_extent(nc, CELLSIZE, parallel = "auto"),
        "`mirai` backend detected.*Running in parallel"
      )
    },
    finally = {
      mirai::daemons(0)
    }
  )
})
