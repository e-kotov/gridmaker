test_that("tune_parallel_configuration caps in-memory workers correctly", {
  # Mock extent (100x100km)
  ext <- c(xmin = 0, ymin = 0, xmax = 100000, ymax = 100000)

  # Disk Stream: Should return all workers (16)
  w_disk <- tune_parallel_configuration(
    ext,
    1,
    3035,
    16,
    "mirai",
    is_disk_stream = TRUE
  )
  expect_equal(w_disk, 16)

  # In-Memory Mirai: Cap at 4
  w_mem_mirai <- tune_parallel_configuration(
    ext,
    1,
    3035,
    16,
    "mirai",
    is_disk_stream = FALSE
  )
  expect_equal(w_mem_mirai, 4)

  # In-Memory Future: Cap at 2 for huge grid (500k+ cells)
  # cellsize = 1 -> 1e10 cells (Huge)
  w_mem_fut <- tune_parallel_configuration(
    ext,
    1,
    3035,
    16,
    "future",
    is_disk_stream = FALSE
  )
  expect_equal(w_mem_fut, 2)

  # Trivial Grid: Sequential (0)
  # 100km extent / 50km cells = 2x2 = 4 cells total.
  # 4 < 10000 -> Should return 0.
  w_tiny <- tune_parallel_configuration(
    ext,
    50000,
    3035,
    16,
    "mirai",
    is_disk_stream = FALSE
  )
  expect_equal(w_tiny, 0)
})
