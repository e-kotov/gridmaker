library(testthat)
library(gridmaker)

test_that("vector_grid_backend validation works in inspire_grid", {
  # Valid backends
  expect_no_error(
    inspire_grid(
      matrix(c(0, 0, 1000, 1000), 2, 2),
      cellsize_m = 1000,
      crs = 3035,
      vector_grid_backend = "cpp"
    )
  )

  # Invalid backend throws error via match.arg
  expect_error(
    inspire_grid(
      matrix(c(0, 0, 1000, 1000), 2, 2),
      cellsize_m = 1000,
      crs = 3035,
      vector_grid_backend = "invalid_backend"
    ),
    "should be one of .cpp., .sfheaders."
  )
})

test_that("vector_grid_backend honors options while validating", {
  # Set invalid option
  withr::with_options(list(gridmaker.vector_grid_backend = "typo"), {
    expect_error(
      inspire_grid(
        matrix(c(0, 0, 1000, 1000), 2, 2),
        cellsize_m = 1000,
        crs = 3035
      ),
      "should be one of .cpp., .sfheaders."
    )
  })

  # Set valid option
  withr::with_options(list(gridmaker.vector_grid_backend = "sfheaders"), {
    # This might require sfheaders installed to run fully, 
    # but match.arg validation should pass anyway
    # We check if it AT LEAST passes match.arg and then might fail on requireNamespace
    tryCatch({
      inspire_grid(
        matrix(c(0, 0, 1000, 1000), 2, 2),
        cellsize_m = 1000,
        crs = 3035,
        vector_grid_backend = "sfheaders"
      )
    }, error = function(e) {
      # If it fails, it should NOT be because of match.arg
      expect_false(grepl("should be one of", e$message))
    })
  })
})

test_that("vector_grid_backend validation works in inspire_grid_from_ids", {
  ids <- c("CRS3035RES1000mN3000000E4000000")
  
  expect_no_error(
    inspire_grid_from_ids(ids, vector_grid_backend = "cpp")
  )
  
  expect_error(
    inspire_grid_from_ids(ids, vector_grid_backend = "wrong"),
    "should be one of .cpp., .sfheaders."
  )
})
