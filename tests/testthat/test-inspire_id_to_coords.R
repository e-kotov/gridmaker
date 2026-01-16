# --- Tests for inspire_id_to_coords ---

test_that("inspire_id_to_coords works with long format IDs", {
  long_id <- "CRS3035RES1000mN3497000E4447000"

  # Default output (dataframe)
  parsed_df <- inspire_id_to_coords(long_id)
  expect_s3_class(parsed_df, "data.frame")
  expect_equal(names(parsed_df), c("crs", "cellsize", "y", "x"))
  expect_equal(parsed_df$crs, 3035)
  expect_equal(parsed_df$cellsize, 1000)
  expect_equal(parsed_df$y, 3497000)
  expect_equal(parsed_df$x, 4447000)

  # sf output
  parsed_sf <- inspire_id_to_coords(long_id, as_sf = TRUE)
  expect_s3_class(parsed_sf, "sf")
  expect_equal(sf::st_crs(parsed_sf), sf::st_crs(3035))
  coords <- sf::st_coordinates(parsed_sf)
  expect_equal(as.numeric(coords[1, "X"]), 4447000)
  expect_equal(as.numeric(coords[1, "Y"]), 3497000)
})

test_that("inspire_id_to_coords works with short format IDs", {
  short_id <- "10kmN349E444"

  expect_warning(
    parsed_df <- inspire_id_to_coords(short_id),
    "CRS not specified for short-form IDs"
  )

  # Default output (dataframe)
  # 10km cells with short coords N349E444 scale to (3490000, 4440000) in meters
  # Because 10000m has 4 trailing zeros, multiplier = 10^4 = 10000
  parsed_df <- inspire_id_to_coords(short_id, crs = 3035)
  expect_s3_class(parsed_df, "data.frame")
  expect_equal(names(parsed_df), c("crs", "cellsize", "y", "x"))
  expect_equal(parsed_df$cellsize, 10000) # Check m conversion
  expect_equal(parsed_df$y, 3490000)
  expect_equal(parsed_df$x, 4440000)

  # sf output
  parsed_sf <- inspire_id_to_coords(short_id, as_sf = TRUE, crs = 3035)
  expect_s3_class(parsed_sf, "sf")
  # Short format IDs default to CRS 3035
  expect_equal(sf::st_crs(parsed_sf), sf::st_crs(3035))
  coords <- sf::st_coordinates(parsed_sf)
  expect_equal(as.numeric(coords[1, "X"]), 4440000)
  expect_equal(as.numeric(coords[1, "Y"]), 3490000)
})

test_that("inspire_id_to_coords handles vectorization and mixed-format issues", {
  ids_vec <- c(
    "CRS3035RES1000mN3497000E4447000",
    "1kmE4478N3618", # legacy short format
    "1kmN3618E4478" # new short format
  )

  expect_warning(
    parsed_vec <- inspire_id_to_coords(ids_vec),
    regexp = "contains a mix of long"
  )
  expect_equal(nrow(parsed_vec), 3)

  # 1km = 1000m has 3 trailing zeros, multiplier = 10^3 = 1000
  # So short coords 3618 and 4478 scale to 3618000 and 4478000
  expect_equal(parsed_vec$cellsize[1], 1000)
  expect_equal(parsed_vec$x[1], 4447000)
  expect_equal(parsed_vec$y[2], 3618000)
  expect_equal(parsed_vec$x[3], 4478000)
})

test_that("inspire_id_to_coords handles edge cases: malformed, NA, and empty inputs", {
  # Vector with a good ID, a malformed one, and an NA
  ids_have_na <- c(
    "1kmN100E200", # Valid
    NA_character_ # NA
  )
  expect_error(
    inspire_id_to_coords(ids_have_na),
    "Input 'inspire' contains NA values"
  )

  ids_have_malformed <- c(
    "1kmN100E200", # Valid
    "1kmX100Y200" # Malformed
  )

  expect_error(
    inspire_id_to_coords(ids_have_malformed),
    "One or more INSPIRE IDs had a malformed format"
  )

  # fail on Empty input vector
  expect_error(
    inspire_id_to_coords(character(0)),
    "Input 'inspire' cannot be an empty vector"
  )
  # fail on input containing empty strings
  ids_have_empty <- c("1kmN100E200", "")
  expect_error(
    inspire_id_to_coords(ids_have_empty),
    "Input 'inspire' contains empty strings"
  )
})

test_that("inspire_id_to_coords warns on multiple CRSs", {
  # This only applies to long-form IDs where CRS is parsed
  mixed_crs_ids <- c(
    "CRS3035RES1mN1E1",
    "CRS5070RES1mN2E2" # Different CRS
  )

  # Should warn when creating an sf object from mixed CRSs
  expect_error(
    parsed_sf <- inspire_id_to_coords(mixed_crs_ids, as_sf = TRUE),
    "INSPIRE identifiers contain more than one CRS"
  )
})
