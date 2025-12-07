test_that("inspire_id_format works correctly for long to short conversion (default NE)", {
  long_ids <- c(
    "CRS3035RES1000mN2684000E4334000",
    "CRS3035RES10000mN2700000E4400000",
    "CRS3035RES100mN3400000E4400000"
  )
  expected_short <- c(
    "1kmN2684E4334",
    "10kmN270E440",
    "100mN34000E44000"
  )
  expect_equal(inspire_id_format(long_ids), expected_short)
})

test_that("inspire_id_format works correctly for short to long conversion with default CRS", {
  short_ids <- c(
    "1kmN2684E4334",
    "10kmN270E440",
    "100mN34000E44000"
  )
  expected_long <- c(
    "CRS3035RES1000mN2684000E4334000",
    "CRS3035RES10000mN2700000E4400000",
    "CRS3035RES100mN3400000E4400000"
  )
  expect_equal(inspire_id_format(short_ids), expected_long)
})

test_that("inspire_id_format respects axis_order = 'EN' for long to short conversion", {
  long_ids <- c(
    "CRS3035RES1000mN2684000E4334000",
    "CRS3035RES10000mN2700000E4400000"
  )
  expected_short_en <- c(
    "1kmE4334N2684",
    "10kmE440N270"
  )
  expect_equal(inspire_id_format(long_ids, axis_order = "EN"), expected_short_en)
})

test_that("inspire_id_format correctly parses 'EN' axis order for short to long conversion", {
  short_ids_en <- c(
    "1kmE4334N2684",
    "10kmE440N270",
    "100mE44000N34000"
  )
  expected_long <- c(
    "CRS3035RES1000mN2684000E4334000",
    "CRS3035RES10000mN2700000E4400000",
    "CRS3035RES100mN3400000E4400000"
  )
  expect_equal(inspire_id_format(short_ids_en), expected_long)
})


test_that("inspire_id_format works correctly for short to long with a custom CRS", {
  short_ids_ne <- c("1kmN2684E4334", "10kmN270E440")
  short_ids_en <- c("1kmE4334N2684", "10kmE440N270")

  # Note: CRS is now 3857, not the default 3035
  expected_long_3857 <- c(
    "CRS3857RES1000mN2684000E4334000",
    "CRS3857RES10000mN2700000E4400000"
  )

  # Test with NE input
  expect_equal(inspire_id_format(short_ids_ne, crs = 3857), expected_long_3857)
  # Test with EN input
  expect_equal(inspire_id_format(short_ids_en, crs = 3857), expected_long_3857)
})

test_that("inspire_id_format maintains round-trip consistency (long -> short 'NE' -> long)", {
  original_long <- c(
    "CRS3035RES1000mN2684000E4334000",
    "CRS3035RES5000mN2700000E4400000"
  )
  shortened <- inspire_id_format(original_long, axis_order = "NE")
  re_lengthened <- inspire_id_format(shortened, crs = 3035)
  expect_equal(re_lengthened, original_long)
})

test_that("inspire_id_format maintains round-trip consistency (short 'NE' -> long -> short 'NE')", {
  original_short <- c("1kmN2684E4334", "5kmN540E880")
  lengthened <- inspire_id_format(original_short)
  re_shortened <- inspire_id_format(lengthened, axis_order = "NE")
  expect_equal(re_shortened, original_short)
})

test_that("inspire_id_format maintains round-trip consistency (long -> short 'EN' -> long)", {
  original_long <- c(
    "CRS3035RES1000mN2684000E4334000",
    "CRS3035RES5000mN2700000E4400000"
  )
  shortened_en <- inspire_id_format(original_long, axis_order = "EN")
  re_lengthened <- inspire_id_format(shortened_en, crs = 3035)
  expect_equal(re_lengthened, original_long)
})

test_that("inspire_id_format maintains round-trip consistency (short 'EN' -> long -> short 'EN')", {
  original_short_en <- c("1kmE4334N2684", "5kmE880N540")
  lengthened <- inspire_id_format(original_short_en)
  re_shortened_en <- inspire_id_format(lengthened, axis_order = "EN")
  expect_equal(re_shortened_en, original_short_en)
})

test_that("inspire_id_format handles empty input gracefully", {
  expect_equal(inspire_id_format(character(0)), character(0))
})

test_that("inspire_id_format throws an error for mixed input formats", {
  mixed_ids <- c(
    "CRS3035RES1000mN2684000E4334000",
    "10kmN270E440"
  )
  expect_error(
    inspire_id_format(mixed_ids),
    "Input contains a mix of long and short INSPIRE ID formats."
  )
})

test_that("inspire_id_format handles single-item vectors correctly for all formats", {
  long_id <- "CRS3035RES100mN12300E45600"
  short_id_ne <- "100mN123E456"
  short_id_en <- "100mE456N123"

  # Test long -> short (both orders)
  expect_equal(inspire_id_format(long_id, axis_order = "NE"), short_id_ne)
  expect_equal(inspire_id_format(long_id, axis_order = "EN"), short_id_en)

  # Test short -> long (both orders)
  expect_equal(inspire_id_format(short_id_ne, crs = 3035), long_id)
  expect_equal(inspire_id_format(short_id_en, crs = 3035), long_id)
})

test_that("inspire_id_format throws an error for invalid axis_order", {
  long_ids <- "CRS3035RES1000mN2684000E4334000"
  # Using `regexp = "'arg' should be one of"` which is a standard part of
  # the error message from `match.arg`.
  expect_error(
    inspire_id_format(long_ids, axis_order = "XY"),
    regexp = "'arg' should be one of"
  )
})
