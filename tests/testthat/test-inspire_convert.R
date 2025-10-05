test_that("inspire_convert works correctly for long to short conversion", {
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
  expect_equal(inspire_convert(long_ids), expected_short)
})

test_that("inspire_convert works correctly for short to long conversion with default CRS", {
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
  expect_equal(inspire_convert(short_ids), expected_long)
})

test_that("inspire_convert works correctly for short to long conversion with a custom CRS", {
  short_ids <- c("1kmN2684E4334", "10kmN270E440")
  expected_long <- c(
    "CRS3035RES1000mN2684000E4334000",
    "CRS3035RES10000mN2700000E4400000"
  )
  expect_equal(inspire_convert(short_ids, crs = 3035), expected_long)
})

test_that("inspire_convert maintains round-trip consistency (long -> short -> long)", {
  original_long <- c(
    "CRS3035RES1000mN2684000E4334000",
    "CRS3035RES5000mN2700000E4400000"
  )
  shortened <- inspire_convert(original_long)
  re_lengthened <- inspire_convert(shortened, crs = 3035) # Must specify CRS here
  expect_equal(re_lengthened, original_long)
})

test_that("inspire_convert maintains round-trip consistency (short -> long -> short)", {
  original_short <- c("1kmN2684E4334", "5kmN540E880")
  lengthened <- inspire_convert(original_short)
  re_shortened <- inspire_convert(lengthened)
  expect_equal(re_shortened, original_short)
})

test_that("inspire_convert handles empty input gracefully", {
  expect_equal(inspire_convert(character(0)), character(0))
})

test_that("inspire_convert throws an error for mixed input formats", {
  mixed_ids <- c(
    "CRS3035RES1000mN2684000E4334000",
    "10kmN270E440"
  )
  expect_error(
    inspire_convert(mixed_ids),
    "Input contains a mix of long and short INSPIRE ID formats."
  )
})

test_that("inspire_convert handles single-item vectors correctly", {
  long_id <- "CRS3035RES100mN12300E45600"
  short_id <- "100mN123E456"
  expect_equal(inspire_convert(long_id), short_id)
  expect_equal(inspire_convert(short_id, crs = 3035), long_id)
})
