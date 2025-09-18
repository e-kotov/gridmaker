#' @name inspire
#' @param inspire A vector of INSPIRE IDs. Can be either legacy or non-legacy.
#' @param as_sf Whether to return an object of class \code{sfc} or a dataframe.
#' @export
inspire_extract <- function(inspire, as_sf = FALSE) {
  if (all(startsWith(inspire, "CRS"))) {
    parsed <- utils::strcapture(
      "^CRS([0-9]+)RES([0-9]+)mN([0-9]+)E([0-9]+)$",
      x = inspire,
      proto = list(crs = integer(), res = numeric(), y = integer(), x = integer())
    )
  } else {
    parsed <- utils::strcapture(
      "^([0-9]+k?m)N([0-9]+)E([0-9]+)$",
      x = inspire,
      proto = list(res = character(), y = integer(), x = integer())
    )
    parsed$res <- res_to_m(parsed$res)
  }

  parsed$x <- parsed$x * parsed$res + parsed$res / 2
  parsed$y <- parsed$y * parsed$res + parsed$res / 2

  if (as_sf) {
    crs <- unique(parsed$crs)

    if (is.null(crs)) {
      crs <- 3035
    }

    if (length(crs) > 1) {
      warning("More than one CRS parsed. Taking the first one.")
      crs <- crs[1]
    }

    parsed <- sf::st_as_sf(parsed, coords = c("x", "y"), crs = crs)
  }

  parsed
}


guess_resolution <- function(x, y) {
  x <- x[1:2000] # take a sample
  y <- y[1:2000]
  diff_x <- diff(sort(x))
  diff_y <- diff(sort(y))
  diff_x <- diff_x[!diff_x == 0]
  diff_y <- diff_y[!diff_y == 0]
  dist <- stats::median(c(diff_x, diff_y))

  if (!dist %in% res_to_m(grid_reses)) {
    stop(paste(
      "Provided coordinates are not properly aligned in a standard",
      "grid. Standard grid resolutions are {.val {grid_reses}}."
    ))
  }

  dist
}


res_to_m <- function(res) {
  is_km <- grepl("(?<=[0-9])km", res, perl = TRUE)
  numbers <- as.numeric(regex_match(res, "^[0-9]+", i = 1))
  numbers <- ifelse(is_km, numbers * 1000, numbers)
  numbers
}


m_to_res <- function(m) {
  if (m >= 1000) {
    sprintf("%skm", m / 1000)
  } else {
    sprintf("%sm", m)
  }
}


grid_reses <- c("100m", "250m", "1km", "10km", "100km")
