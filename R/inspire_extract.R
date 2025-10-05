#' @name inspire
#' @param inspire A vector of INSPIRE IDs. Can be either legacy or non-legacy.
#' @param as_sf Whether to return an object of class \code{sfc} or a dataframe.
#' @export
inspire_extract <- function(inspire, as_sf = FALSE) {
  if (all(startsWith(inspire, "CRS"))) {
    parsed <- utils::strcapture(
      "^CRS([0-9]+)RES([0-9]+)mN([0-9]+)E([0-9]+)$",
      x = inspire,
      proto = list(crs = integer(), cellsize = numeric(), y = numeric(), x = numeric())
    )
  } else {
    parsed <- utils::strcapture(
      "^([0-9]+k?m)N([0-9]+)E([0-9]+)$",
      x = inspire,
      proto = list(res = character(), y = numeric(), x = numeric())
    )
    parsed$res <- res_to_m(parsed$cellsize)
  }

  if (as_sf) {
    crs <- unique(parsed$crs)

    if (is.null(crs)) {
      crs <- 3035
    }

    if (length(crs) > 1) {
      stop(sprintf(
        "INSPIRE identifiers contain more than one CRS (%s).",
        paste(paste0("EPSG:", crs), collapse = ", ")
      ))
      crs <- crs[1]
    }

    parsed <- sf::st_as_sf(parsed, coords = c("x", "y"), crs = crs)
  }

  parsed
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
