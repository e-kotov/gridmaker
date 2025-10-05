#' @name inspire
#' @param inspire A vector of INSPIRE IDs. Can be either legacy or non-legacy.
#' @param as_sf Whether to return an object of class \code{sfc} or a dataframe.
#' @param crs An optional numeric EPSG code or `sf::st_crs` object. If provided,
#'   this CRS will be assigned to all parsed coordinates, overriding any CRS
#'   information found in long-form IDs. If `NULL` (the default), the CRS is
#'   inferred from long-form IDs. When only short-form IDs are present,
#'   the function will default to EPSG:3035 (with a warning) for both
#'   `sf` and `data.frame` outputs.
#' @export
inspire_extract <- function(inspire, as_sf = FALSE, crs = NULL) {
  # 1. Validate all inputs first
  validate_inspire_input(inspire)

  # 2. Parse all ID formats into a standardized data frame
  is_long <- startsWith(inspire, "CRS")
  is_short <- !is_long
  if (any(is_long) && any(is_short)) {
    warning(
      "Input 'inspire' contains a mix of long and short ID formats.",
      call. = FALSE
    )
  }
  parsed <- parse_inspire_ids(inspire, is_long, is_short)

  # 3. Stop if any IDs were malformed (indicated by NA in cellsize)
  if (any(is.na(parsed$cellsize))) {
    stop(
      "One or more INSPIRE IDs had a malformed format and could not be parsed.",
      call. = FALSE
    )
  }

  # 4. Determine the final CRS based on user input and parsed data
  final_crs_obj <- determine_final_crs(parsed, user_crs = crs)

  # 5. Apply the final CRS to the data frame
  if (!is.null(final_crs_obj)) {
    final_epsg <- if (inherits(final_crs_obj, "crs")) {
      final_crs_obj$epsg
    } else {
      final_crs_obj
    }
    parsed$crs[is.na(parsed$crs)] <- final_epsg
  }

  # 6. Convert to sf object if requested
  if (as_sf) {
    parsed <- sf::st_as_sf(parsed, coords = c("x", "y"), crs = final_crs_obj)
  }

  return(parsed)
}


# --- Helper Functions (Internal, not exported) ---

#' Validate the input vector for inspire_extract
#' @noRd
validate_inspire_input <- function(inspire) {
  if (length(inspire) == 0) {
    stop("Input 'inspire' cannot be an empty vector.", call. = FALSE)
  }
  if (!is.character(inspire)) {
    stop("Input 'inspire' must be a character vector.", call. = FALSE)
  }
  if (any(is.na(inspire))) {
    stop("Input 'inspire' contains NA values.", call. = FALSE)
  }
  if (any(nchar(trimws(inspire)) == 0)) {
    stop("Input 'inspire' contains empty strings.", call. = FALSE)
  }
}

#' Parse a vector of INSPIRE IDs (long, short, or mixed)
#' @noRd
parse_inspire_ids <- function(inspire, is_long, is_short) {
  parsed <- data.frame(
    crs = rep(NA_integer_, length(inspire)),
    cellsize = rep(NA_real_, length(inspire)),
    y = rep(NA_real_, length(inspire)),
    x = rep(NA_real_, length(inspire))
  )

  # Parse long IDs
  if (any(is_long)) {
    long_indices <- which(is_long)
    parsed_long <- utils::strcapture(
      "^CRS([0-9]+)RES([0-9]+)mN([0-9]+)E([0-9]+)$",
      inspire[long_indices],
      proto = list(
        crs = integer(),
        cellsize = numeric(),
        y = numeric(),
        x = numeric()
      )
    )
    parsed[long_indices, ] <- parsed_long
  }

  # Parse short IDs (handling both N..E and E..N formats)
  if (any(is_short)) {
    short_indices <- which(is_short)
    parsed_short_ne <- utils::strcapture(
      "^([0-9]+k?m)N([0-9]+)E([0-9]+)$",
      inspire[short_indices],
      proto = list(cellsize_str = character(), y = numeric(), x = numeric())
    )

    failed_indices <- which(is.na(parsed_short_ne$cellsize_str))
    if (length(failed_indices) > 0) {
      original_failed_inspire <- inspire[short_indices[failed_indices]]
      parsed_short_en <- utils::strcapture(
        "^([0-9]+k?m)E([0-9]+)N([0-9]+)$",
        original_failed_inspire,
        proto = list(cellsize_str = character(), x = numeric(), y = numeric())
      )
      parsed_short_ne[failed_indices, ] <- parsed_short_en[, c(
        "cellsize_str",
        "y",
        "x"
      )]
    }

    parsed[short_indices, "cellsize"] <- res_to_m(parsed_short_ne$cellsize_str)
    parsed[short_indices, "y"] <- parsed_short_ne$y
    parsed[short_indices, "x"] <- parsed_short_ne$x
  }

  return(parsed)
}

#' Determine the final CRS based on user input and parsed data
#' @noRd
determine_final_crs <- function(parsed_df, user_crs) {
  if (!is.null(user_crs)) {
    return(user_crs)
  }

  unique_parsed_crs <- unique(parsed_df$crs)
  crs_valid <- unique_parsed_crs[!is.na(unique_parsed_crs)]

  if (length(crs_valid) > 1) {
    stop(
      sprintf(
        "INSPIRE identifiers contain more than one CRS (%s). Specify the 'crs' argument to override.",
        paste(paste0("EPSG:", crs_valid), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  if (length(crs_valid) == 1) {
    return(crs_valid[1])
  }

  # If we are here, no valid CRS was parsed. Check if any short IDs existed.
  if (any(is.na(parsed_df$crs))) {
    warning(
      "CRS not specified for short-form IDs. Defaulting to EPSG:3035.",
      call. = FALSE
    )
    return(3035)
  }

  return(NULL) # No CRS needed or found
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
