#' @rdname inspire_id_coords
#' @param inspire A vector of INSPIRE IDs. Can be either legacy or non-legacy.
#' @param as_sf Whether to return an object of class \code{sfc} or a dataframe.
#' @param crs An optional numeric EPSG code or `sf::st_crs` object. If provided,
#'   this CRS will be assigned to all parsed coordinates, overriding any CRS
#'   information found in long-form IDs. If `NULL` (the default), the CRS is
#'   inferred from long-form IDs. When only short-form IDs are present,
#'   the function will default to EPSG:3035 (with a warning) for both
#'   `sf` and `data.frame` outputs.
#' @export
inspire_id_to_coords <- function(inspire, as_sf = FALSE, crs = NULL) {
  # 1. Validate inputs (Basic checks only)
  if (!is.character(inspire)) stop("Input 'inspire' must be a character vector.", call. = FALSE)
  if (length(inspire) == 0) stop("Input 'inspire' cannot be an empty vector.", call. = FALSE)
  if (any(is.na(inspire))) stop("Input 'inspire' contains NA values.", call. = FALSE)
  if (any(nchar(trimws(inspire)) == 0L)) {
    stop("Input 'inspire' contains empty strings, which are not valid INSPIRE IDs.", call. = FALSE)
  }

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

  # 3. Stop if any IDs were malformed (indicated by NA in any critical field)
  if (any(is.na(parsed$cellsize)) || any(is.na(parsed$x)) || any(is.na(parsed$y))) {
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



#' Parse a vector of INSPIRE IDs (long, short, or mixed)
#' @noRd
parse_inspire_ids <- function(inspire, is_long, is_short) {
  # Use the C++ kernel for parsing (10-20x faster)
  parse_inspire_ids_rcpp(inspire, is_long, is_short)
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


m_to_res <- function(m) {
  if (m >= 1000) {
    sprintf("%skm", m / 1000)
  } else {
    sprintf("%sm", m)
  }
}
