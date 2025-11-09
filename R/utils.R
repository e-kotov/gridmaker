# Helper to provide a default value for NULL objects.
# From rlang::`%||%`
`%||%` <- function(a, b) if (!is.null(a)) a else b


regex_match <- function(text, pattern, i = NULL, ...) {
  match <- regmatches(text, regexec(pattern, text, ...))
  if (!is.null(i)) {
    match <- vapply(match, FUN.VALUE = character(1), function(x) {
      if (length(x) >= i) {
        x[[i]]
      } else {
        NA_character_
      }
    })
  }
  match
}

#' Get available RAM
#' @keywords internal
#' @return A `numeric` amount of available RAM in GB.
#' @noRd
.get_ram_gb <- function(type = NULL) {
  mem_info <- ps::ps_system_memory()

  # Convert all values to GB and round to 2 decimal places
  mem_info_gb <- lapply(mem_info, function(x) {
    if (is.numeric(x)) {
      return(round(x / 1024^3, 2))
    }
    return(x) # Keep non-numeric values as-is (like $percent)
  })

  # If type is specified, return just that value
  if (!is.null(type)) {
    if (!type %in% names(mem_info_gb)) {
      stop("Memory type '", type, "' not available on this system")
    }
    return(mem_info_gb[[type]])
  }

  # Otherwise return the whole list
  return(mem_info_gb)
}
