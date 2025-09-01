# Helper to provide a default value for NULL objects.
# From rlang::`%||%`
`%||%` <- function(a, b) if (!is.null(a)) a else b
