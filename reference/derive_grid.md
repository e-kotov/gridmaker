# Convert INSPIRE IDs into a spatial grid

This function takes a vector of INSPIRE-compliant IDs and derives a
regular spatial grid from it. For generating a spatial grid from a
spatial extent, see
[`create_grid`](http://www.ekotov.pro/gridmaker/reference/create_grid.md).

## Usage

``` r
derive_grid(
  ids,
  point_type = c("llc", "centroid"),
  output_type = c("sf_polygons", "sf_points", "dataframe"),
  include_llc = TRUE,
  parallel = FALSE,
  quiet = FALSE
)
```

## Arguments

- ids:

  A vector containing character strings of INSPIRE-compliant IDs. Can be
  either short or long INSPIRE IDs.

- point_type:

  A character string, used only when `output_type = "sf_points"`.
  Determines the location of the points: `"centroid"` for the center of
  the cell, or `"llc"` (default) for the lower-left corner.

- output_type:

  The class of the output object: `"sf_polygons"` (default) creates a
  spatial object with polygon geometries, `"sf_points"` creates an `sf`
  object with point geometries, and `"dataframe"` creates a data frame
  with grid cell centroid coordinates (`X_centroid`, `Y_centroid`).

- include_llc:

  A logical value. If `TRUE` (default), columns for the lower-left
  corner coordinates (`X_LLC`, `Y_LLC`) of each cell are included in the
  output.

- parallel:

  Controls parallel execution. Options are:

  - **`'auto'` (default):** Automatically detects and uses a configured
    `mirai` or `future` backend if one is available. If both are set, it
    prefers the one with more available workers and issues a warning. If
    neither is configured, it runs sequentially.

  - **`TRUE`:** Forces the function to attempt parallel execution. It
    will raise an error if a valid parallel backend (with \>1 worker) is
    not configured.

  - **`FALSE`:** Forces the function to run in single-threaded
    sequential mode.

  For parallelism, you must configure a backend *before* calling this
  function, for example: `mirai::daemons(4)` or
  `future::plan("multisession")`.

- quiet:

  logical value. If ‘TRUE’, all progress messages and progress bars are
  suppressed. Defaults to ‘FALSE’.

## Value

If `dsn` is `NULL` (the default), an `sf` object or `data.frame`
representing the grid. If `dsn` is specified, the function writes the
grid to a file and returns `invisible(dsn)`.

## Examples

``` r
library(sf)

inspire <- c(
  "CRS3035RES100000mN26E43", "CRS3035RES100000mN26E44",
  "CRS3035RES100000mN27E41", "CRS3035RES100000mN27E42",
  "CRS3035RES100000mN27E43", "CRS3035RES100000mN27E44"
)

grid <- derive_grid(inspire)
plot(grid$geometry)
```
