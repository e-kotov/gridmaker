# Generate INSPIRE IDs

Given pairs of coordinates, generates their INSPIRE grid representation.
Given INSPIRE identifiers, can also extract the X and Y coordinates.

An INSPIRE ID contains information about the CRS, cell size and the
ETRS89-LAEA coordinates of the lower-left corner of the grid cell in its
format.

    CRS3035{cellsize}mN{y}E{x} # long format
        {cellsize}N{y}E{x}         # short format (NE order)
        {cellsize}E{x}N{y}         # short format (EN order)

The long format always uses meters while the short format aggregates
cell sizes greater or equal to 1000m to km.

## Usage

``` r
inspire_extract(inspire, as_sf = FALSE, crs = NULL)

inspire_generate(
  coords,
  cellsize_m = NULL,
  short = FALSE,
  axis_order = "NE",
  llc = FALSE,
  tolerance = 1e-06,
  sample = 2000
)
```

## Arguments

- inspire:

  A vector of INSPIRE IDs. Can be either legacy or non-legacy.

- as_sf:

  Whether to return an object of class `sfc` or a dataframe.

- crs:

  An optional numeric EPSG code or
  [`sf::st_crs`](https://r-spatial.github.io/sf/reference/st_crs.html)
  object. If provided, this CRS will be assigned to all parsed
  coordinates, overriding any CRS information found in long-form IDs. If
  `NULL` (the default), the CRS is inferred from long-form IDs. When
  only short-form IDs are present, the function will default to
  EPSG:3035 (with a warning) for both `sf` and `data.frame` outputs.

- coords:

  A list, matrix, or dataframe where the X and Y coordinates are either
  in the columns `"x"` and `"y"` or in the first and second column
  position, respectively. Column names are converted to lowercase.

  Can also be a `sf`/`sfc` object in which case the coordinates are
  extracted using
  [`st_coordinates`](https://r-spatial.github.io/sf/reference/st_coordinates.html).

- cellsize_m:

  A single integer representing the grid cell size in metres (e.g., 1000
  for a 1 km grid).

- short:

  If `TRUE`, generates short INSPIRE ID. Defaults to `FALSE`.

- axis_order:

  A character string specifying the coordinate order for the output.
  This parameter is **only used when `short = TRUE`**. It can be one of:

  - `"NE"` (the default) to produce the format `{cellsize}N{y}E{x}`.

  - `"EN"` to produce the format `{cellsize}E{x}N{y}`.

- llc:

  Do the coordinates in `coords` represent the lower-left corners of
  their cells? If `FALSE`, subtracts each coordinate by half of
  `cellsize_m`. If `TRUE`, leaves them as-is. Defaults to `FALSE`, i.e.,
  treat coordinates as cell centroids.

- tolerance:

  If `res` is `NULL`, controls the maximum acceptable difference between
  calculated cell spacings to consider them uniform. Defaults to `1e-6`.

- sample:

  If `res` is `NULL`, specifies the number of points to guess a
  resolution from. Defaults to 2000 to keep performance high. Increase
  this value if you are uncertain about the quality of your data.

## Value

`inspire_generate` returns a character vector containing the INSPIRE
identifiers.

`inspire_extract` returns a dataframe or
[`sf`](https://r-spatial.github.io/sf/reference/sf.html) dataframe (if
`as_sf = TRUE`) containing the points extracted from the INSPIRE
identifiers and information about the CRS and cell sizes. Note that the
returned coordinates are always the centers of the grid cells as opposed
to the lower-left corners.

## Examples

``` r
# Generate IDs from a dataframe
coords <- data.frame(x = c(4334100, 4334200), y = 2684000)
gen <- inspire_generate(coords, llc = TRUE, cellsize_m = 100)
ext <- inspire_extract(gen)[c("x", "y")]
# Note: inspire_extract gives cell centers, so this won't be identical if llc=TRUE

# Generate long format IDs
inspire_generate(coords, llc = TRUE, cellsize_m = 100)
#> [1] "CRS3035RES100mN2684000E4334100" "CRS3035RES100mN2684000E4334200"

# Generate short format IDs with default "NE" axis order
inspire_generate(coords, llc = TRUE, cellsize_m = 1000, short = TRUE)
#> [1] "1kmN2684E4334" "1kmN2684E4334"

# Generate short format IDs with "EN" axis order
inspire_generate(coords, llc = TRUE, cellsize_m = 1000, short = TRUE, axis_order = "EN")
#> [1] "1kmE4334N2684" "1kmE4334N2684"

# Extract coordinates from short ID strings
inspire_extract("100mN34000E44000", crs = 3035)
#>    crs cellsize     y     x
#> 1 3035      100 34000 44000

# Generate IDs from an sf dataframe
if (requireNamespace("sf", quietly = TRUE)) {
  coords_df <- data.frame(x = c(4334100, 4334200), y = 2684000)
  coords_sf <- sf::st_as_sf(coords_df, coords = c("x", "y"), crs = 3035)
  inspire_generate(coords_sf, cellsize_m = 1000)
}
#> [1] "CRS3035RES1000mN2683500E4333600" "CRS3035RES1000mN2683500E4333700"
```
