# Convert INSPIRE IDs between long and short formats

This function converts a vector of INSPIRE-compliant grid IDs from their
long format to the short format, or vice-versa. It automatically detects
the input format and is fully vectorized to handle large inputs
efficiently.

The long format is `CRS{epsg}RES{cellsize}mN{y}E{x}`. The short format
can be either `{cellsize}N{y}E{x}` or `{cellsize}E{x}N{y}`.

## Usage

``` r
inspire_id_format(ids, crs = 3035, axis_order = "NE")
```

## Arguments

- ids:

  A character vector of INSPIRE IDs. All IDs in the vector must be of
  the same format (either all long or all short).

- crs:

  An integer representing the EPSG code. This parameter is **only used
  when converting from the short format to the long format**. It
  defaults to `3035` (ETRS89-LAEA).

- axis_order:

  A character string specifying the coordinate order for the output.
  This parameter is **only used when converting from the long format to
  the short format**. It can be one of:

  - `"NE"` (the default) to produce the format `{cellsize}N{y}E{x}`.

  - `"EN"` to produce the format `{cellsize}E{x}N{y}`.

## Value

A character vector of the converted INSPIRE IDs.

## Examples

``` r
long_ids <- c("CRS3035RES1000mN2684000E4334000", "CRS3035RES10000mN2700000E4400000")
short_ids_ne <- c("1kmN2684E4334", "10kmN270E440")
short_ids_en <- c("1kmE4334N2684", "10kmE440N270")

# --- Long to Short ---

# Convert long to short with default "NE" order
inspire_id_format(long_ids)
#> [1] "1kmN2684E4334" "10kmN270E440" 

# Convert long to short with specified "EN" order
inspire_id_format(long_ids, axis_order = "EN")
#> [1] "1kmE4334N2684" "10kmE440N270" 

# --- Short to Long ---

# Convert short ("NE" format) to long with default CRS (3035)
inspire_id_format(short_ids_ne)
#> [1] "CRS3035RES1000mN2684000E4334000"  "CRS3035RES10000mN2700000E4400000"

# The function also correctly parses the "EN" format
inspire_id_format(short_ids_en)
#> [1] "CRS3035RES1000mN2684000E4334000"  "CRS3035RES10000mN2700000E4400000"

# Override the CRS when converting short to long
inspire_id_format(short_ids_ne, crs = 3857)
#> [1] "CRS3857RES1000mN2684000E4334000"  "CRS3857RES10000mN2700000E4400000"
```
