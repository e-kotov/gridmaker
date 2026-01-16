# Create or Reconstruct an INSPIRE Grid

Generates a standard-compliant spatial grid aligned to the CRS origin.
This function acts as a unified interface:

- If `x` is a spatial object (extent), it creates a new grid (calling
  `inspire_grid_from_extent`).

- If `x` is a character vector (INSPIRE IDs), it reconstructs the grid
  (calling `inspire_grid_from_ids`).

It combines high performance for large areas (using `sfheaders`) with a
flexible and robust set of features for input handling and output
formatting, including INSPIRE-compliant grid IDs and automatic parallel
processing with `mirai` and `future` backends.

This function takes a vector of INSPIRE-compliant IDs and derives a
regular spatial grid from it. For generating a spatial grid from a
spatial extent, see `inspire_grid_from_extent`.

## Usage

``` r
inspire_grid(
  x,
  cellsize_m = NULL,
  crs = NULL,
  output_type = "sf_polygons",
  clip_to_input = FALSE,
  use_convex_hull = FALSE,
  buffer_m = 0,
  id_format = "both",
  axis_order = "NE",
  include_llc = TRUE,
  point_type = "centroid",
  parallel = "auto",
  quiet = getOption("gridmaker.quiet", FALSE),
  dsn = NULL,
  layer = NULL,
  max_memory_gb = NULL,
  include_rat = FALSE,
  ...
)

# S3 method for class 'sf'
inspire_grid(
  x,
  cellsize_m = NULL,
  crs = NULL,
  output_type = "sf_polygons",
  clip_to_input = FALSE,
  use_convex_hull = FALSE,
  buffer_m = 0,
  id_format = "both",
  axis_order = "NE",
  include_llc = TRUE,
  point_type = "centroid",
  parallel = "auto",
  quiet = getOption("gridmaker.quiet", FALSE),
  dsn = NULL,
  layer = NULL,
  max_memory_gb = NULL,
  include_rat = FALSE,
  ...
)

# S3 method for class 'sfc'
inspire_grid(
  x,
  cellsize_m = NULL,
  crs = NULL,
  output_type = "sf_polygons",
  clip_to_input = FALSE,
  use_convex_hull = FALSE,
  buffer_m = 0,
  id_format = "both",
  axis_order = "NE",
  include_llc = TRUE,
  point_type = "centroid",
  parallel = "auto",
  quiet = getOption("gridmaker.quiet", FALSE),
  dsn = NULL,
  layer = NULL,
  max_memory_gb = NULL,
  include_rat = FALSE,
  ...
)

# S3 method for class 'bbox'
inspire_grid(
  x,
  cellsize_m = NULL,
  crs = NULL,
  output_type = "sf_polygons",
  clip_to_input = FALSE,
  use_convex_hull = FALSE,
  buffer_m = 0,
  id_format = "both",
  axis_order = "NE",
  include_llc = TRUE,
  point_type = "centroid",
  parallel = "auto",
  quiet = getOption("gridmaker.quiet", FALSE),
  dsn = NULL,
  layer = NULL,
  max_memory_gb = NULL,
  include_rat = FALSE,
  ...
)

# S3 method for class 'numeric'
inspire_grid(
  x,
  cellsize_m = NULL,
  crs = NULL,
  output_type = "sf_polygons",
  clip_to_input = FALSE,
  use_convex_hull = FALSE,
  buffer_m = 0,
  id_format = "both",
  axis_order = "NE",
  include_llc = TRUE,
  point_type = "centroid",
  parallel = "auto",
  quiet = getOption("gridmaker.quiet", FALSE),
  dsn = NULL,
  layer = NULL,
  max_memory_gb = NULL,
  include_rat = FALSE,
  ...
)

# S3 method for class 'matrix'
inspire_grid(
  x,
  cellsize_m = NULL,
  crs = NULL,
  output_type = "sf_polygons",
  clip_to_input = FALSE,
  use_convex_hull = FALSE,
  buffer_m = 0,
  id_format = "both",
  axis_order = "NE",
  include_llc = TRUE,
  point_type = "centroid",
  parallel = "auto",
  quiet = getOption("gridmaker.quiet", FALSE),
  dsn = NULL,
  layer = NULL,
  max_memory_gb = NULL,
  include_rat = FALSE,
  ...
)

# S3 method for class 'character'
inspire_grid(
  x,
  cellsize_m = NULL,
  crs = NULL,
  output_type = "sf_polygons",
  clip_to_input = FALSE,
  use_convex_hull = FALSE,
  buffer_m = 0,
  id_format = "both",
  axis_order = "NE",
  include_llc = TRUE,
  point_type = "llc",
  parallel = "auto",
  quiet = getOption("gridmaker.quiet", FALSE),
  dsn = NULL,
  layer = NULL,
  max_memory_gb = NULL,
  include_rat = FALSE,
  ...
)

inspire_grid_from_extent(
  grid_extent,
  cellsize_m,
  crs = NULL,
  output_type = "sf_polygons",
  clip_to_input = FALSE,
  use_convex_hull = FALSE,
  buffer_m = 0,
  id_format = "both",
  axis_order = "NE",
  include_llc = TRUE,
  point_type = "centroid",
  parallel = "auto",
  quiet = getOption("gridmaker.quiet", FALSE),
  dsn = NULL,
  layer = NULL,
  max_memory_gb = NULL,
  include_rat = FALSE,
  ...
)

inspire_grid_from_ids(
  ids,
  point_type = c("llc", "centroid"),
  output_type = c("sf_polygons", "sf_points", "dataframe"),
  include_llc = TRUE,
  id_format = c("both", "long", "short"),
  axis_order = c("NE", "EN"),
  quiet = getOption("gridmaker.quiet", FALSE),
  dsn = NULL,
  layer = NULL,
  ...
)
```

## Arguments

- x:

  The main input object: either a spatial object (extent) or a character
  vector (INSPIRE IDs).

- cellsize_m:

  A single integer representing the grid cell size in metres (e.g., 1000
  for a 1 km grid). Required for spatial inputs.

- crs:

  The coordinate reference system (CRS) for the output grid. Accepts
  various formats handled by
  [`sf::st_crs()`](https://r-spatial.github.io/sf/reference/st_crs.html):
  an integer or numeric EPSG code (e.g., `3035`), a string
  representation like `"epsg:3035"`, or a `crs` object. If `NULL`
  (default), the CRS is inherited from the spatial input. If the input
  also lacks a CRS, the function will stop with an error.

- output_type:

  The class of the output object: `"sf_polygons"` (default) creates a
  spatial object with polygon geometries, `"sf_points"` creates an `sf`
  object with point geometries, `"dataframe"` creates a data frame with
  grid cell centroid coordinates (`X_centroid`, `Y_centroid`), and
  `"spatraster"` creates a
  [`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  object with grid cell IDs stored as factor levels (Raster Attribute
  Table). **Note:** `"spatraster"` is only supported by
  `inspire_grid_from_extent()`, not by `inspire_grid_from_ids()`.

- clip_to_input:

  A logical value. If `TRUE`, the grid is filtered to include only cells
  that intersect the spatial input. This does not cut cell geometries.

- use_convex_hull:

  A logical value. If `TRUE` and `clip_to_input` is `TRUE`, the grid is
  clipped to the convex hull of the input geometry, which can be faster
  and simpler than using a complex polygon.

- buffer_m:

  A numeric value. If `clip_to_input` is `TRUE`, this specifies a buffer
  distance in metres to apply to the spatial input before clipping.
  Defaults to `0` (no buffer).

- id_format:

  A character string specifying which grid cell IDs to generate. Options
  are `"both"` (default), `"long"`, `"short"`, or `"none"`.

- axis_order:

  A character string specifying the coordinate order for the output
  Short INSPIRE IDs. This parameter is **only used when `id_format` is
  `"short"` or `"both"`**. It can be one of:

  - `"NE"` (the default) to produce the format `{cellsize}N{y}E{x}`.

  - `"EN"` to produce the format `{cellsize}E{x}N{y}` (e.g. this format
    is used in [Danish national
    grid](https://www.dst.dk/en/TilSalg/produkter/geodata/kvadratnet)).

- include_llc:

  A logical value. If `TRUE` (default), columns for the lower-left
  corner coordinates (`X_LLC`, `Y_LLC`) of each cell are included in the
  output.

- point_type:

  A character string determining the location of the points when
  `output_type = "sf_points"`: `"centroid"` for the center of the cell,
  or `"llc"` for the lower-left corner. Default is `"llc"`.

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
  function, for example: `mirai::daemons(8)` or
  `future::plan("multisession", workers = 8)`. **Performance tip:**
  Benchmarks show 4-8 workers provide optimal performance for most grid
  sizes. Using \>8 workers typically yields diminishing returns due to
  I/O bottlenecks. The function automatically limits active workers for
  small grids to minimize overhead: \<50k cells use max 4 workers,
  \<500k cells use max 8 workers, \<2M cells use max 16 workers. This
  automatic limiting can be overridden by setting
  `options(gridmaker.tile_multiplier)`. **Note:** Parallel processing
  support depends on the backend and output type:

  - **`mirai` backend:** Supports parallel processing for all outputs,
    including efficient disk streaming. This is the recommended modern
    backend that also runs asynchronously, which allows it to pass
    chunks of grid that are ready to the disk writer and therefore makes
    streaming to disk efficient, as there is no need to all data in
    memory first.

  - **`future` backend:** Supports parallel processing **only** for
    in-memory vector generation (`sf`, `dataframe`). It does not support
    raster output or disk-based streaming (falls back to sequential),
    because it would need to first accumualte all data in memory before
    writing to disk, negating the benefits of streaming.

- quiet:

  Logical value. If `TRUE`, all progress messages and progress bars are
  suppressed. Defaults to `FALSE`.

- dsn:

  The destination for the output grid. For sf objects, this is passed to
  [`sf::st_write`](https://r-spatial.github.io/sf/reference/st_write.html).
  For `spatraster` output, this uses
  [`terra::writeRaster`](https://rspatial.github.io/terra/reference/writeRaster.html).
  This can be a file path (e.g., `"path/to/grid.gpkg"` for vector data
  or `"path/to/grid.tif"` for raster data) or a database connection
  string. If `dsn` is provided, the grid is written to the specified
  location instead of being returned as an object.

  **Supported vector formats for chunked disk writes:**

  - `.gpkg` (GeoPackage) - **Recommended** - Best balance of speed,
    compatibility, and modern features

  - `.parquet`, `.geoparquet` (GeoParquet) - Modern columnar format,
    excellent for large grids (requires sf 1.0+/GDAL 3.5+)

  - `.shp` (Shapefile) - Widely used, fast writes, but has limitations
    (10-char field names, 2GB limit)

  - `.geojson`, `.json` (GeoJSON) - Web-friendly, works but slower for
    large grids

  - `.geojsonl`, `.geojsonseq` (GeoJSONSeq) - Newline-delimited GeoJSON

  - `.sqlite` (SQLite/SpatiaLite) - Database format (GeoPackage is built
    on SQLite)

  - `.fgb` (FlatGeobuf) - Cloud-optimized format

  - `.csv`, `.tsv`, `.txt` (for dataframe output only)

  Other formats not listed have not been tested and will generate a
  warning.

- layer:

  The name of the grid layer, passed directly to
  [`sf::st_write`](https://r-spatial.github.io/sf/reference/st_write.html).
  Its interpretation depends on the destination driver. For a GeoPackage
  file, this will be the layer name. If `dsn` is a file path and `layer`
  is not specified, it defaults to the file's base name.

- max_memory_gb:

  A numeric value. Maximum memory in gigabytes to use for grid creation.
  Default is `NULL`, in which case there is an automatic limit based on
  **available free system memory** (not total system RAM). Using this
  argument allows manual override, which is recommended on certain HPC
  (High Performance Computing) systems where jobs are allocated a fixed
  amount of memory that is less than the total free memory of the
  allocated node.

- include_rat:

  Logical. If `TRUE`, generate a Raster Attribute Table (RAT) mapping
  numeric cell IDs to INSPIRE grid ID strings. Default is `FALSE`.

  **What is a RAT?** A Raster Attribute Table stores metadata (like
  INSPIRE IDs) for each unique raster value. Without RAT, raster cells
  contain only numeric IDs (1, 2, 3...). With RAT, software like QGIS/R
  can display the IDs as human-readable labels.

  **Format-specific behavior:**

  - **GeoTIFF (.tif):** RAT stored in `.tif.aux.xml` sidecar file (XML).
    **Warning:** This sidecar can be **larger than the TIFF itself** for
    large grids. For chunked/streaming writes, requires a second pass
    (slower). Consider KEA or Erdas Imagine formats for large grids with
    labels.

  - **KEA (.kea), Erdas Imagine (.img):** RAT embedded natively.
    **Recommended** for large grids requiring labels.

  - **NetCDF (.nc), HDF5 (.hdf):** RAT **not supported**. An error is
    raised if `include_rat = TRUE`.

- ...:

  Additional arguments passed to backend handlers. When writing to text
  files (e.g., .csv, .tsv) via `dsn`, these arguments are passed to
  [`write_delim`](https://readr.tidyverse.org/reference/write_delim.html)
  (e.g., `na = "NA"`, `quote = "all"`). When writing to spatial files
  via `dsn`, these are passed to
  [`st_write`](https://r-spatial.github.io/sf/reference/st_write.html).
  For `output_type = "spatraster"` writing, these are passed to
  [`writeRaster`](https://rspatial.github.io/terra/reference/writeRaster.html).
  For streaming backends (`mirai` or sequential), this can include
  `max_cells_per_chunk` to control memory usage.

- grid_extent:

  The spatial object defining the extent. Can be an `sf` object, `sfc`
  geometry collection, `bbox`, `numeric` vector (as c(xmin, ymin, xmax,
  ymax)), or `matrix`.

- ids:

  A character vector of INSPIRE-compliant grid cell IDs (e.g.,
  `"CRS3035RES100000mN26E43"`).

## Value

If `dsn` is `NULL` (the default), an `sf` object, `data.frame`, or
`SpatRaster` representing the grid. If `dsn` is specified, the function
writes the grid to a file and returns `invisible(dsn)`.

An `sf` object or `data.frame` representing the grid derived from the
INSPIRE IDs. If `dsn` is specified, returns `invisible(dsn)`.

## Details

This function creates a spatial grid aligned to the CRS origin, with
support for clipping to input geometries, parallel processing, and
multiple output formats.

## Examples

``` r
library(sf)
#> Linking to GEOS 3.12.1, GDAL 3.8.4, PROJ 9.4.0; sf_use_s2() is TRUE
# Load the sample data from the sf package
nc_raw <- st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)

# Define target projected CRS and cell size
target_crs <- 5070 # NAD83 / Conus Albers
cellsize_m <- 10000 # 10 km

# Project the data
nc <- st_transform(nc_raw, target_crs)

# Create a grid covering the data
nc_grid <- inspire_grid_from_extent(
  grid_extent = nc,
  cellsize_m = cellsize_m,
  output_type = "sf_polygons",
  clip_to_input = TRUE
)
#> No parallel backend detected. Running in sequential mode.
#> Tip: Your system has 4 cores. Configure a parallel backend for faster grid generation.

# Or using the S3 generic
nc_grid <- inspire_grid(nc, cellsize_m = cellsize_m, clip_to_input = TRUE)
#> No parallel backend detected. Running in sequential mode.
#> Tip: Your system has 4 cores. Configure a parallel backend for faster grid generation.

head(nc_grid, 3)
#> Simple feature collection with 3 features and 4 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 1570000 ymin: 1340000 xmax: 1600000 ymax: 1360000
#> Projected CRS: NAD83 / Conus Albers
#>     X_LLC   Y_LLC                      GRD_ID_LONG GRD_ID_SHORT
#> 1 1580000 1340000 CRS5070RES10000mN1340000E1580000 10kmN134E158
#> 2 1590000 1340000 CRS5070RES10000mN1340000E1590000 10kmN134E159
#> 3 1570000 1350000 CRS5070RES10000mN1350000E1570000 10kmN135E157
#>                         geometry
#> 1 POLYGON ((1580000 1340000, ...
#> 2 POLYGON ((1590000 1340000, ...
#> 3 POLYGON ((1570000 1350000, ...
library(sf)

inspire <- c(
  "CRS3035RES100000mN26E43", "CRS3035RES100000mN26E44",
  "CRS3035RES100000mN27E41", "CRS3035RES100000mN27E42",
  "CRS3035RES100000mN27E43", "CRS3035RES100000mN27E44"
)

grid <- inspire_grid_from_ids(inspire)
plot(grid$geometry)


# Or using the S3 generic
grid <- inspire_grid(inspire)
plot(grid$geometry)
```
