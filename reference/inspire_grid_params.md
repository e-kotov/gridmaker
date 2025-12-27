# Common parameters for INSPIRE grid functions

This is a documentation template containing common parameter definitions
shared across inspire_grid functions. This documentation is not exported
and exists solely to provide @inheritParams targets.

## Arguments

- crs:

  The coordinate reference system (CRS) for the output grid. Accepts
  various formats handled by
  [`sf::st_crs()`](https://r-spatial.github.io/sf/reference/st_crs.html):
  an integer or numeric EPSG code (e.g., `3035`), a string
  representation like `"epsg:3035"`, or a `crs` object. If `NULL`
  (default), the CRS is inherited from the spatial input. If the input
  also lacks a CRS, the function will stop with an error.

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

  - `.shp` (Shapefile) - Widely used, fast writes, but has limitations
    (10-char field names, 2GB limit)

  - `.geojson`, `.json` (GeoJSON) - Web-friendly, works but slower for
    large grids

  - `.geojsonl`, `.geojsonseq` (GeoJSONSeq) - Newline-delimited GeoJSON

  - `.sqlite` (SQLite/SpatiaLite) - Database format (GeoPackage is built
    on SQLite)

  - `.fgb` (FlatGeobuf) - Cloud-optimized format

  - `.gdb` (OpenFileGDB) - ESRI FileGDB format

  - `.csv`, `.tsv`, `.txt` (for dataframe output only)

  Other formats not listed have not been tested and will generate a
  warning.

- layer:

  The name of the grid layer, passed directly to
  [`sf::st_write`](https://r-spatial.github.io/sf/reference/st_write.html).
  Its interpretation depends on the destination driver. For a GeoPackage
  file, this will be the layer name. If `dsn` is a file path and `layer`
  is not specified, it defaults to the file's base name.

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

- max_memory_gb:

  A numeric value. Maximum memory in gigabytes to use for grid creation.
  Default is `NULL`, in which case there is an automatic limit based on
  **available free system memory** (not total system RAM).
