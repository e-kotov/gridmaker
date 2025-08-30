

<!-- README.md is generated from README.Rmd. Please edit that file -->

# gridmaker

<!-- badges: start -->

[![Project Status:
Active](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/gridmaker)](https://CRAN.R-project.org/package=gridmaker)
[![R-CMD-check](https://github.com/e-kotov/gridmaker/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/e-kotov/gridmaker/actions/workflows/R-CMD-check.yaml)
[![R-CMD-check](https://github.com/e-kotov/gridmaker/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/e-kotov/gridmaker/actions/workflows/R-CMD-check.yaml)
<!-- [![Codecov test coverage](https://codecov.io/gh/e-kotov/gridmaker/graph/badge.svg)](https://app.codecov.io/gh/e-kotov/gridmaker) -->
<!-- badges: end -->

Creates GISCO compatible and INSPIRE-compliant grids with IDs that look
like ‘CRS3035RES1000mN3497000E4448000’ or ‘1kmN3497E4447’. Input can be
`sf`, `sfc` objects or bounding boxes. Outout can be `sf` polygons, `sf`
centroids, or just `data.frame` with grid cell center or bottom left
corner coordinates. The resulting grids are always aligned to rounded
coorindates as per INSPIRE requirements (see here
<https://github.com/INSPIRE-MIF/technical-guidelines/tree/main/data/su>).

> [!TIP]
>
> Instead of downloading 2+ GB of Eurostat GISCO grid data, then
> extracting it for your region of interest, you can create grids on the
> fly with this package.

## Installation

You can install the development version of gridmaker from
[GitHub](https://github.com/e-kotov/gridmaker) with:

``` r
# install.packages("pak")
pak::pak("e-kotov/gridmaker")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(gridmaker)
library(sf)


# Load the sample data from the sf package
nc_raw <- st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)

# Define target projected CRS and cell size
target_crs <- 5070 # NAD83 / Conus Albers
cellsize_m <- 10000 # 10 km

# Project the data
nc <- st_transform(nc_raw, target_crs)

# Create a grid covering the data
nc_grid <- create_grid(
  grid_extent = nc,
  cellsize_m = cellsize_m,
  output_type = "sf_polygons",
  clip_to_input = TRUE
)

head(nc_grid, 3)
```

``` r
Simple feature collection with 3 features and 5 fields
Geometry type: POLYGON
Dimension:     XY
Bounding box:  xmin: 1570000 ymin: 1340000 xmax: 1600000 ymax: 1360000
Projected CRS: NAD83 / Conus Albers
   id                       geometry   X_LLC   Y_LLC                      GRD_ID_LONG GRD_ID_SHORT
1  54 POLYGON ((1580000 1340000, ... 1580000 1340000 CRS5070RES10000mN1340000E1580000 10kmN134E158
2  55 POLYGON ((1590000 1340000, ... 1590000 1340000 CRS5070RES10000mN1340000E1590000 10kmN134E159
3 132 POLYGON ((1570000 1350000, ... 1570000 1350000 CRS5070RES10000mN1350000E1570000 10kmN135E157
```

## Citation

To cite package ‘gridmaker’ in publications use:

Kotov E (2025). *gridmaker: Create INSPIRE-compliant grids with IDs*. R
package version 0.0.0.9000, <https://github.com/e-kotov/gridmaker>.

BibTeX:

    @Manual{,
      title = {gridmaker: Create INSPIRE-compliant grids with IDs},
      author = {Egor Kotov},
      year = {2025},
      note = {R package version 0.0.0.9000},
      url = {https://github.com/e-kotov/gridmaker},
    }
