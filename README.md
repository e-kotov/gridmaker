

<!-- README.md is generated from README.Rmd. Please edit that file -->

# gridmaker Create INSPIRE-compliant grids with IDs <a href="https://www.ekotov.pro/gridmaker/"><img src="man/figures/logo.png" align="right" width="200" alt="gridmaker website" /></a>

badges: start

[![Project Status:
Active](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![CRAN
status](https://www.r-pkg.org/badges/version/gridmaker)](https://CRAN.R-project.org/package=gridmaker)
[![gridmaker status
badge](https://e-kotov.r-universe.dev/gridmaker/badges/version)](https://e-kotov.r-universe.dev/gridmaker)
[![R-CMD-check](https://github.com/e-kotov/gridmaker/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/e-kotov/gridmaker/actions/workflows/R-CMD-check.yaml)
[![R-CMD-check](https://github.com/e-kotov/gridmaker/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/e-kotov/gridmaker/actions/workflows/R-CMD-check.yaml)
[![pkgcheck](https://github.com/e-kotov/gridmaker/workflows/pkgcheck/badge.svg)](https://github.com/e-kotov/gridmaker/actions?query=workflow%3Apkgcheck)
[![Codecov test
coverage](https://codecov.io/gh/e-kotov/gridmaker/graph/badge.svg)](https://app.codecov.io/gh/e-kotov/gridmaker)
<!-- badges: end -->

Creates GISCO compatible and INSPIRE-compliant grids with IDs that look
like ‘CRS3035RES1000mN3497000E4448000’ or ‘1kmN3497E4447’. Input can be
`sf`, `sfc` objects or bounding boxes. Output can be `sf` polygons, `sf`
centroids, or just `data.frame` with grid cell center or bottom left
corner coordinates. The resulting grids are always aligned to rounded
coordinates as per INSPIRE requirements (see here
<https://github.com/INSPIRE-MIF/technical-guidelines/tree/main/data/su>).

![Create GISCO/INSPIRE grids on the fly](man/figures/malta.png)

> [!TIP]
>
> Instead of
> [downloading](https://ec.europa.eu/eurostat/web/gisco/geodata/grids)
> 1.5 GB (points) or 2.6 GB (polygons) of Eurostat GISCO grid data, then
> extracting it for your region of interest, you can create grids on the
> fly with this package. The resulting grid will not carry all [the
> attributes of the original GISCO
> grid](https://gisco-services.ec.europa.eu/grid/GISCO_grid_metadata.pdf),
> such as NUTS codes, population count, distance to the coast, country
> land border, etc., but it will have identical geometry and IDs.

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
  cellsize_m = 5000,
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
     id   X_LLC   Y_LLC                      GRD_ID_LONG GRD_ID_SHORT                       geometry
54   54 1580000 1340000 CRS5070RES10000mN1340000E1580000 10kmN134E158 POLYGON ((1580000 1340000, ...
55   55 1590000 1340000 CRS5070RES10000mN1340000E1590000 10kmN134E159 POLYGON ((1590000 1340000, ...
132 132 1570000 1350000 CRS5070RES10000mN1350000E1570000 10kmN135E157 POLYGON ((1570000 1350000, ...
```

``` r
plot(st_geometry(nc_grid), border = 'grey20')
plot(st_geometry(nc), add = TRUE, border = 'red', lwd = 2)
```

<img src="man/figures/us-grid.png" width="400"
alt="10km grid for North Carolina" />

## Citation

To cite package ‘gridmaker’ in publications use:

Kotov E (2025). *gridmaker: Create INSPIRE-compliant grids with IDs*.
<https://github.com/e-kotov/gridmaker>.

BibTeX:

    @Manual{gridmaker,
      title = {gridmaker: Create INSPIRE-compliant grids with IDs},
      author = {Egor Kotov},
      year = {2025},
      url = {https://github.com/e-kotov/gridmaker},
    }

> [!NOTE]
>
> For original Eurostat’s command line utility to create GISCO grids,
> see <https://github.com/eurostat/GridMaker>.
