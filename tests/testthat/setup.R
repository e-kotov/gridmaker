library(sf)

# --- Test Data Preparation ---
# Load the sample data
nc_raw <- st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)

# Define target CRS and a larger cell size for faster tests
TARGET_CRS <- 5070 # NAD83 / Conus Albers
CELLSIZE <- 10000 # 10 km

# Project the data
nc <- st_transform(nc_raw, TARGET_CRS)
