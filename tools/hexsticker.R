# --- Load all required libraries ---
library(hexSticker)
library(ggplot2)
library(sf)
library(giscoR)
library(gridmaker)

# Create the 'private' directory if it doesn't exist
if (!dir.exists("private")) {
  dir.create("private")
}

# --- 1. Define the professional color palette ---
# A modern, sophisticated slate-blue and teal palette.
colors <- list(
  hex_fill = "#2c3e50", # Dark slate blue
  hex_border = "#233140", # Even darker slate blue
  text_light = "#ecf0f1", # Off-white for text
  grid_fill = "#a2d5d9", # Soft, desaturated teal
  grid_border = "#76abb0", # Darker teal for grid lines
  outline = "#34495e" # Dark grey for Malta outline
)

# --- 2. Get and prepare the spatial data ---
malta_boundary <- gisco_get_countries(
  year = "2024",
  epsg = "3035",
  country = "Malta",
  resolution = "01"
)

malta_grid <- create_grid(
  grid_extent = malta_boundary,
  cellsize_m = 5000,
  clip_to_input = TRUE
)

# --- 3. Create the ggplot graphic for the subplot ---
logo_plot <- ggplot() +
  geom_sf(
    data = malta_grid,
    fill = colors$grid_fill,
    color = colors$grid_border,
    linewidth = 0.5
  ) +
  geom_sf(
    data = malta_boundary,
    fill = NA,
    color = colors$outline,
    linewidth = 0.8
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA)
  )

# --- 4. Create the hex sticker and add custom annotations ---

# First, create the sticker object *without* saving it yet.
# We leave the url/bottom text fields blank to add them manually.
s <- sticker(
  # --- Subplot ---
  subplot = logo_plot,
  s_x = 1.0,
  s_y = 0.95, # Nudge the plot up to make room for bottom text
  s_width = 0.95, # Keep it contained within the hex
  s_height = 0.85,

  # --- Package Name ---
  package = "gridmaker",
  p_family = "sans",
  p_size = 30,
  p_y = 1.55, # Position title at the top
  p_color = colors$text_light,

  # --- Hexagon Styling ---
  h_fill = colors$hex_fill,
  h_color = colors$hex_border,

  # --- Output (we save manually later) ---
  filename = "man/figures/logo.png",
  dpi = 600
)

# --- 5. Add custom text annotations to the sticker object ---
# This is the advanced step. We add layers to the ggplot object `s`.
# The coordinates (x, y) and angles are fine-tuned to fit the hex shape.
s_final <- s +
  annotate(
    "text",
    x = 0.5,
    y = 0.75,
    label = "GISCO",
    color = colors$text_light,
    size = 16,
    family = "sans",
    angle = 0
  ) +
  annotate(
    "text",
    x = 1.45,
    y = 1.15,
    label = "INSPIRE",
    color = colors$text_light,
    size = 16,
    family = "sans",
    angle = 0
  ) +
  annotate(
    "text",
    x = 1.0,
    y = 0.48,
    label = "CRS3035 RES 5000m N1425000 E4725000",
    color = colors$text_light,
    size = 10,
    family = "sans",
    angle = 0
  )


# print(s_final)

# Now, explicitly save the final, modified sticker object
# Using `save_sticker()` is better than `ggsave()` for this.
save_sticker(
  "man/figures/logo.png",
  s_final,
  dpi = 500
)
