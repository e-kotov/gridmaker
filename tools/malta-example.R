# Load all the required libraries
library(sf)
library(giscoR)
library(gridmaker)
library(ggplot2)
library(patchwork)

# 1. --- Get the Spatial Data ---
boundary <- gisco_get_countries(
  year = "2024",
  epsg = "3035",
  country = "Malta",
  resolution = "01"
)

# 2. --- Create the Grid ---
grid <- inspire_grid(
  grid_extent = boundary,
  cellsize_m = 1000,
  clip_to_input = TRUE
)

# 3. --- Create the Left Plot (with Titles and Caption) ---
p1 <- ggplot() +
  geom_sf(data = boundary, fill = "gray90", color = "gray40") +
  labs(
    title = "sf boundary object",
    subtitle = "or bounding box",
    caption = "Data from {giscoR} R package"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      size = 18,
      face = "bold",
      margin = margin(b = 5)
    ),
    plot.subtitle = element_text(hjust = 0.5, size = 14),
    plot.caption = element_text(
      hjust = 0.5,
      size = 11,
      face = "italic",
      color = "gray20"
    )
  )

# 4. --- Create the Right Plot (with Titles and Caption) ---
p2 <- ggplot() +
  geom_sf(data = grid, fill = "#ffcccb", color = "#e06666", linewidth = 0.25) +
  geom_sf(data = boundary, fill = NA, color = "gray40", linewidth = 0.5) +
  labs(
    title = "GISCO/INSPIRE standardized grid",
    subtitle = "with IDs in form of CRS 3035 RES 1000m N3016000 E4032000",
    caption = "Grid from {gridmaker} R package"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      size = 18,
      face = "bold",
      margin = margin(b = 5)
    ),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    plot.caption = element_text(
      hjust = 0.5,
      size = 11,
      face = "italic",
      color = "gray20"
    )
  )

# 5. --- Create the Center Plot (The Arrow) ---
p_arrow <- ggplot() +
  annotate("text", x = 0, y = 0, label = "→", size = 30) +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(plot.margin = margin(0, 0, 0, 0))

# 6. --- Combine All Plots and Add Overall Title ---

# THIS IS THE KEY CHANGE: Reduce the middle value in `widths`
# from 0.25 to a smaller number like 0.15 to shrink the center panel.
final_plot <- (p1 + p_arrow + p2 + plot_layout(widths = c(1, 0.15, 1))) +
  plot_annotation(
    title = "gridmaker: Create Standardized Spatial Grids",
    subtitle = "Transforming an sf object into a GISCO/INSPIRE-compliant grid\n",
    theme = theme(
      plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(
        size = 16,
        hjust = 0.5,
        margin = margin(b = 10)
      )
    )
  )

# Display the final plot
print(final_plot)

# Optional: Save with a reduced width to match the new compact layout
ggsave(
  "man/figures/malta.png",
  plot = final_plot,
  width = 12,
  height = 7,
  dpi = 160,
  bg = "white"
)
