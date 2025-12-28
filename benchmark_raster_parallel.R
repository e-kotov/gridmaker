#!/usr/bin/env Rscript
# Benchmark: Raster Grid Generation Strategies
# Compare performance of different approaches to raster generation

library(gridmaker)
library(terra)
library(sf)
library(bench)

# Set a fixed seed for reproducibility
set.seed(42)

# Create test extent (Belgium)
belgium_bbox <- st_bbox(
  c(xmin = 2500000, ymin = 6500000, xmax = 3000000, ymax = 7000000),
  crs = st_crs(3035)
)
belgium_extent <- st_as_sfc(belgium_bbox)

# Test parameters
resolutions <- c(1000, 500) # Cell sizes in meters
core_counts <- c(1, 2, 4) # Number of cores to test

# Create results storage
results <- list()

cat("==============================================\n")
cat("Raster Grid Generation Benchmark\n")
cat("==============================================\n\n")
cat("Test extent: Belgium (500km x 500km)\n")
cat("Resolutions: ", paste(resolutions, "m", collapse = ", "), "\n")
cat("Core counts: ", paste(core_counts, collapse = ", "), "\n")
cat("Replications: 3 per configuration\n\n")

# Helper function to estimate cells
estimate_cells <- function(extent, cellsize_m) {
  bbox <- st_bbox(extent)
  width <- bbox["xmax"] - bbox["xmin"]
  height <- bbox["ymax"] - bbox["ymin"]
  return((width / cellsize_m) * (height / cellsize_m))
}

# Cleanup function
cleanup_files <- function() {
  files <- list.files(
    tempdir(),
    pattern = "^bench_raster_.*\\.(tif|nc)$",
    full.names = TRUE
  )
  unlink(files)
}

# Benchmark scenarios
for (res in resolutions) {
  n_cells <- estimate_cells(belgium_extent, res)

  cat("\n")
  cat("----------------------------------------------\n")
  cat(
    "RESOLUTION:",
    res,
    "m (~",
    format(n_cells, big.mark = ",", scientific = FALSE),
    "cells)\n"
  )
  cat("----------------------------------------------\n\n")

  # Scenario 1: Simple in-memory generation
  cat("1. Simple in-memory generation...\n")
  bm1 <- bench::mark(
    in_memory_simple = {
      grid <- inspire_grid(
        x = belgium_extent,
        cellsize_m = res,
        output_type = "spatraster",
        quiet = TRUE
      )
      rm(grid)
      gc(verbose = FALSE)
    },
    iterations = 1,
    check = FALSE,
    memory = TRUE
  )
  results[[paste0("in_memory_simple_", res, "m")]] <- bm1
  cat("   Median time:", format(bm1$median), "\n")
  cat("   Peak memory:", format(bm1$mem_alloc), "\n\n")

  # Scenario 2-3: Chunked in-memory with varying cores
  for (cores in core_counts) {
    cat(
      cores,
      ". Chunked in-memory (",
      cores,
      " core",
      ifelse(cores > 1, "s", ""),
      ")...\n",
      sep = ""
    )

    # Note: For in-memory generation, cores affect terra's internal operations
    # We'll use options to set cores for this test
    bm_chunked <- bench::mark(
      chunked_in_memory = {
        old_opt <- getOption("gridmaker.terra_cores")
        options(gridmaker.terra_cores = cores)

        grid <- inspire_grid(
          x = belgium_extent,
          cellsize_m = res,
          output_type = "spatraster",
          quiet = TRUE
        )

        options(gridmaker.terra_cores = old_opt)
        rm(grid)
        gc(verbose = FALSE)
      },
      iterations = 1,
      check = FALSE,
      memory = TRUE
    )
    results[[paste0("chunked_memory_", res, "m_", cores, "c")]] <- bm_chunked
    cat("   Median time:", format(bm_chunked$median), "\n")
    cat("   Peak memory:", format(bm_chunked$mem_alloc), "\n\n")
  }

  # Scenario 4: Direct non-chunked to file
  # Note: For small enough datasets, terra might not chunk
  cat("4. Direct to file (auto-chunking)...\n")
  bm4 <- bench::mark(
    direct_to_file = {
      outfile <- tempfile(pattern = "bench_raster_direct_", fileext = ".tif")
      inspire_grid(
        x = belgium_extent,
        cellsize_m = res,
        output_type = "spatraster",
        dsn = outfile,
        quiet = TRUE
      )
      unlink(outfile)
    },
    iterations = 1,
    check = FALSE,
    memory = TRUE
  )
  results[[paste0("direct_file_", res, "m")]] <- bm4
  cat("   Median time:", format(bm4$median), "\n")
  cat("   Peak memory:", format(bm4$mem_alloc), "\n\n")

  # Scenario 5-6: Chunked to file with varying cores
  for (cores in core_counts) {
    cat(
      cores + 4,
      ". Chunked to file (",
      cores,
      " core",
      ifelse(cores > 1, "s", ""),
      ")...\n",
      sep = ""
    )

    bm_file <- bench::mark(
      chunked_to_file = {
        outfile <- tempfile(pattern = "bench_raster_chunked_", fileext = ".tif")
        old_opt <- getOption("gridmaker.terra_cores")
        options(gridmaker.terra_cores = cores)

        inspire_grid(
          x = belgium_extent,
          cellsize_m = res,
          output_type = "spatraster",
          dsn = outfile,
          quiet = TRUE
        )

        options(gridmaker.terra_cores = old_opt)
        unlink(outfile)
      },
      iterations = 1,
      check = FALSE,
      memory = TRUE
    )
    results[[paste0("chunked_file_", res, "m_", cores, "c")]] <- bm_file
    cat("   Median time:", format(bm_file$median), "\n")
    cat("   Peak memory:", format(bm_file$mem_alloc), "\n\n")
  }

  cleanup_files()
}

cat("\n==============================================\n")
cat("Benchmark Complete\n")
cat("==============================================\n\n")

# Combine all results into a summary table
summary_df <- do.call(
  rbind,
  lapply(names(results), function(name) {
    bm <- results[[name]]

    # Parse name to extract metadata
    parts <- strsplit(name, "_")[[1]]
    scenario <- paste(parts[1:(length(parts) - 1)], collapse = "_")
    res <- gsub("m", "", parts[length(parts) - 1])
    cores_part <- parts[length(parts)]
    cores <- if (grepl("c$", cores_part)) {
      as.integer(gsub("c", "", cores_part))
    } else {
      1
    }

    data.frame(
      scenario = scenario,
      resolution_m = as.integer(res),
      cores = cores,
      median_time_s = as.numeric(bm$median),
      mean_time_s = as.numeric(bm$mean),
      mem_alloc_mb = as.numeric(bm$mem_alloc) / 1024^2,
      stringsAsFactors = FALSE
    )
  })
)

# Sort by resolution and scenario
summary_df <- summary_df[
  order(summary_df$resolution_m, summary_df$scenario, summary_df$cores),
]

# Print summary
cat("\nSUMMARY TABLE:\n")
cat("==============\n\n")
print(summary_df, row.names = FALSE, digits = 3)

# Save results
output_file <- "benchmark_raster_results.rds"
saveRDS(list(results = results, summary = summary_df), output_file)
cat("\n\nResults saved to:", output_file, "\n")

# Create a simple visualization if ggplot2 is available
if (requireNamespace("ggplot2", quietly = TRUE)) {
  library(ggplot2)

  # Add a human-readable scenario label
  summary_df$scenario_label <- paste0(
    gsub("_", " ", tools::toTitleCase(summary_df$scenario)),
    " (",
    summary_df$cores,
    " core",
    ifelse(summary_df$cores > 1, "s", ""),
    ")"
  )

  p <- ggplot(
    summary_df,
    aes(x = factor(resolution_m), y = median_time_s, fill = scenario_label)
  ) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(
      title = "Raster Generation Performance Comparison",
      subtitle = "Belgium extent (~500km × 500km)",
      x = "Cell Size (meters)",
      y = "Median Time (seconds)",
      fill = "Strategy"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom", legend.text = element_text(size = 8)) +
    guides(fill = guide_legend(ncol = 2))

  ggsave("benchmark_raster_plot.png", p, width = 12, height = 8, dpi = 150)
  cat("Plot saved to: benchmark_raster_plot.png\n")
}

cat("\n✓ Benchmark complete!\n")
