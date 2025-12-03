# Changelog

## gridmaker 0.1.0

- Initial version with basic grid generation functionality.
- **Performance optimizations based on multi-resolution benchmarks
  (50m-1000m cell sizes):**
  - Refined adaptive worker scaling: \<50k cells use max 4 workers,
    \<500k cells use max 8 workers, \<2M cells use max 16 workers. This
    minimizes parallelization overhead which can reduce performance by
    up to 50% on small grids.
  - Benchmarks show 8 workers provide optimal performance for most grid
    sizes (50m-500m cells).
  - Default `tile_multiplier` set to 1, as benchmarks show `tile_mult=2`
    consistently reduces performance (e.g., 50m cells: 1.97x speedup
    with mult=1 vs 1.47x with mult=2).
  - Added user-facing warnings when \>32 workers are configured
    (performance typically decreases due to overhead) or when
    `tile_multiplier > 1` is set.
