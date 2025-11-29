# gridmaker 0.1.0

* Initial version with basic grid generation functionality.
* Optimized parallel chunking strategy to improve performance on high-core systems (>16 workers).
* Implemented adaptive worker scaling for parallel grid generation. The function now automatically limits the number of active workers for smaller grids (< 500k cells) to reduce parallelization overhead.
* Changed the default `tile_multiplier` to 1, as benchmarks showed this consistently provides the best performance.
