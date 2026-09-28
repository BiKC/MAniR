#!/usr/bin/env Rscript
# Benchmark selected calculation and display stages. Run with:
# Rscript benchmarks/benchmark.R
# Rscript benchmarks/benchmark.R --smoke
# Rscript benchmarks/benchmark.R --large
# Results are hardware-dependent; repeated runs and environment details
# are required before reporting performance claims.

source("R/matrix_io.R")
source("R/matrix_analysis.R")
source("R/matrix_plot.R")

args <- commandArgs(trailingOnly = TRUE)
smoke <- "--smoke" %in% args
large <- "--large" %in% args
sizes <- if (smoke) c(60L, 120L) else if (large)
  c(500L, 1000L, 2500L, 5000L, 10000L)
else c(100L, 300L, 600L, 1000L, 2500L)
repeats <- if (smoke) 1L else 3L
dir.create("benchmarks/results", recursive = TRUE, showWarnings = FALSE)
results <- list()
idx <- 1L

for (n in sizes) {
  cat("Generating synthetic matrix for", n, "isolates\n")
  set.seed(777)
  features <- matrix(stats::rnorm(n * 7L), nrow = n, ncol = 7L)
  m <- exp(-as.matrix(stats::dist(features)) / 3)
  ids <- sprintf("isolate_%06d", seq_len(n))
  dimnames(m) <- list(ids, ids)
  diag(m) <- 1
  rm(features)
  gc()
  for (repeat_index in seq_len(repeats)) {
    run_case <- function(stage, expr) {
      gc(reset = TRUE)
      start <- proc.time()[["elapsed"]]
      result <- force(expr)
      elapsed <- proc.time()[["elapsed"]] - start
      # R's reported high-water vector heap is approximate, not process RSS.
      approx_heap_mb <- unname(gc()[2L, "max used"]) * 8 / 1024^2
      results[[idx]] <<- data.frame(
        n = n, repeat_index = repeat_index, stage = stage,
        seconds = elapsed, approx_vector_heap_mb = approx_heap_mb,
        renderer = "MAniR v3", stringsAsFactors = FALSE)
      idx <<- idx + 1L
      invisible(result)
    }
    run_case("validate", validate_matrix(m, kind = "similarity"))
    if (n <= 2500L) {
      run_case("cluster", matrix_order(m, kind = "similarity"))
    } else {
      run_case("skip_cluster_large", matrix_order(m, kind = "similarity"))
    }
    run_case("pair_sampling_100k", paired_values(m, m,
              max_pairs = 100000L))
    run_case("raster_512", {
      file <- tempfile(fileext = ".png")
      ma_save_png(m, file, max_side = 512L)
      unlink(file)
    })
  }
  rm(m)
  gc()
}
result <- do.call(rbind, results)
print(result, row.names = FALSE)
out <- file.path("benchmarks/results",
                 paste0("MAniR-", format(Sys.time(), "%Y%m%d-%H%M%S"),
                        if (smoke) "-smoke" else "", ".csv"))
utils::write.csv(result, out, row.names = FALSE)
cat("Results written to:", out, "\n")
cat("R version:", R.version.string, "\n")
print(utils::sessionInfo())
