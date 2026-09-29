#!/usr/bin/env Rscript
# Compare the historical corrplot + heatmaply pipeline with the upgraded
# cluster-once + adaptive-plot pipeline. This replays representative calls,
# rather than executing the original interactive app in a browser.
# Prerequisites: install.packages(c("corrplot", "heatmaply", "plotly"))
# Usage: Rscript benchmarks/compare_original.R
#        Rscript benchmarks/compare_original.R --smoke

source("R/matrix_io.R")
source("R/matrix_analysis.R")
source("R/matrix_plot.R")
for (pkg in c("corrplot", "heatmaply", "plotly")) {
  if (!requireNamespace(pkg, quietly = TRUE))
    stop("Install ", pkg, " to compare with the historical rendering pipeline.")
}
smoke <- "--smoke" %in% commandArgs(trailingOnly = TRUE)
sizes <- if (smoke) c(50L, 100L) else c(100L, 300L, 600L)
repeats <- if (smoke) 1L else 3L
results <- list()
index <- 1L
dir.create("benchmarks/results", recursive = TRUE, showWarnings = FALSE)

record <- function(n, repeat_index, pipeline, expression) {
  gc(reset = TRUE)
  start <- proc.time()[["elapsed"]]
  invisible(force(expression))
  elapsed <- proc.time()[["elapsed"]] - start
  results[[index]] <<- data.frame(
    n = n, repeat_index = repeat_index, pipeline = pipeline,
    seconds = elapsed,
    approx_vector_heap_mb = unname(gc()[2L, "max used"]) * 8 / 1024^2)
  index <<- index + 1L
}

for (n in sizes) {
  set.seed(2026)
  features <- matrix(stats::rnorm(n * 5L), ncol = 5L)
  m <- exp(-as.matrix(stats::dist(features)) / 3)
  ids <- sprintf("isolate_%05d", seq_len(n))
  dimnames(m) <- list(ids, ids)
  diag(m) <- 1
  rm(features)
  for (repeat_index in seq_len(repeats)) {
    record(n, repeat_index, "original_corrplot_then_heatmaply", {
      # Original MAniR built a corrplot to obtain cluster order and also
      # constructed a heatmaply figure. Legacy plots are run on an offscreen
      # graphics device so the script works headlessly.
      pdf_file <- tempfile(fileext = ".pdf")
      grDevices::pdf(pdf_file)
      tryCatch({
        corrplot::corrplot.mixed(m, lower = "color", upper = "number",
                                 is.corr = FALSE, order = "hclust",
                                 diag = "n")
      }, finally = {
        grDevices::dev.off()
        unlink(pdf_file)
      })
      fig <- heatmaply::heatmaply(m, show_dendrogram = c(FALSE, FALSE),
                                  cellnote = round(m, 2), hide_colorbar = TRUE)
      plotly::plotly_build(fig)
    })
    record(n, repeat_index, "upgraded_cluster_then_adaptive_render", {
      ordering <- matrix_order(m, kind = "similarity")
      ordered <- m[ordering$ids, ordering$ids, drop = FALSE]
      if (n <= 300L) {
        plotly::plotly_build(ma_interactive(ordered, name = "Matrix"))
      } else {
        png_file <- tempfile(fileext = ".png")
        ma_save_png(ordered, png_file, max_side = 512L)
        unlink(png_file)
      }
    })
  }
  rm(m)
  gc()
}
result <- do.call(rbind, results)
print(result, row.names = FALSE)
output <- file.path("benchmarks/results",
  paste0("comparison-", format(Sys.time(), "%Y%m%d-%H%M%S"), ".csv"))
utils::write.csv(result, output, row.names = FALSE)
cat("Benchmark output:", output, "\n")
cat("The renderers have different capabilities above 300 isolates; compare\n",
    "end-user latency, not pixel-for-pixel rendering speed, in that range.\n")
print(utils::sessionInfo())
