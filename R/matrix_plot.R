# Rendering for standard and large matrices. No matrix analysis is performed here.

ma_colors <- function(palette = "RdBu", n = 256L, reverse = FALSE) {
  palettes <- c("RdBu", "BrBG", "PiYG", "PRGn", "PuOr", "RdYlBu",
                "YlOrRd", "Viridis", "Blues", "Greens", "Greys")
  if (!palette %in% palettes) palette <- "RdBu"
  if (palette == "Viridis") return(grDevices::hcl.colors(n, "Viridis"))
  if (requireNamespace("RColorBrewer", quietly = TRUE)) {
    max_colors <- min(11L, RColorBrewer::brewer.pal.info[palette, "maxcolors"])
    base <- RColorBrewer::brewer.pal(max_colors, palette)
  } else {
    base <- grDevices::hcl.colors(11L, "Blue-Red 3")
  }
  if (reverse) base <- rev(base)
  grDevices::colorRampPalette(base)(n)
}

ma_scale <- function(x, log_scale = FALSE) {
  original <- x
  valid <- is.finite(x)
  if (!any(valid)) stop("There are no finite values to display.")
  if (log_scale) {
    if (any(x[valid] <= 0)) stop("Log scaling requires strictly positive values.")
    x[valid] <- log(x[valid])
  }
  rng <- range(x[valid])
  if (diff(rng) == 0) {
    x[valid] <- 0.5
  } else {
    x[valid] <- (x[valid] - rng[1L]) / diff(rng)
  }
  x[!valid] <- NA_real_
  attr(x, "original_range") <- range(original[valid])
  x
}

ma_combined <- function(first, second, log_scale = FALSE) {
  if (!identical(dimnames(first), dimnames(second)))
    stop("Both matrices must have the same sample ordering.")
  a <- ma_scale(first, log_scale)
  b <- ma_scale(second, log_scale)
  combined <- a
  combined[lower.tri(combined)] <- b[lower.tri(b)]
  diag(combined) <- 0.5
  actual <- first
  actual[lower.tri(actual)] <- second[lower.tri(second)]
  list(values = combined, original = actual,
       first_range = attr(a, "original_range"),
       second_range = attr(b, "original_range"))
}

# Plotly is intentionally limited to manageable interactive views; the
# large-matrix renderer does not transmit hundreds of thousands of cell labels.
ma_interactive <- function(m, name = "Matrix", palette = "RdBu",
                           show_numbers = FALSE, combined = FALSE,
                           first_name = "Matrix 1", second_name = "Matrix 2",
                           log_scale = FALSE) {
  if (!requireNamespace("plotly", quietly = TRUE))
    stop("Install the plotly package for interactive plots.")
  n <- nrow(m)
  if (n > 300L) stop("Use the raster renderer for matrices above 300 isolates.")
  if (combined) {
    norm <- ma_combined(m$first, m$second, log_scale)
    z <- norm$values
    original <- norm$original
  } else {
    original <- m
    z <- ma_scale(m, log_scale)
  }
  ids <- rownames(original)
  hover <- matrix("", nrow = n, ncol = n)
  for (j in seq_len(n)) {
    dataset <- if (combined) ifelse(seq_len(n) < j, first_name,
                                   ifelse(seq_len(n) > j, second_name, "Diagonal")) else name
    hover[, j] <- paste0("Row: ", ids, "<br>Column: ", ids[j],
                          "<br>", dataset, ": ", format(original[, j],
                          digits = 8L, trim = TRUE))
  }
  note <- NULL
  if (show_numbers && n <= 70L) {
    note <- matrix(format(round(original, 2L), trim = TRUE),
                   nrow = n, ncol = n)
  }
  p <- plotly::plot_ly(
    x = ids, y = rev(ids), z = z[n:1L, , drop = FALSE],
    text = hover[n:1L, , drop = FALSE],
    type = "heatmap", colors = ma_colors(palette),
    zmin = 0, zmax = 1, hoverinfo = "text",
    showscale = TRUE
  )
  if (!is.null(note)) {
    # Text labels only for small matrices; large labels overload the browser.
    for (i in seq_len(n)) {
      p <- plotly::add_annotations(p, x = ids, y = ids[i],
        text = note[i, ], showarrow = FALSE,
        font = list(size = if (n > 35L) 7L else 10L))
    }
  }
  plotly::layout(p, title = name,
                 xaxis = list(title = "", side = "bottom", tickangle = -65,
                              automargin = TRUE),
                 yaxis = list(title = "", automargin = TRUE),
                 margin = list(l = 110, b = 130, r = 30, t = 55))
}

# Representative-pixel preview: never use the reduced view for numerical
# inference. The original matrix remains available for exact cell inspection.
ma_preview <- function(m, max_side = 512L) {
  n <- nrow(m)
  ix <- unique(as.integer(round(seq.int(1L, n, length.out = min(n, max_side)))))
  list(matrix = m[ix, ix, drop = FALSE], indices = ix, full_n = n,
       sampled = length(ix) < n)
}

ma_raster <- function(m, palette = "RdBu", title = "",
                      max_side = 512L, log_scale = FALSE,
                      metadata = NULL, group_column = NULL,
                      normalized = FALSE) {
  preview <- ma_preview(m, max_side)
  x <- preview$matrix
  z <- if (normalized) x else ma_scale(x, log_scale)
  cols <- ma_colors(palette)
  values <- as.integer(1 + round(z * (length(cols) - 1L)))
  color_matrix <- matrix(NA_character_, nrow(z), ncol(z))
  finite <- is.finite(z)
  color_matrix[finite] <- cols[values[finite]]
  color_matrix[!finite] <- "#E5E5E5"
  # Rows of a raster are drawn top to bottom.
  pix <- grDevices::as.raster(color_matrix)
  n <- nrow(x)
  show_at <- unique(as.integer(round(seq(1, n, length.out = min(n, 16L)))))
  plot(NA, xlim = c(0.5, n + 0.5), ylim = c(0.5, n + 0.5),
       xaxs = "i", yaxs = "i", asp = 1, axes = FALSE,
       xlab = "", ylab = "", main = title)
  graphics::rasterImage(pix, 0.5, 0.5, n + 0.5, n + 0.5,
                        interpolate = FALSE)
  axis(1, at = show_at, labels = colnames(x)[show_at],
       las = 2, cex.axis = 0.55)
  axis(2, at = n + 1L - show_at, labels = rownames(x)[show_at],
       las = 2, cex.axis = 0.55)
  box()
  if (preview$sampled)
    mtext(sprintf("Representative view: %d of %d isolates. Inspect original values in the table.",
                  n, preview$full_n), side = 3, line = 0, cex = 0.75)
  invisible(preview)
}

ma_save_png <- function(m, file, palette = "RdBu", title = "",
                        max_side = 1200L, normalized = FALSE) {
  grDevices::png(file, width = 1700L, height = 1600L, res = 160L)
  on.exit(grDevices::dev.off(), add = TRUE)
  op <- graphics::par(mar = c(9, 9, 5, 2))
  on.exit(graphics::par(op), add = TRUE)
  ma_raster(m, palette = palette, title = title, max_side = max_side,
            normalized = normalized)
  invisible(file)
}
