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

ma_scale <- function(x, log_scale = FALSE, center_zero = FALSE) {
  original <- x
  valid <- is.finite(x)
  if (!any(valid)) stop("There are no finite values to display.")
  if (log_scale) {
    if (any(x[valid] < 0)) stop("Log scaling requires nonnegative values.")
    x[valid] <- log1p(x[valid])
  }
  if (center_zero) {
    if (log_scale) stop("Zero-centered scaling cannot use logarithms.")
    radius <- max(abs(x[valid]))
    x[valid] <- if (radius == 0) 0.5 else 0.5 + 0.5 * x[valid] / radius
  } else {
    rng <- range(x[valid])
    if (diff(rng) == 0) {
      x[valid] <- 0.5
    } else {
      x[valid] <- (x[valid] - rng[1L]) / diff(rng)
    }
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
                           log_scale = FALSE, center_zero = FALSE,
                           metadata = NULL, group_column = NULL) {
  if (!requireNamespace("plotly", quietly = TRUE))
    stop("Install the plotly package for interactive plots.")
  n <- if (combined) nrow(m$first) else nrow(m)
  if (n > 300L) stop("Use the raster renderer for matrices above 300 isolates.")
  if (combined) {
    norm <- ma_combined(m$first, m$second, log_scale)
    z <- norm$values
    original <- norm$original
  } else {
    original <- m
    z <- ma_scale(m, log_scale, center_zero)
  }
  ids <- rownames(original)
  ids_safe <- htmltools::htmlEscape(ids)
  hover <- matrix("", nrow = n, ncol = n)
  for (j in seq_len(n)) {
    dataset <- if (combined) ifelse(seq_len(n) < j, first_name,
                                   ifelse(seq_len(n) > j, second_name, "Diagonal")) else name
    hover[, j] <- paste0("Row: ", ids_safe, "<br>Column: ", ids_safe[j],
                          "<br>", dataset, ": ", format(original[, j],
                          digits = 8L, trim = TRUE))
  }
  shapes <- NULL
  if (!is.null(metadata) && !is.null(group_column) &&
      isTRUE(nzchar(group_column)) && group_column %in% names(metadata)) {
    groups <- as.character(metadata[match(ids, rownames(metadata)), group_column])
    group_text <- htmltools::htmlEscape(ifelse(is.na(groups), "Missing", groups))
    for (j in seq_len(n))
      hover[, j] <- paste0(hover[, j], "<br>",
                            htmltools::htmlEscape(group_column), ": ",
                            group_text)
    classes <- unique(groups[!is.na(groups)])
    if (length(classes) <= 30L && length(classes) > 0L) {
      category_colors <- grDevices::hcl.colors(length(classes), "Set 2")
      selected <- category_colors[match(groups, classes)]
      selected[is.na(selected)] <- "#E5E5E5"
      shapes <- lapply(seq_len(n), function(i) {
        list(type = "rect", xref = "paper", yref = "y",
             x0 = -0.035, x1 = -0.012,
             y0 = n - i - 0.45, y1 = n - i + 0.45,
             fillcolor = selected[i], line = list(width = 0))
      })
    }
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
        text = note[i, ], xref = "x", yref = "y", showarrow = FALSE,
        font = list(size = if (n > 35L) 7L else 10L))
    }
  }
  plotly::layout(p, title = name,
                 xaxis = list(title = "", side = "bottom", tickangle = -65,
                              automargin = TRUE),
                 yaxis = list(title = "", automargin = TRUE),
                 shapes = shapes,
                 margin = list(l = 130, b = 130, r = 30, t = 55))
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
                      normalized = FALSE, center_zero = FALSE) {
  preview <- ma_preview(m, max_side)
  x <- preview$matrix
  z <- if (normalized) x else ma_scale(x, log_scale, center_zero)
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
  plot(NA, xlim = c(0, n + 0.5), ylim = c(0.5, n + 0.5),
       xaxs = "i", yaxs = "i", asp = 1, axes = FALSE,
       xlab = "", ylab = "", main = title)
  graphics::rasterImage(pix, 0.5, 0.5, n + 0.5, n + 0.5,
                        interpolate = FALSE)
  axis(1, at = show_at, labels = colnames(x)[show_at],
       las = 2, cex.axis = 0.55)
  axis(2, at = n + 1L - show_at, labels = rownames(x)[show_at],
       las = 2, cex.axis = 0.55)
  box()
  if (!is.null(metadata) && !is.null(group_column) && nzchar(group_column) &&
      group_column %in% colnames(metadata)) {
    groups <- as.character(metadata[match(rownames(x), rownames(metadata)), group_column])
    valid <- !is.na(groups)
    levels <- unique(groups[valid])
    annotation_colors <- grDevices::hcl.colors(max(1L, length(levels)), "Set 2")
    fill <- annotation_colors[match(groups, levels)]
    if (any(valid)) {
      # The left-hand color strip represents the selected metadata field.
      graphics::rect(0.06, n - which(valid) + 0.5, 0.40,
                     n - which(valid) + 1.5, col = fill[valid], border = NA)
      if (length(levels) <= 12L) {
        graphics::legend("topright", legend = levels, fill = annotation_colors,
                         cex = 0.55, bty = "n")
      }
    }
  }
  if (preview$sampled)
    mtext(sprintf("Representative view: %d of %d isolates. Inspect original values in the table.",
                  n, preview$full_n), side = 3, line = 0, cex = 0.75)
  invisible(preview)
}

ma_save_png <- function(m, file, palette = "RdBu", title = "",
                        max_side = 1200L, normalized = FALSE,
                        log_scale = FALSE) {
  grDevices::png(file, width = 1700L, height = 1600L, res = 160L)
  on.exit(grDevices::dev.off(), add = TRUE)
  graphics::par(mar = c(9, 9, 5, 2))
  ma_raster(m, palette = palette, title = title, max_side = max_side,
            normalized = normalized, log_scale = log_scale)
  invisible(file)
}


# Vector cells are useful for journal figures at modest dimensions. Rendering
# millions of individual rectangles into PDF/SVG would be excessive, so larger
# figures embed the same documented representative raster overview instead.
ma_publication_plot <- function(m, palette = "RdBu", title = "",
                                normalized = FALSE, vector_limit = 150L,
                                log_scale = FALSE) {
  n <- nrow(m)
  if (n > vector_limit) {
    ma_raster(m, palette = palette, title = title, max_side = 1200L,
              normalized = normalized, log_scale = log_scale)
    return(invisible(FALSE))
  }
  z <- if (normalized) m else ma_scale(m, log_scale = log_scale)
  cols <- ma_colors(palette)
  scaled <- as.integer(1 + round(z * (length(cols) - 1L)))
  fill <- matrix("#E5E5E5", nrow(z), ncol(z))
  good <- is.finite(z)
  fill[good] <- cols[scaled[good]]
  i <- rep.int(seq_len(n), times = n)
  j <- rep(seq_len(n), each = n)
  graphics::plot(NA_real_, xlim = c(0.5, n + 0.5),
                 ylim = c(0.5, n + 0.5), xaxs = "i", yaxs = "i",
                 asp = 1, axes = FALSE, xlab = "", ylab = "", main = title)
  # One vector rectangle per cell, with original numeric values retained
  # separately in CSV/RDS rather than embedded into rendered colors.
  graphics::rect(j - 0.5, n - i + 0.5, j + 0.5, n - i + 1.5,
                 col = fill[cbind(i, j)], border = NA)
  at <- unique(as.integer(round(seq.int(1L, n, length.out = min(20L, n)))))
  graphics::axis(1, at = at, labels = colnames(m)[at], las = 2, cex.axis = 0.6)
  graphics::axis(2, at = n + 1L - at, labels = rownames(m)[at],
                 las = 2, cex.axis = 0.6)
  graphics::box()
  invisible(TRUE)
}

ma_save_pdf <- function(m, file, palette = "RdBu", title = "",
                        normalized = FALSE, log_scale = FALSE) {
  grDevices::pdf(file, width = 11, height = 11, useDingbats = FALSE)
  on.exit(grDevices::dev.off(), add = TRUE)
  graphics::par(mar = c(10, 10, 5, 2))
  ma_publication_plot(m, palette, title, normalized, log_scale = log_scale)
  invisible(file)
}

ma_save_svg <- function(m, file, palette = "RdBu", title = "",
                        normalized = FALSE, log_scale = FALSE) {
  grDevices::svg(file, width = 11, height = 11)
  on.exit(grDevices::dev.off(), add = TRUE)
  graphics::par(mar = c(10, 10, 5, 2))
  ma_publication_plot(m, palette, title, normalized, log_scale = log_scale)
  invisible(file)
}
