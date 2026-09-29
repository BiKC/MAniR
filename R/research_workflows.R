# Researcher-facing summaries. These functions do not depend on Shiny or Plotly.
# They use the original numerical values, never heatmap-normalized colors.

manir_pair_inspection <- function(first, second = NULL, isolate_1, isolate_2,
                                  metadata = NULL, field = NULL,
                                  comparable = FALSE) {
  ids <- colnames(first)
  if (length(isolate_1) != 1L || length(isolate_2) != 1L ||
      is.na(isolate_1) || is.na(isolate_2) ||
      !isolate_1 %in% ids || !isolate_2 %in% ids ||
      identical(isolate_1, isolate_2))
    stop("Choose two different isolates present in the first matrix.")
  if (!is.null(second) &&
      (!isolate_1 %in% colnames(second) || !isolate_2 %in% colnames(second)))
    stop("Both isolates must be present in the second matrix to compare them.")
  value1 <- unname(first[isolate_1, isolate_2])
  value2 <- if (is.null(second)) NA_real_ else
    unname(second[isolate_1, isolate_2])
  label1 <- label2 <- NA_character_
  if (!is.null(metadata) && !is.null(field) && length(field) == 1L &&
      !is.na(field) && nzchar(field) && field %in% names(metadata)) {
    label1 <- if (isolate_1 %in% rownames(metadata))
      as.character(metadata[isolate_1, field]) else NA_character_
    label2 <- if (isolate_2 %in% rownames(metadata))
      as.character(metadata[isolate_2, field]) else NA_character_
  }
  list(isolate_1 = isolate_1, isolate_2 = isolate_2,
       first = value1, second = value2,
       difference = if (isTRUE(comparable) && !is.null(second))
         value1 - value2 else NULL,
       metadata_field = if (is.null(field)) "" else field,
       group_1 = label1, group_2 = label2)
}

# A descriptive within/between-category summary, computed independently for
# each original matrix. Treat the pairs as non-independent observations.
manir_group_overview <- function(first, second = NULL, metadata, field,
                                 max_pairs = 100000L) {
  if (is.null(metadata) || !is.data.frame(metadata) ||
      length(field) != 1L || is.na(field) || !nzchar(field) ||
      !field %in% names(metadata))
    stop("Select a metadata column containing sample groups or batches.")
  one <- replicate_summary(first, metadata, field, max_pairs = max_pairs)
  names(one)[names(one) == "type"] <- "comparison"
  one <- data.frame(matrix = "Matrix 1", one, check.names = FALSE)
  if (!is.null(second)) {
    two <- replicate_summary(second, metadata, field, max_pairs = max_pairs)
    names(two)[names(two) == "type"] <- "comparison"
    one <- rbind(one, data.frame(matrix = "Matrix 2", two,
                                check.names = FALSE))
  }
  rownames(one) <- NULL
  one
}

manir_directional_association <- function(pairs, kind1 = "similarity",
                                           kind2 = "similarity") {
  d <- pairs$data
  if (nrow(d) < 3L)
    return(list(pearson = NA_real_, spearman = NA_real_, n = nrow(d)))
  x <- if (identical(kind1, "distance")) -d$first else d$first
  y <- if (identical(kind2, "distance")) -d$second else d$second
  valid <- is.finite(x) & is.finite(y)
  x <- x[valid]; y <- y[valid]
  list(pearson = if (length(x) >= 3L && stats::sd(x) > 0 &&
                     stats::sd(y) > 0) stats::cor(x, y) else NA_real_,
       spearman = if (length(unique(x)) > 1L && length(unique(y)) > 1L)
         suppressWarnings(stats::cor(x, y, method = "spearman"))
         else NA_real_, n = length(x))
}

manir_fmt <- function(value) {
  if (length(value) != 1L || !is.finite(value)) return("not estimable")
  format(signif(value, 4L), trim = TRUE)
}

# Markdown text export for an editable lab notebook. Only optional,
# already-computed results are included. Expensive clustering/permutation
# analyses are never started by the report download itself.
manir_research_report <- function(dataset, pairs = NULL, group_table = NULL,
                                   group_field = "", cluster = NULL,
                                   comparable = FALSE,
                                   question = "", notes = "",
                                   max_pairs = 100000L) {
  example <- identical(dataset$source, "synthetic example")
  one <- if (!is.null(dataset$label1)) dataset$label1 else "Matrix 1"
  two <- if (!is.null(dataset$label2)) dataset$label2 else "Matrix 2"
  lines <- c("# MAniR analysis notes", "",
    if (nzchar(trimws(question))) paste("**Research question:**",
                                      trimws(question)) else NULL,
    if (example) "**SYNTHETIC teaching example: not measured biological evidence.**" else NULL,
    "## Dataset", "",
    paste("- Source:", dataset$source),
    sprintf("- %s: %d isolates (%s)", one, nrow(dataset$first),
            dataset$kind1),
    if (!is.null(dataset$second))
      sprintf("- %s: %d isolates (%s)", two, nrow(dataset$second),
              dataset$kind2),
    sprintf("- Sample matching: %s; clustering linkage: %s",
            dataset$matching, dataset$linkage),
    if (!is.null(dataset$metadata))
      paste("- Metadata fields:", paste(names(dataset$metadata),
                                       collapse = ", ")),
    "")
  if (!is.null(pairs)) {
    stats <- manir_directional_association(pairs, dataset$kind1,
                                           dataset$kind2)
    lines <- c(lines, "## Pairwise relationship", "",
      sprintf("- %s of %s unordered pairs evaluated; %s had values in both matrices.",
              pairs$evaluated_pairs,
              format(pairs$total_pairs, scientific = FALSE),
              pairs$valid_pairs),
      sprintf("- Direction-adjusted Pearson r: %s", manir_fmt(stats$pearson)),
      sprintf("- Direction-adjusted Spearman rho: %s", manir_fmt(stats$spearman)),
      if (isTRUE(pairs$sampled))
        sprintf("- Reproducible sample: %s pairs (seed %s; requested limit %s).",
                pairs$evaluated_pairs, pairs$seed, max_pairs),
      if (isTRUE(comparable)) {
        raw <- matrix_agreement(pairs)
        c(sprintf("- Mean absolute difference (original units): %s",
                  manir_fmt(raw$mae)),
          sprintf("- Root mean squared difference (original units): %s",
                  manir_fmt(raw$rmse)))
      } else "- Raw differences omitted: measurement equivalence was not confirmed.",
      "",
      "These are descriptive measurements. Repeated isolate pairs are not independent observations.",
      "")
  }
  if (!is.null(cluster)) {
    lines <- c(lines, "## Cluster comparison", "",
      sprintf("- Adjusted Rand index: %s", manir_fmt(cluster$ari)),
      sprintf("- Adjusted Wallace, matrix 1 to 2: %s",
              manir_fmt(cluster$adjusted_wallace_1_to_2)),
      sprintf("- Adjusted Wallace, matrix 2 to 1: %s",
              manir_fmt(cluster$adjusted_wallace_2_to_1)),
      "Cluster labels are arbitrary; agreement is not a measure of typing accuracy.",
      "")
  }
  if (!is.null(group_table) && nrow(group_table)) {
    lines <- c(lines, "## Metadata groups", "",
      paste("Selected field:", group_field), "",
      "| Matrix | Pair category | Pairs | Median | Sampled |",
      "| --- | --- | ---: | ---: | --- |",
      vapply(seq_len(nrow(group_table)), function(i) {
        x <- group_table[i, ]
        sprintf("| %s | %s | %s | %s | %s |",
                x$matrix, x$comparison, x$pairs,
                manir_fmt(x$median), if (isTRUE(x$sampled)) "yes" else "no")
      }, character(1L)),
      "",
      "Group summaries are descriptive. Pairwise values reuse isolates, and batch effects may be confounded.",
      "")
  }
  lines <- c(lines, "## Researcher notes", "",
             if (nzchar(trimws(notes))) notes else
               "Record what the figures suggest, the limitations, and what you plan to check next.",
             "", "## Reproducibility", "",
             "- Export the original matrix CSV or RDS files alongside these notes.",
             "- Record the selected metadata field, pair limit and version of MAniR.",
             "- Figures may use independently normalized colors or raster previews; use original values for inference.")
  lines
}


# Values for a descriptive group-distribution plot. Never feed these into a
# standard two-independent-samples test: pairs within groups share isolates.
manir_group_pairs <- function(matrix, metadata, field,
                              max_pairs = 20000L) {
  if (is.null(metadata) || !is.data.frame(metadata) ||
      length(field) != 1L || is.na(field) || !nzchar(field) ||
      !field %in% names(metadata))
    stop("Choose a valid metadata group or batch field.")
  groups <- as.character(metadata[match(rownames(matrix),
                                        rownames(metadata)), field])
  ix <- upper_pair_indices(nrow(matrix), max_pairs = max_pairs)
  value <- matrix[cbind(ix$i, ix$j)]
  left <- groups[ix$i]
  right <- groups[ix$j]
  valid <- is.finite(value) & !is.na(left) & !is.na(right) &
    nzchar(left) & nzchar(right)
  list(data = data.frame(
    group = ifelse(left[valid] == right[valid], "Within", "Between"),
    value = value[valid]),
    sampled = ix$sampled,
    evaluated_pairs = length(value),
    total_pairs = ix$total)
}
