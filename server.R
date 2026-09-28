# Session-local state: uploaded matrices are never placed in global caches.
server <- function(input, output, session) {
  is_excel <- function(file) {
    !is.null(file) && tolower(tools::file_ext(file$name)) %in% c("xlsx", "xlsm")
  }
  output$first_sheet_ui <- shiny::renderUI({
    if (!is_excel(input$first_file)) return(NULL)
    sheets <- tryCatch(matrix_sheet_names(input$first_file$datapath, input$first_file$name),
                       error = function(e) character())
    shiny::selectInput("first_sheet", "First matrix worksheet",
                       choices = sheets, selected = sheets[1L])
  })
  output$second_sheet_ui <- shiny::renderUI({
    f <- if (!is.null(input$second_file)) input$second_file else input$first_file
    if (!is_excel(f)) return(NULL)
    sheets <- tryCatch(matrix_sheet_names(f$datapath, f$name),
                       error = function(e) character())
    suggested <- if (is.null(input$second_file) && length(sheets) > 1L)
      sheets[2L] else "None"
    shiny::selectInput("second_sheet", "Second matrix worksheet",
                       choices = c("None", sheets), selected = suggested)
  })
  output$metadata_sheet_ui <- shiny::renderUI({
    f <- if (!is.null(input$metadata_file)) input$metadata_file else input$first_file
    if (!is_excel(f)) return(NULL)
    sheets <- tryCatch(matrix_sheet_names(f$datapath, f$name),
                       error = function(e) character())
    suggested <- if (is.null(input$metadata_file) &&
                     is.null(input$second_file) && length(sheets) > 2L)
      sheets[3L] else "None"
    shiny::selectInput("metadata_sheet", "Metadata worksheet",
                       choices = c("None", sheets), selected = suggested)
  })

  analyze_input <- function(example = FALSE) {
    if (!example) shiny::req(input$first_file)
    shiny::withProgress(message = "Validating matrices and computing sample order",
                        value = 0, {
      tryCatch({
        if (example) {
          demo <- manir_load_example()
          a <- demo$first
          b <- demo$second
          meta <- demo$metadata
          kind1 <- "similarity"
          kind2 <- "similarity"
          match_mode <- "strict"
          cluster_requested <- TRUE
          linkage <- "complete"
          shiny::incProgress(0.40)
        } else {
          kind1 <- input$kind1
          kind2 <- input$kind2
          match_mode <- input$match_mode
          cluster_requested <- isTRUE(input$cluster)
          linkage <- input$linkage
          f <- input$first_file
        wb1 <- if (is_excel(f)) openxlsx::loadWorkbook(f$datapath) else NULL
        a <- read_matrix_input(f$datapath,
           sheet = if (is_excel(f)) input$first_sheet else NULL,
           workbook = wb1, format = f$name, kind = input$kind1)
        shiny::incProgress(0.20)
        f2 <- if (!is.null(input$second_file)) input$second_file else f
        has_second <- !is.null(input$second_file) ||
          (is_excel(f) && !is.null(input$second_sheet) &&
           input$second_sheet != "None")
        b <- NULL
        if (has_second) {
          if (is_excel(f2) && (is.null(input$second_sheet) ||
                               input$second_sheet == "None"))
            stop("Select a second matrix worksheet.")
          wb2 <- if (!is.null(input$second_file) && is_excel(f2))
            openxlsx::loadWorkbook(f2$datapath) else wb1
          b <- read_matrix_input(f2$datapath,
              sheet = if (is_excel(f2)) input$second_sheet else NULL,
              workbook = wb2, format = f2$name, kind = input$kind2)
          # Match once for early actionable errors; avoid retaining copies.
          shared_ids <- intersect(colnames(a), colnames(b))
          if (input$match_mode == "strict" &&
              !setequal(colnames(a), colnames(b)))
            stop(sprintf("Matrices have %d and %d isolates; %d are shared. Select 'Shared isolates only' or fix the IDs.",
                         nrow(a), nrow(b), length(shared_ids)))
          if (length(shared_ids) < 2L) stop("At least two shared isolates are required.")
        }
        shiny::incProgress(0.20)
        meta <- NULL
        fmeta <- if (!is.null(input$metadata_file))
          input$metadata_file else f
        if (is_excel(fmeta) && !is.null(input$metadata_sheet) &&
            input$metadata_sheet != "None") {
          wbm <- if (!is.null(input$metadata_file))
            openxlsx::loadWorkbook(fmeta$datapath) else wb1
          meta <- read_metadata_input(fmeta$datapath,
                                       input$metadata_sheet, wbm, format = fmeta$name)
        } else if (!is.null(input$metadata_file) && !is_excel(fmeta)) {
          meta <- read_metadata_input(fmeta$datapath, format = fmeta$name)
        }
          if (!is.null(meta) && !all(colnames(a) %in% rownames(meta)))
            shiny::showNotification("Some matrix samples have no metadata.",
                                    type = "warning", duration = 8)
        }
        explicit <- NULL
        if (!example && !is.null(input$order_file)) {
          explicit <- trimws(readLines(input$order_file$datapath, warn = FALSE))
          explicit <- sub("[,\t].*$", "", explicit)
          explicit <- explicit[nzchar(explicit) & explicit != "sample_id"]
          if (anyDuplicated(explicit)) stop("The imported sample order has duplicate IDs.")
          if (!any(explicit %in% colnames(a)))
            stop("The imported sample order matches no samples.")
        }
        order_for <- function(m, kind) {
          if (!is.null(explicit))
            return(c(intersect(explicit, colnames(m)),
                     setdiff(colnames(m), explicit)))
          matrix_order(m, kind, cluster = cluster_requested,
                       linkage = linkage)$ids
        }
        order1 <- order_for(a, kind1)
        shiny::incProgress(0.30)
        order2 <- if (!is.null(b)) order_for(b, kind2) else NULL
        shiny::incProgress(0.30)
        shiny::updateSelectInput(session, "metadata_column",
          choices = c("None" = "", if (!is.null(meta)) names(meta)),
          selected = if (example && !is.null(meta) && "group" %in% names(meta))
            "group" else "")
        shiny::showNotification(
          sprintf("Loaded %d isolates%s.", nrow(a),
                  if (!is.null(b)) paste0(" and ", nrow(b), " in matrix 2")
                  else ""),
          type = "message", duration = 4)
        list(first = a, second = b, metadata = meta,
             order1 = order1, order2 = order2,
             source = if (example) "synthetic example" else "uploaded files",
             matching = match_mode,
             clustering_skipped = cluster_requested &&
               is.null(explicit) &&
               (nrow(a) > 2000L || (!is.null(b) && nrow(b) > 2000L)),
             kind1 = kind1, kind2 = kind2,
             linkage = linkage)
      }, error = function(e) {
        shiny::showNotification(conditionMessage(e), type = "error",
                                duration = NULL)
        NULL
      })
    })
  }
  loaded <- shiny::reactiveVal(NULL)
  shiny::observeEvent(input$visualize, {
    loaded(analyze_input(example = FALSE))
  })
  shiny::observeEvent(input$load_example, {
    shiny::updateCheckboxInput(session, "comparable_scales", value = FALSE)
    shiny::updateSelectInput(session, "kind1", selected = "similarity")
    shiny::updateSelectInput(session, "kind2", selected = "similarity")
    shiny::updateRadioButtons(session, "match_mode", selected = "strict")
    shiny::updateCheckboxInput(session, "cluster", value = TRUE)
    shiny::updateSelectInput(session, "linkage", selected = "complete")
    shiny::updateNumericInput(session, "cluster_k", value = 3)
    loaded(analyze_input(example = TRUE))
  })
  output$download_example <- shiny::downloadHandler(
    filename = function() "MAniR_synthetic_example.xlsx",
    content = function(file) manir_write_example_workbook(file)
  )

  data <- shiny::reactive({ shiny::req(loaded()); loaded() })
  has_two <- shiny::reactive(!is.null(data()$second))
  subset_if_needed <- function(m, ids) {
    if (identical(colnames(m), ids) && identical(rownames(m), ids))
      m else m[ids, ids, drop = FALSE]
  }
  ordered_first <- shiny::reactive({
    d <- data()
    subset_if_needed(d$first, d$order1)
  })
  ordered_second <- shiny::reactive({
    d <- data()
    shiny::req(d$second)
    subset_if_needed(d$second, d$order2)
  })
  shared <- shiny::reactive({
    d <- data()
    shiny::req(d$second)
    match_matrices(d$first, d$second, d$matching)
  })
  combined_first <- shiny::reactive({
    d <- data()
    both <- shared()
    ids <- d$order1[d$order1 %in% both$ids]
    list(first = subset_if_needed(d$first, ids),
         second = subset_if_needed(d$second, ids))
  })
  combined_second <- shiny::reactive({
    d <- data()
    both <- shared()
    ids <- d$order2[d$order2 %in% both$ids]
    list(first = subset_if_needed(d$second, ids),
         second = subset_if_needed(d$first, ids))
  })
  # A checkbox cannot make two fundamentally different measurement types
  # subtractable. The bundled ANI/MALDI example is always protected.
  can_difference <- shiny::reactive({
    d <- data()
    !is.null(d$second) && isTRUE(input$comparable_scales) &&
      !identical(d$source, "synthetic example")
  })
  diff_matrix <- shiny::reactive({
    shiny::req(can_difference())
    combined_first()
  })
  format_range <- function(m) {
    values <- m[is.finite(m)]
    if (!length(values)) return("no finite values")
    sprintf("%s to %s", format(signif(min(values), 5L), trim = TRUE),
            format(signif(max(values), 5L), trim = TRUE))
  }
  visible_indices <- function(n) {
    ix <- if (isTRUE(input$zoom_enabled) && n > 300L) {
      start <- max(1L, min(n, as.integer(input$zoom_start)))
      seq.int(start, min(n, start + as.integer(input$zoom_size) - 1L))
    } else seq_len(n)
    if (length(ix) > 512L)
      ix <- ix[unique(as.integer(round(seq(1, length(ix), length.out = 512L))))]
    ix
  }
  pair_data <- shiny::reactive({
    m <- shared()
    paired_values(m$first, m$second, max_pairs = input$max_pairs)
  })

  output$overview <- shiny::renderUI({
    d <- data()
    shiny::tagList(
      shiny::h3("Dataset overview"),
      shiny::p(if (identical(d$source, "synthetic example"))
        "Synthetic teaching dataset. These are fictional similarities, not measured biological results."
        else "Uploaded dataset."),
      shiny::div(class = "metric", sprintf("Matrix 1: %s isolates",
                                           format(nrow(d$first), big.mark = ","))),
      if (!is.null(d$second))
        shiny::div(class = "metric", sprintf("Matrix 2: %s isolates; %s shared",
           format(nrow(d$second), big.mark = ","),
           format(length(shared()$ids), big.mark = ","))),
      if (!is.null(d$second) && d$matching == "intersection")
        shiny::p(sprintf("Excluded from comparison: %d only in matrix 1, %d only in matrix 2.",
           length(shared()$only_first), length(shared()$only_second))),
      if (d$clustering_skipped)
        shiny::p("Clustering was skipped above 2,000 isolates. Import a sample order for large datasets."),
      if (!is.null(d$second) && pair_data()$sampled)
        shiny::p(sprintf("Pairwise statistics are based on %s of %s distinct possible pairs, sampled with seed %s.",
           format(pair_data()$evaluated_pairs, big.mark = ","),
           format(pair_data()$total_pairs, big.mark = ","),
           pair_data()$seed)),
      if (!is.null(d$second) && !can_difference())
        shiny::p(if (identical(d$source, "synthetic example"))
          "ANI percentages and MALDI spectral similarities have different units. Look at Pearson/Spearman association and rank gaps; direct differences are disabled for this example."
          else "Direct differences are hidden until you confirm that the two matrices measure the same quantity in the same units.")
    )
  })
  output$agreement <- shiny::renderTable({
    shiny::req(has_two())
    pp <- pair_data()
    stats <- matrix_agreement(pp)
    data.frame(
      Measure = c("Valid isolate pairs", "Pearson r", "Spearman rho",
                  if (can_difference()) c("Mean absolute difference",
                                                  "Root mean squared difference")),
      Value = c(stats$n, signif(stats$pearson, 5),
                signif(stats$spearman, 5),
                if (can_difference())
                  c(signif(stats$mae, 5), signif(stats$rmse, 5)))
    )
  })
  output$mantel_note <- shiny::renderUI({
    shiny::p(class = "small-help",
      "Optional Mantel test: click Run Mantel test in the sidebar. It permutes whole isolate labels to test matrix association. It does not demonstrate equal typing performance, and it cannot run on incomplete matrices or above 400 shared isolates.")
  })
  output$replicate_table <- shiny::renderTable({
    d <- data()
    shiny::req(d$metadata, nzchar(input$metadata_column))
    replicate_summary(d$first, d$metadata, input$metadata_column,
                      max_pairs = input$max_pairs)
  })

  # Each plot is calculated only when its tab is shown. Palette changes do
  # not reload workbooks or recompute clustering.
  make_view <- function(prefix, source, name, combined = FALSE,
                        first_label = "Matrix 1", second_label = "Matrix 2",
                        is_difference = FALSE) {
    if (!is_difference) {
      output[[paste0(prefix, "_intro")]] <- shiny::renderUI({
        m <- source()
        d <- data()
        if (combined) {
          shiny::div(class = "section-note",
            shiny::strong(sprintf("Upper triangle: %s (original range %s). ",
                                  first_label, format_range(m$first))),
            shiny::strong(sprintf("Lower triangle: %s (original range %s).",
                                  second_label, format_range(m$second))),
            shiny::p(class = "small-help",
              "The common 0–1 color bar refers to separately normalized display colors, not a common measurement scale. A neutral diagonal separates the methods.")
          )
        } else {
          kind <- if (prefix == "plot1") d$kind1 else d$kind2
          shiny::p(class = "section-note",
            sprintf("%s contains %d isolates. Original data range: %s. Input type: %s. Colors are rescaled for display; hover to see actual values.",
                    name, nrow(m), format_range(m), kind))
        }
      })
      output[[paste0(prefix, "_legend")]] <- shiny::renderUI({
        d <- data()
        field <- input$metadata_column
        key <- ma_metadata_palette(d$metadata, field)
        if (is.null(key)) return(NULL)
        shiny::div(class = "metadata-key",
          shiny::span(class = "key-title",
                      sprintf("%s annotation:", field)),
          lapply(names(key), function(group)
            shiny::span(
              shiny::span(class = "swatch",
                          style = paste0("background-color:", unname(key[[group]]))),
              group
            )),
          shiny::span(class = "small-help",
            "(color track above the heatmap; not a similarity scale)")
        )
      })
    }
    output[[paste0(prefix, "_ui")]] <- shiny::renderUI({
      if (is_difference) shiny::req(can_difference())
      m <- source()
      size <- if (combined || is_difference) nrow(m$first) else nrow(m)
      if (size <= 300L)
        plotly::plotlyOutput(paste0(prefix, "_interactive"),
                             width = "100%", height = "750px")
      else shiny::tagList(
        shiny::p("Large-matrix raster preview. Click a cell to inspect its exact original value."),
        shiny::plotOutput(paste0(prefix, "_raster"),
                          width = "100%", height = "850px",
                          click = paste0(prefix, "_click"))
      )
    })
    output[[paste0(prefix, "_interactive")]] <- plotly::renderPlotly({
      if (is_difference) shiny::req(can_difference())
      m <- source()
      size <- if (combined || is_difference) nrow(m$first) else nrow(m)
      shiny::req(size <= 300L)
      if (is_difference) m <- m$first - m$second
      d <- data()
      meta <- if (is_difference || !isTRUE(nzchar(input$metadata_column)))
        NULL else d$metadata
      ma_interactive(m, name = name, palette = input$palette,
        show_numbers = input$show_numbers, combined = combined,
        first_name = first_label, second_name = second_label,
        log_scale = input$log_scale && !is_difference,
        center_zero = is_difference,
        metadata = meta, group_column = input$metadata_column)
    })
    output[[paste0(prefix, "_raster")]] <- shiny::renderPlot({
      if (is_difference) shiny::req(can_difference())
      m <- source()
      size <- if (combined || is_difference) nrow(m$first) else nrow(m)
      shiny::req(size > 300L)
      ix <- visible_indices(size)
      if (combined || is_difference) {
        # Slice first: do not build three full N*N combined copies for a preview.
        a <- m$first[ix, ix, drop = FALSE]
        b <- m$second[ix, ix, drop = FALSE]
        if (combined) {
          m <- ma_combined(a, b, input$log_scale)$values
          normal <- TRUE
        } else {
          m <- a - b
          normal <- FALSE
        }
      } else {
        m <- m[ix, ix, drop = FALSE]
        normal <- FALSE
      }
      graphics::par(mar = c(9, 9, 5, 2))
      d <- data()
      meta <- if (is_difference || !nzchar(input$metadata_column)) NULL
              else d$metadata
      ma_raster(m, palette = input$palette, title = name,
                metadata = meta, group_column = input$metadata_column,
                normalized = normal, log_scale = input$log_scale && !is_difference,
                center_zero = is_difference)
      if (length(ix) < size)
        graphics::mtext(if (isTRUE(input$zoom_enabled))
          sprintf("Zoom region; %d of %d isolates (starting at %d).",
                  length(ix), size, min(ix))
          else sprintf("Representative overview: %d of %d isolates. Zoom for exact detail.",
                       length(ix), size), side = 3, line = 0.1, cex = 0.75)
    }, res = 110)
    output[[paste0(prefix, "_cell")]] <- shiny::renderText({
      shiny::req(input[[paste0(prefix, "_click")]])
      if (is_difference) shiny::req(can_difference())
      m <- source()
      base <- if (combined || is_difference) m$first else m
      ix <- visible_indices(nrow(base))
      click <- input[[paste0(prefix, "_click")]]
      col <- as.integer(round(click$x))
      row <- as.integer(length(ix) + 1L - round(click$y))
      if (is.na(col) || is.na(row) || col < 1L ||
          row < 1L || col > length(ix) || row > length(ix)) return("")
      i <- ix[row]
      j <- ix[col]
      value <- if (combined && i > j) m$second[i, j]
               else if (is_difference) m$first[i, j] - m$second[i, j]
               else if (combined) m$first[i, j] else m[i, j]
      sprintf("%s / %s: %s%s", rownames(base)[i],
              colnames(base)[j], format(value, digits = 9),
              if (combined) if (i < j) paste0(" (", first_label, ")")
              else if (i > j) paste0(" (", second_label, ")")
              else " (diagonal)" else "")
    })
  }
  make_view("plot1", ordered_first, "Matrix 1")
  make_view("plot2", ordered_second, "Matrix 2")
  make_view("combined1", combined_first, "Combined, ordered by matrix 1",
            combined = TRUE)
  make_view("combined2", combined_second, "Combined, ordered by matrix 2",
            combined = TRUE, first_label = "Matrix 2", second_label = "Matrix 1")
  make_view("difference", diff_matrix, "Matrix 1 minus matrix 2",
            is_difference = TRUE)

  output$pairwise_intro <- shiny::renderUI({
    shiny::req(has_two())
    d <- data()
    p <- pair_data()
    axis1 <- if (identical(d$source, "synthetic example"))
      "fictional ANI-like percentage" else paste("matrix 1", d$kind1)
    axis2 <- if (identical(d$source, "synthetic example"))
      "fictional MALDI-like similarity (0–1)" else paste("matrix 2", d$kind2)
    shiny::p(class = "section-note",
      sprintf("%s distinct pairs with a value in both matrices, of %s possible pairs. X = %s; Y = %s.%s",
              format(p$valid_pairs, big.mark = ","),
              format(p$total_pairs, big.mark = ","),
              axis1, axis2,
              if (p$sampled)
                sprintf(" Evaluated a reproducible sample of %s pairs (seed %d).",
                        format(p$evaluated_pairs, big.mark = ","), p$seed)
              else " All pairs are included."))
  })
  output$scatter <- shiny::renderPlot({
    shiny::req(has_two())
    p <- pair_data()
    d <- p$data
    shiny::validate(shiny::need(nrow(d) >= 2L,
      "At least two nonmissing sample pairs are needed."))
    current <- data()
    first_label <- if (identical(current$source, "synthetic example"))
      "Illustrative ANI (%)" else paste("Matrix 1 (", current$kind1, ")", sep = "")
    second_label <- if (identical(current$source, "synthetic example"))
      "Illustrative MALDI similarity (0–1)" else
        paste("Matrix 2 (", current$kind2, ")", sep = "")
    graphics::plot(d$first, d$second, pch = 19L,
                   col = grDevices::adjustcolor("#305f86", alpha.f = 0.58),
                   cex = 0.9, xlab = first_label, ylab = second_label,
                   main = sprintf("Distinct isolate pairs (n = %s%s)",
                     format(nrow(d), big.mark = ","),
                     if (p$sampled) ", sampled" else ""))
    if (can_difference()) graphics::abline(0, 1, lty = 2, col = "#555555")
    if (nrow(d) >= 3L) {
      gap <- pair_rank_gaps(p, current$kind1, current$kind2, top = 5L)
      key <- paste(d$sample_1, d$sample_2, sep = "|")
      highlighted <- match(paste(gap$sample_1, gap$sample_2, sep = "|"), key)
      graphics::points(d$first[highlighted], d$second[highlighted],
                       pch = 1L, cex = 1.65, lwd = 1.5, col = "#c45a35")
    }
  })
  output$rank_gaps <- shiny::renderTable({
    shiny::req(has_two())
    current <- data()
    gap <- pair_rank_gaps(pair_data(), current$kind1, current$kind2, top = 10L)
    if (!nrow(gap)) return(NULL)
    names(gap) <- c("Isolate 1", "Isolate 2", "Matrix 1", "Matrix 2", "Rank gap")
    gap
  }, digits = 4, rownames = FALSE)
  output$absolute_discrepancies_intro <- shiny::renderUI({
    if (!can_difference()) return(NULL)
    shiny::tagList(
      shiny::h4("Largest absolute differences"),
      shiny::p(class = "small-help",
        "These raw differences are available only because the two matrices have been marked as directly comparable. Sort by their magnitude for descriptive inspection, not a significance test.")
    )
  })
  output$discrepancies <- shiny::renderTable({
    shiny::req(has_two(), can_difference())
    d <- pair_data()$data
    d <- d[order(abs(d$difference), decreasing = TRUE), , drop = FALSE]
    utils::head(d, 20L)
  }, digits = 5)

  concordance <- shiny::reactive({
    shiny::req(has_two())
    m <- shared()
    shiny::validate(shiny::need(nrow(m$first) <= 2000L,
         "Cluster concordance is limited to 2,000 shared isolates."))
    shiny::validate(shiny::need(input$cluster_k < nrow(m$first),
         "Choose fewer clusters than shared isolates."))
    cluster_concordance(m$first, m$second, kind_a = data()$kind1,
                        kind_b = data()$kind2, k = input$cluster_k,
                        linkage = data()$linkage)
  })
  output$cluster_intro <- shiny::renderUI({
    shiny::req(has_two())
    d <- data()
    shiny::p(class = "section-note",
      sprintf("Clustering %d shared isolates independently in each matrix, using %d clusters and %s linkage. Change k in the sidebar to explore the stability of agreement.",
              length(shared()$ids), input$cluster_k, d$linkage))
  })
  output$cluster_summary <- shiny::renderTable({
    c <- concordance()
    data.frame(measure = c("Adjusted Rand index",
                             "Adjusted Wallace (matrix 1 -> matrix 2)",
                             "Adjusted Wallace (matrix 2 -> matrix 1)"),
               value = c(c$ari, c$adjusted_wallace_1_to_2,
                         c$adjusted_wallace_2_to_1))
  })
  output$cluster_overlap <- shiny::renderTable({
    x <- concordance()$contingency
    overlap <- as.data.frame.matrix(x)
    names(overlap) <- paste("Matrix 2 cluster", colnames(x))
    overlap <- data.frame("Matrix 1 cluster" = rownames(x), overlap,
                          check.names = FALSE, row.names = NULL)
    overlap
  }, rownames = FALSE)
  output$cluster_table <- shiny::renderTable({
    assignment <- utils::head(concordance()$assignments, 100L)
    names(assignment) <- c("Isolate", "Matrix 1 cluster", "Matrix 2 cluster")
    assignment
  }, rownames = FALSE)
  output$metadata_preview <- shiny::renderTable({
    shiny::req(data()$metadata)
    utils::head(data()$metadata, 100L)
  }, rownames = TRUE)
  output$difference_explainer <- shiny::renderUI({
    d <- data()
    if (is.null(d$second))
      return(shiny::p(class = "section-note",
        "Upload a second matrix, or load the example, before comparing pairs."))
    if (identical(d$source, "synthetic example"))
      return(shiny::div(class = "reading-guide",
        shiny::strong("Unavailable for the example"),
        shiny::p("The fictional ANI measurements are percentage-like (about 95–100), while the fictional MALDI similarities range from 0 to 1. These are different measurement types. The application deliberately blocks direct subtraction for this example. Use the pairwise rank-gap table or the two combined heatmaps instead.")))
    if (!can_difference())
      return(shiny::p(class = "section-note",
        "To enable a difference map, confirm in the sidebar that both matrices measure the same quantity in the same units and were processed compatibly."))
    shiny::p(class = "section-note",
      "Difference = matrix 1 minus matrix 2 in original units. Zero is the neutral midpoint; positive and negative values appear on opposite sides. Colors are scaled symmetrically around zero, so hover or export the exact matrix for numerical comparisons.")
  })
  output$download_difference_ui <- shiny::renderUI({
    if (!can_difference()) return(NULL)
    shiny::downloadButton("download_difference", "Exact difference matrix CSV")
  })
  output$difference_download_notice <- shiny::renderUI({
    d <- data()
    if (!can_difference())
      shiny::p(class = "small-help",
        if (identical(d$source, "synthetic example"))
          "The difference export is disabled for the mixed-unit example."
        else "The difference export appears after you confirm comparable measurements and load both matrices.")
  })
  output$metadata_note <- shiny::renderUI({
    shiny::req(data()$metadata)
    shiny::p("Showing up to 100 metadata rows. Choose a categorical metadata column in the sidebar to compare within-group and between-group matrix values.")
  })

  mantel_result <- shiny::eventReactive(input$run_mantel, {
    shiny::req(has_two())
    tryCatch({
      x <- shared()
      matrix_mantel(x$first, x$second, permutations = input$permutations)
    }, error = function(e) list(error = conditionMessage(e)))
  })
  output$mantel_result <- shiny::renderPrint({
    shiny::req(mantel_result())
    r <- mantel_result()
    if (!is.null(r$error)) cat("Mantel test: ", r$error, "\n")
    else cat(sprintf("Mantel test: r = %.5f, two-sided permutation p = %.5g; %d label permutations, seed %d.\n",
                     r$observed, r$p_two_sided, r$permutations, r$seed))
  })

  output$download_first <- shiny::downloadHandler(
    filename = function() "MAniR_matrix1.csv",
    content = function(file) write_matrix_csv(data()$first, file))
  output$download_second <- shiny::downloadHandler(
    filename = function() "MAniR_matrix2.csv",
    content = function(file) {
      shiny::req(data()$second)
      write_matrix_csv(data()$second, file)
    })
  output$download_pairs <- shiny::downloadHandler(
    filename = function() "MAniR_pairwise_values.csv",
    content = function(file) {
      shiny::req(has_two())
      utils::write.csv(pair_data()$data, file, row.names = FALSE)
    })
  output$download_clusters <- shiny::downloadHandler(
    filename = function() "MAniR_clusters.csv",
    content = function(file) utils::write.csv(
      concordance()$assignments, file, row.names = FALSE))
  output$download_rds <- shiny::downloadHandler(
    filename = function() "MAniR_matrices.rds",
    content = function(file) {
      d <- data()
      saveRDS(list(first = d$first, second = d$second,
                   metadata = d$metadata, order1 = d$order1,
                   order2 = d$order2, matching = d$matching,
                   kinds = c(d$kind1, d$kind2)), file)
    })
  output$download_first_png <- shiny::downloadHandler(
    filename = function() "MAniR_matrix1.png",
    content = function(file)
      ma_save_png(ordered_first(), file, palette = input$palette,
                  title = "Matrix 1", log_scale = input$log_scale))
  output$download_combined_png <- shiny::downloadHandler(
    filename = function() "MAniR_combined.png",
    content = function(file) {
      m <- combined_first()
      # Export no more than 1,200 representative isolates to bound memory,
      # then calculate display normalization on that exact exported subset.
      ix <- ma_preview(m$first, max_side = 1200L)$indices
      a <- m$first[ix, ix, drop = FALSE]
      b <- m$second[ix, ix, drop = FALSE]
      shown <- ma_combined(a, b, input$log_scale)$values
      ma_save_png(shown, file, palette = input$palette,
                  title = sprintf("Combined matrix, %d of %d isolates", length(ix),
                                  nrow(m$first)), normalized = TRUE)
    })
  output$download_pdf <- shiny::downloadHandler(
    filename = function() "MAniR_matrix1.pdf",
    content = function(file)
      ma_save_pdf(ordered_first(), file, palette = input$palette,
                  title = "Matrix 1", log_scale = input$log_scale))
  output$download_svg <- shiny::downloadHandler(
    filename = function() "MAniR_matrix1.svg",
    content = function(file)
      ma_save_svg(ordered_first(), file, palette = input$palette,
                  title = "Matrix 1", log_scale = input$log_scale))
  output$download_html <- shiny::downloadHandler(
    filename = function() "MAniR_interactive_preview.html",
    content = function(file) {
      m <- ordered_first()
      ix <- ma_preview(m, max_side = 300L)$indices
      widget <- ma_interactive(m[ix, ix, drop = FALSE],
        name = sprintf("Matrix 1 (%d of %d isolates)", length(ix), nrow(m)),
        palette = input$palette, show_numbers = input$show_numbers,
        log_scale = input$log_scale)
      htmlwidgets::saveWidget(widget, file = file, selfcontained = TRUE)
    })
  output$download_difference <- shiny::downloadHandler(
    filename = function() "MAniR_difference_matrix.csv",
    content = function(file) {
      shiny::req(can_difference())
      m <- combined_first()
      # Full difference matrices are intentionally materialized only when
      # explicitly requested for export.
      write_matrix_csv(m$first - m$second, file)
    })
  output$download_settings <- shiny::downloadHandler(
    filename = function() "MAniR_analysis_settings.txt",
    content = function(file) {
      d <- data()
      writeLines(c(
        paste("MAniR generated:", Sys.time()),
        paste("First matrix:", nrow(d$first), "isolates"),
        paste("Second matrix:", if (is.null(d$second)) "None" else nrow(d$second)),
        paste("Matrix kinds:", d$kind1, d$kind2),
        paste("Sample matching:", d$matching),
        paste("Cluster requested:", input$cluster),
        paste("Cluster limit:", 2000L),
        paste("Linkage:", d$linkage),
        paste("Color palette:", input$palette),
        paste("Log display scaling:", input$log_scale),
        paste("Comparable units selected:", input$comparable_scales),
        paste("Difference analysis permitted:", can_difference()),
        paste("Maximum analyzed pairs:", input$max_pairs),
        paste("Pair sampling seed:", 1L),
        paste("Mantel seed:", 1L),
        paste("Session:", paste(utils::capture.output(utils::sessionInfo()),
                               collapse = "\n"))
      ), file)
    })
}
