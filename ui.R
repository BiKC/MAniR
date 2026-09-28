# MAniR: matrix comparison and scalable visualization.
manir_guide <- function(lead, details = NULL) {
  # Help stays one short line until opened, leaving the visualization visible.
  preview <- strsplit(lead, "[.!?]")[[1L]][1L]
  if (nchar(preview) > 108L) preview <- paste0(substr(preview, 1L, 105L), "...")
  shiny::tags$details(class = "reading-guide",
    shiny::tags$summary(shiny::strong("How to read this"),
                        shiny::span(class = "guide-preview", preview)),
    shiny::div(class = "guide-body",
      shiny::p(lead),
      if (!is.null(details))
        shiny::tagList(shiny::strong("Further interpretation"), shiny::p(details))
    )
  )
}
ui <- shiny::fluidPage(
  shiny::tags$head(
    shiny::tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    shiny::tags$style(shiny::HTML("
      body { background: #f7f9fb; color: #213547; }
      .well { background: #fff; border-color: #dae2ea; }
      .main-panel { background: #fff; padding: 18px; border: 1px solid #dae2ea;
                    border-radius: 8px; }
      .small-help { color: #56697a; font-size: .9em; margin-bottom: 10px; }
      .plot-area { min-height: 520px; }
      .metric { padding: 10px; margin: 6px 0; background: #f2f6f9;
                border-left: 3px solid #487f9b; }
      .tab-content { padding-top: 15px; }
      .reading-guide { border-left: 3px solid #3c7494; background: #eef5fa;
                       padding: 12px 16px; margin: 12px 0 20px; color: #234257;
                       line-height: 1.45; border-radius: 4px; }
      .reading-guide p { margin: 6px 0 0; }
      .reading-guide details { margin-top: 8px; }
      .reading-guide summary { cursor: pointer; font-weight: 600; }
      .reading-guide details p { margin-top: 8px; max-width: 88ch; }
      .metadata-key { display: flex; flex-wrap: wrap; align-items: center;
                      gap: 7px 16px; margin: 8px 0 3px; font-size: 13px; }
      .metadata-key .swatch { display: inline-block; width: 15px; height: 13px;
                              border-radius: 2px; margin-right: 5px;
                              vertical-align: middle; }
      .metadata-key .key-title { font-weight: 600; color: #29465c; }
      .section-note { margin: 13px 0 8px; color: #3b5668; }
      .results-table { margin: 12px 0 20px; }
      .main-panel .btn { margin: 7px 7px 7px 0; }
    ")),
    shiny::tags$link(rel = "stylesheet", type = "text/css",
                     href = "workspace.css")
  ),
  shiny::div(class = "app-header",
    shiny::h2("MAniR"),
    shiny::span(class = "app-description",
      "Compare pairwise similarities, correlations and distances")
  ),
  shiny::div(class = "workspace",
  shiny::sidebarLayout(
    shiny::sidebarPanel(width = 3,
      shiny::div(class = "control-sidebar",
        shiny::div(class = "sidebar-scroll",
          shiny::div(class = "example-quick",
            shiny::h4("Try MAniR with example data"),
            shiny::p("12 synthetic isolates with ANI-like, MALDI-like and metadata matrices."),
            shiny::actionButton("load_example", "Load example", class = "btn-primary"),
            shiny::tags$details(
              shiny::tags$summary("About the example / download"),
              shiny::p("Synthetic demonstration data, not measured biological values."),
              shiny::downloadButton("download_example", "Example workbook (.xlsx)")
            )
          ),
          shiny::tags$details(class = "control-group", open = "open",
            shiny::tags$summary("Upload matrices"),
            shiny::div(class = "group-body",
              shiny::fileInput("first_file", "Matrix 1",
                accept = c(".xlsx", ".xlsm", ".csv", ".tsv", ".txt", ".gz", ".rds")),
              shiny::uiOutput("first_sheet_ui"),
              shiny::fileInput("second_file", "Matrix 2 (optional)",
                accept = c(".xlsx", ".xlsm", ".csv", ".tsv", ".gz", ".rds")),
              shiny::uiOutput("second_sheet_ui"),
              shiny::fileInput("metadata_file", "Metadata (optional)",
                accept = c(".xlsx", ".csv", ".tsv", ".rds")),
              shiny::uiOutput("metadata_sheet_ui"),
              shiny::fileInput("order_file", "Sample order (optional)",
                accept = c(".txt", ".csv", ".tsv")),
              shiny::selectInput("kind1", "Matrix 1 represents",
                choices = c("Similarity" = "similarity", "Correlation" = "correlation",
                            "Distance" = "distance", "Unspecified" = "auto")),
              shiny::selectInput("kind2", "Matrix 2 represents",
                choices = c("Similarity" = "similarity", "Correlation" = "correlation",
                            "Distance" = "distance", "Unspecified" = "auto")),
              shiny::radioButtons("match_mode", "Compare samples",
                choices = c("Identical IDs" = "strict",
                            "Shared isolates" = "intersection"),
                selected = "strict"),
              shiny::p(class = "small-help",
                "Similarity: larger means closer. Distance: smaller means closer. Correlation is between -1 and 1.")
            )
          ),
          shiny::tags$details(class = "control-group",
            shiny::tags$summary("Analysis settings"),
            shiny::div(class = "group-body",
              shiny::checkboxInput("cluster", "Cluster samples on load", value = TRUE),
              shiny::selectInput("linkage", "Clustering linkage",
                choices = c("Complete (original default)" = "complete",
                            "Average" = "average", "Single" = "single",
                            "Ward D2" = "ward.D2"), selected = "complete"),
              shiny::checkboxInput("comparable_scales",
                "Same measurement and units in both matrices", value = FALSE),
              shiny::p(class = "small-help",
                "Only enable raw differences when measurements are directly comparable. ANI percentages and MALDI scores are different quantities."),
              shiny::numericInput("permutations", "Mantel permutations",
                999L, min = 99L, max = 9999L),
              shiny::p(class = "small-help",
                "Changing the matrix types, linkage, matching rule or clustering requires loading again. Display controls on the right update immediately.")
            )
          )
        ),
        shiny::div(class = "sidebar-footer",
          shiny::actionButton("visualize", "Load and analyze uploaded files",
                              class = "btn-primary"),
          shiny::uiOutput("sidebar_status")
        )
      )
    ),
    shiny::mainPanel(width = 9,
      shiny::div(class = "main-panel",
        shiny::div(class = "results-toolbar",
          shiny::conditionalPanel(
            condition = "input.results_tab === 'Heatmaps'",
            shiny::div(class = "toolbar-field",
              shiny::selectInput("palette", "Colors",
                choices = c("RdBu", "BrBG", "PiYG", "PRGn", "PuOr", "RdYlBu",
                            "Viridis", "YlOrRd", "Blues", "Greens", "Greys"),
                selected = "RdBu")
            )
          ),
          shiny::div(class = "toolbar-field",
            shiny::selectInput("metadata_column", "Annotation",
              choices = c("None" = ""))
          ),
          shiny::conditionalPanel(
            condition = "input.results_tab === 'Heatmaps'",
            shiny::div(class = "toolbar-field control-checkbox",
              shiny::checkboxInput("show_numbers", "Show cell values", value = FALSE)
            )
          ),
          shiny::conditionalPanel(
            condition = "input.results_tab === 'Pairwise comparison'",
            shiny::div(class = "toolbar-field contextual pair-limit",
              shiny::numericInput("max_pairs", "Max pairs",
                100000L, min = 1000L, max = 1000000L, step = 1000L)
            )
          ),
          shiny::conditionalPanel(
            condition = "input.results_tab === 'Cluster comparison'",
            shiny::div(class = "toolbar-field contextual",
              shiny::numericInput("cluster_k", "Clusters (k)",
                3L, min = 2L, max = 100L)
            )
          ),
          shiny::conditionalPanel(
            condition = "input.results_tab === 'Overview'",
            shiny::div(class = "toolbar-field control-checkbox",
              shiny::actionButton("run_mantel", "Run Mantel test",
                                  class = "btn-default btn-sm")
            )
          ),
          shiny::conditionalPanel(
            condition = "input.results_tab === 'Heatmaps'",
            shiny::tags$details(class = "toolbar-more",
              shiny::tags$summary("Display options"),
              shiny::div(class = "toolbar-extra",
                shiny::div(class = "toolbar-field control-checkbox",
                  shiny::checkboxInput("log_scale", "Log(1+x) color scaling",
                    value = FALSE)
                ),
                shiny::div(class = "toolbar-field control-checkbox",
                  shiny::checkboxInput("zoom_enabled", "Zoom into large matrices",
                    value = FALSE)
                ),
                shiny::conditionalPanel(
                  condition = "input.zoom_enabled",
                  shiny::div(class = "toolbar-field",
                    shiny::numericInput("zoom_start", "First isolate",
                      1L, min = 1L, step = 1L)
                  ),
                  shiny::div(class = "toolbar-field",
                    shiny::numericInput("zoom_size", "Number of isolates",
                      300L, min = 10L, max = 1000L, step = 50L)
                  )
                )
              )
            )
          ),
          shiny::div(class = "toolbar-status",
            shiny::uiOutput("active_analysis")
          )
        ),
        shiny::div(class = "results-shell",
        shiny::tabsetPanel(id = "results_tab",
          shiny::tabPanel("Overview",
            manir_guide(
              "Start with the number of shared isolates and the relationship between the two matrices. Correlations show whether pairs that are high in one measurement also tend to be high in the other; they do not establish equivalence.",
              "Pearson's r summarizes a linear relationship. Spearman's rho compares ranks and can detect a monotonic relationship on different scales. The same isolate appears in several pairs, so ordinary independent-observation p-values would be inappropriate. Use the optional Mantel permutation test for matrix association."),
            shiny::uiOutput("overview"),
            shiny::h4("Association between matrices"),
            shiny::tableOutput("agreement"),
            shiny::uiOutput("mantel_note"),
            shiny::verbatimTextOutput("mantel_result"),
            shiny::h4("Within- and between-group values"),
            shiny::p(class = "small-help",
              "Select a metadata category in the sidebar to describe how the first matrix differs within and between those groups. These are descriptive pairwise summaries, not an independent-sample test."),
            shiny::tableOutput("replicate_table")),
          shiny::tabPanel("Heatmaps",
            shiny::div(class = "matrix-nav",
              shiny::tabsetPanel(id = "matrix_view", type = "pills",
              shiny::tabPanel("Matrix 1",
                manir_guide(
                  "Every cell compares the isolate named on its row with the isolate named on its column. The diagonal compares an isolate with itself. Larger values mean more similar samples only for similarity and positive-correlation measurements.",
                  "Clustering puts isolates with related measurements next to each other; it does not change the values. The color bar shows a 0–1 display scale based on the data range, not necessarily the original numerical units. Hover over a cell for its original value. Metadata colors identify categories, not measured similarity."),
                shiny::uiOutput("plot1_intro"), shiny::uiOutput("plot1_legend"),
                shiny::uiOutput("plot1_ui"), shiny::verbatimTextOutput("plot1_cell")),
              shiny::tabPanel("Matrix 2",
                manir_guide(
                  "Read this matrix in the same way as matrix 1. Its clustering order can differ, and the display colors are scaled to this matrix's own numerical range.",
                  "Equal colors across the separate Matrix 1 and Matrix 2 plots do not imply equal biological measurements. Hover over any cell for the exact original value. Choose the same metadata track to see whether visual clusters correspond to the same sample categories."),
                shiny::uiOutput("plot2_intro"), shiny::uiOutput("plot2_legend"),
                shiny::uiOutput("plot2_ui"), shiny::verbatimTextOutput("plot2_cell")),
              shiny::tabPanel("Combined (order 1)",
                manir_guide(
                  "This split heatmap places matrix 1 above the diagonal and matrix 2 below it. Both triangles use the ordering calculated from matrix 1, so the same pair can be inspected in both methods.",
                  "Each triangle is normalized independently for display. Comparing the position of clusters is meaningful; directly comparing two color shades or subtracting their display values is not. The diagonal is displayed with a neutral midpoint color because it separates the two methods, not because its original measurements equal 0.5."),
                shiny::uiOutput("combined1_intro"), shiny::uiOutput("combined1_legend"),
                shiny::uiOutput("combined1_ui"), shiny::verbatimTextOutput("combined1_cell")),
              shiny::tabPanel("Combined (order 2)",
                manir_guide(
                  "The same split comparison, now ordered using matrix 2. The upper triangle is matrix 2 and the lower triangle is matrix 1.",
                  "Switch between both combined tabs to see how each method's ordering groups isolates. Triangle colors remain independently scaled and the diagonal is a visual separator. Inspect original values in the cell hover rather than using color as a shared measurement."),
                shiny::uiOutput("combined2_intro"), shiny::uiOutput("combined2_legend"),
                shiny::uiOutput("combined2_ui"), shiny::verbatimTextOutput("combined2_cell")),
              shiny::tabPanel("Difference",
                manir_guide(
                  "This tab calculates matrix 1 minus matrix 2 for each shared isolate pair. Zero is the midpoint of the diverging color scale; opposite sides represent opposite signs.",
                  "A difference is interpretable only when both matrices measure the same quantity in the same units and have compatible preprocessing. Two different measurement types, such as ANI percentages and MALDI spectral scores, should be examined with rank comparisons instead."),
                shiny::uiOutput("difference_explainer"), shiny::uiOutput("difference_ui"),
                shiny::verbatimTextOutput("difference_cell"))
              )
            )
          ),
          shiny::tabPanel("Pairwise comparison",
            manir_guide(
              "Each dot is one distinct, unordered isolate pair. Its X position is the pair's value in matrix 1 and its Y position is the value for the same pair in matrix 2. Neither the diagonal nor mirrored duplicates are counted.",
              "An upward pattern suggests positive association, while points far from the main pattern merit closer inspection. Different units are allowed here: compare ranks or correlations, not the raw distance from a one-to-one line. Pairwise observations share isolates, so they are not statistically independent."),
            shiny::uiOutput("pairwise_intro"),
            shiny::div(class = "plot-frame",
              shiny::plotOutput("scatter", height = "100%")
            ),
            shiny::h4("Pairs with the largest rank differences"),
            shiny::p(class = "small-help",
              "These are exploratory discrepancies in the relative ordering of pairs, not statistically significant outliers. A rank gap near 0 means the pair has a similar position among all evaluated pairs in both matrices. Orange rings mark up to five of these pairs in the scatterplot."),
            shiny::tableOutput("rank_gaps"),
            shiny::uiOutput("absolute_discrepancies_intro"),
            shiny::tableOutput("discrepancies"),
            shiny::downloadButton("download_pairs", "Download pairwise values (CSV)")),
          shiny::tabPanel("Cluster comparison",
            manir_guide(
              "Each matrix is independently clustered and cut into the same chosen number of groups (k). The statistics compare which isolate pairs are grouped together, regardless of the arbitrary cluster numbers.",
              "Adjusted Rand index (ARI) equals 1 for identical partitions and is near 0 for chance-level agreement under its adjustment model. Adjusted Wallace is directional: matrix 1 to matrix 2 asks whether pairs grouped together in the first remain together in the second, adjusted for chance. Reverse the direction for the other value. These numbers describe agreement, not biological accuracy or a probability that either method is correct."),
            shiny::uiOutput("cluster_intro"),
            shiny::tableOutput("cluster_summary"),
            shiny::h4("How the clusters overlap"),
            shiny::tableOutput("cluster_overlap"),
            shiny::h4("Cluster membership by isolate"),
            shiny::p(class = "small-help",
              "Cluster numbers are arbitrary. Cluster 1 from one matrix is not necessarily the same group as cluster 1 from the other."),
            shiny::tableOutput("cluster_table"),
            shiny::downloadButton("download_clusters", "Download cluster assignments")),
          shiny::tabPanel("Metadata",
            manir_guide(
              "Metadata describe each isolate, such as its group, specimen source or experimental batch. Select a categorical field in the sidebar to show a colored annotation track next to the heatmaps.",
              "Metadata categories are not automatically cluster labels or validation truth. A batch-associated cluster may reflect an experimental effect and needs separate investigation. Numeric metadata should not be interpreted as categories unless intentionally converted."),
            shiny::tableOutput("metadata_preview"),
            shiny::uiOutput("metadata_note")),
          shiny::tabPanel("Export",
            manir_guide(
              "CSV and RDS preserve the original matrix values. PNG, PDF, SVG and interactive HTML preserve the displayed ordering and palette, but figure colors may be normalized or raster-sampled.",
              "Plots from large datasets are representative previews, not complete value tables. Export the CSV or RDS alongside your figures and save the settings manifest to record clustering, color mapping, pair sampling and your R environment."),
            shiny::uiOutput("difference_download_notice"),
            shiny::downloadButton("download_first", "Matrix 1 CSV"),
            shiny::downloadButton("download_second", "Matrix 2 CSV"),
            shiny::downloadButton("download_rds", "Analysis matrices (RDS)"),
            shiny::downloadButton("download_first_png", "Matrix 1 PNG"),
            shiny::downloadButton("download_pdf", "Matrix 1 PDF"),
            shiny::downloadButton("download_svg", "Matrix 1 SVG"),
            shiny::downloadButton("download_html", "Interactive HTML preview"),
            shiny::downloadButton("download_combined_png", "Combined PNG"),
            shiny::uiOutput("download_difference_ui"),
            shiny::downloadButton("download_settings", "Analysis settings (text)")
          )
        )
        )
      )
    )
  )
  )
)
