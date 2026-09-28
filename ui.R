# MAniR: matrix comparison and scalable visualization.
manir_guide <- function(lead, details = NULL) {
  shiny::div(class = "reading-guide",
    shiny::strong("How to read this"),
    shiny::p(lead),
    if (!is.null(details))
      shiny::tags$details(
        shiny::tags$summary("More explanation"),
        shiny::p(details)
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
    "))
  ),
  shiny::titlePanel("MAniR"),
  shiny::p("Explore and compare pairwise similarity, correlation or distance matrices."),
  shiny::sidebarLayout(
    shiny::sidebarPanel(width = 3,
      shiny::div(class = "metric",
        shiny::h4("Try an example"),
        shiny::p("Explore 12 fictional isolates, two similarity matrices and sample metadata. No upload needed."),
        shiny::actionButton("load_example", "Load example dataset",
                             class = "btn-primary"),
        shiny::tags$br(), shiny::tags$br(),
        shiny::downloadButton("download_example", "Download example XLSX"),
        shiny::p(class = "small-help",
          "Synthetic data for demonstration, not real biological measurements.")
      ),
      shiny::hr(),
      shiny::h4("Upload your data"),
      shiny::p(class = "small-help",
        "For very large matrices, CSV or RDS is preferable to Excel.
         Import uses RAM for the matrix and temporary validation data."),
      shiny::fileInput("first_file", "First matrix",
                       accept = c(".xlsx", ".xlsm", ".csv", ".tsv", ".txt",
                                  ".gz", ".rds")),
      shiny::uiOutput("first_sheet_ui"),
      shiny::fileInput("second_file", "Second matrix file (optional)",
                       accept = c(".xlsx", ".xlsm", ".csv", ".tsv", ".gz", ".rds")),
      shiny::uiOutput("second_sheet_ui"),
      shiny::fileInput("metadata_file", "Separate metadata file (optional)",
                       accept = c(".xlsx", ".csv", ".tsv", ".rds")),
      shiny::uiOutput("metadata_sheet_ui"),
      shiny::fileInput("order_file", "Optional sample order (one ID per line)",
                       accept = c(".txt", ".csv", ".tsv")),
      shiny::selectInput("kind1", "Matrix 1 represents",
        choices = c("Similarity" = "similarity", "Correlation" = "correlation",
                    "Distance" = "distance", "Unspecified" = "auto")),
      shiny::selectInput("kind2", "Matrix 2 represents",
        choices = c("Similarity" = "similarity", "Correlation" = "correlation",
                    "Distance" = "distance", "Unspecified" = "auto")),
      shiny::p(class = "small-help",
        "Similarity: larger values mean closer samples. Distance: smaller values mean closer samples. Correlation ranges from -1 to +1; larger values indicate a more positive relationship."),
      shiny::radioButtons("match_mode", "Compare samples",
        choices = c("Require identical IDs" = "strict",
                    "Shared isolates only" = "intersection"),
        selected = "strict"),
      shiny::actionButton("visualize", "Load and analyze", class = "btn-primary"),
      shiny::hr(),
      shiny::h4("Visualization"),
      shiny::checkboxInput("cluster", "Cluster samples", value = TRUE),
      shiny::selectInput("linkage", "Clustering linkage",
        choices = c("Complete (historical default)" = "complete",
                    "Average" = "average", "Single" = "single",
                    "Ward D2" = "ward.D2"), selected = "complete"),
      shiny::selectInput("palette", "Palette",
        choices = c("RdBu", "BrBG", "PiYG", "PRGn", "PuOr", "RdYlBu",
                    "Viridis", "YlOrRd", "Blues", "Greens", "Greys"),
        selected = "RdBu"),
      shiny::checkboxInput("zoom_enabled", "Zoom into a region in large matrices", FALSE),
      shiny::numericInput("zoom_start", "First isolate index", 1, min = 1, step = 1),
      shiny::numericInput("zoom_size", "Isolates in zoom region", 300,
                          min = 10, max = 1000, step = 50),
      shiny::checkboxInput("show_numbers", "Display cell numbers in small plots",
                           value = FALSE),
      shiny::checkboxInput("log_scale", "Logarithmic display scaling",
                           value = FALSE),
      shiny::selectInput("metadata_column", "Categorical annotation track",
                         choices = c("None" = "")),
      shiny::checkboxInput("comparable_scales",
        "Both matrices measure the same quantity on the same scale",
        value = FALSE),
      shiny::p(class = "small-help",
        "Check this only for directly comparable measurements, such as ANI from two sequencing runs. Do not check it for ANI percentages versus MALDI scores, even after normalizing the heatmap colors."),
      shiny::helpText("For large matrices, plots use representative pixels. Numerical analyses and exports use original values unless stated otherwise."),
      shiny::hr(),
      shiny::h4("Statistical analysis"),
      shiny::numericInput("max_pairs", "Maximum plotted/analyzed pairs",
                          100000, min = 1000, max = 1000000, step = 1000),
      shiny::numericInput("cluster_k", "Number of clusters to compare (k)", 3,
                          min = 2, max = 100),
      shiny::p(class = "small-help",
        "Cut both dendrograms into k groups and compare membership. Changing k can change every agreement statistic."),
      shiny::numericInput("permutations", "Mantel permutations", 999,
                          min = 99, max = 9999),
      shiny::actionButton("run_mantel", "Run Mantel test"),
      shiny::p(class = "small-help",
        "The Mantel test checks association between matrices by permuting whole isolate labels, not individual cells. It requires complete matrices and at most 400 shared isolates; its p-value is not a test of typing-method equivalence.")
    ),
    shiny::mainPanel(width = 9,
      shiny::div(class = "main-panel",
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
            shiny::verbatimTextOutput("difference_cell")),
          shiny::tabPanel("Pairwise comparison",
            manir_guide(
              "Each dot is one distinct, unordered isolate pair. Its X position is the pair's value in matrix 1 and its Y position is the value for the same pair in matrix 2. Neither the diagonal nor mirrored duplicates are counted.",
              "An upward pattern suggests positive association, while points far from the main pattern merit closer inspection. Different units are allowed here: compare ranks or correlations, not the raw distance from a one-to-one line. Pairwise observations share isolates, so they are not statistically independent."),
            shiny::uiOutput("pairwise_intro"),
            shiny::plotOutput("scatter", height = "520px"),
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
