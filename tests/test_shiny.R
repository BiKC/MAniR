# Minimal server integration test. Requires Shiny and the declared runtime packages.
source("R/matrix_io.R")
source("R/matrix_analysis.R")
source("R/matrix_plot.R")
source("R/example_data.R")
source("R/research_workflows.R")
source("R/research_ui.R")
source("server.R")

ids <- c("001", "002", "003", "004")
m <- matrix(c(1, .7, .4, .3,
              .7, 1, .8, .5,
              .4, .8, 1, .9,
              .3, .5, .9, 1), 4, byrow = TRUE,
            dimnames = list(ids, ids))
path <- tempfile(fileext = ".csv")
write_matrix_csv(m, path)
file_info <- data.frame(name = "example.csv",
                        size = as.numeric(file.info(path)$size),
                        type = "text/csv", datapath = path,
                        stringsAsFactors = FALSE)

shiny::testServer(server, {
  session$setInputs(
    first_file = file_info,
    kind1 = "similarity", kind2 = "similarity",
    cluster = FALSE, linkage = "complete",
    match_mode = "strict", visualize = 1
  )
  loaded_data <- loaded()
  stopifnot(!is.null(loaded_data), nrow(loaded_data$first) == 4L,
            identical(colnames(loaded_data$first), ids),
            is.null(loaded_data$second))
  # A palette change should affect only rendering, not the loaded matrix.
  session$setInputs(palette = "Viridis")
  stopifnot(identical(loaded()$first, loaded_data$first))
  session$setInputs(start_pairs = 1)
  stopifnot(is.null(current_goal()))
  session$setInputs(start_heatmap = 1)
  stopifnot(identical(current_goal()$tab, "Heatmaps"))
  session$setInputs(start_groups = 1)
  stopifnot(identical(current_goal()$tab, "Heatmaps"))
})

shiny::testServer(server, {
  session$setInputs(kind1 = "similarity", kind2 = "similarity",
                    cluster = FALSE, linkage = "complete",
                    match_mode = "strict", load_example = 1)
  demo <- loaded()
  stopifnot(
    !is.null(demo),
    identical(demo$source, "synthetic example"),
    nrow(demo$first) == 12L, nrow(demo$second) == 12L,
    nrow(demo$metadata) == 12L,
    identical(demo$matching, "strict")
  )
  # Even when a checkbox is set, mixed-unit teaching data must not be
  # subtracted or exported as a misleading difference matrix.
  session$setInputs(comparable_scales = TRUE)
  stopifnot(!can_difference())
  session$setInputs(start_agreement = 1)
  stopifnot(identical(current_goal()$tab, "Overview"))
  session$setInputs(start_pairs = 1)
  stopifnot(identical(current_goal()$tab, "Pairwise comparison"))
  session$setInputs(inspect_isolate = "ISO_02", inspect_partner = "ISO_03")
  exact <- selected_pair()
  stopifnot(abs(exact$second - 0.52) < 1e-12,
            is.null(exact$difference))
  session$setInputs(metadata_column = "group", start_groups = 1)
  stopifnot(identical(current_goal()$tab, "Metadata"),
            nrow(group_summary()) == 4L)
  session$setInputs(start_cluster = 1)
  stopifnot(identical(current_goal()$tab, "Cluster comparison"))
  session$setInputs(start_export = 1)
  stopifnot(identical(current_goal()$tab, "Export"))
  session$setInputs(back_to_questions = 1)
  stopifnot(is.null(current_goal()))
  # An ordinary upload after the example must replace it without carrying
  # across the example metadata or second matrix.
  session$setInputs(first_file = file_info, visualize = 1)
  own <- loaded()
  stopifnot(identical(own$source, "uploaded files"),
            nrow(own$first) == 4L, is.null(own$second),
            is.null(own$metadata))
  session$setInputs(second_file = file_info, comparable_scales = TRUE,
                    visualize = 2)
  stopifnot(can_difference(),
            nrow(shared()$first) == 4L)
  session$setInputs(load_example = 2)
  stopifnot(identical(loaded()$source, "synthetic example"),
            nrow(loaded()$second) == 12L, !can_difference(),
            identical(loaded()$linkage, "complete"))
})

cat("Shiny upload and example reactive smoke tests passed.\n")
